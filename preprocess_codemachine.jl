# %% --------

include("utils.jl")
include("data.jl")

using DataFrames

# empty!(ARGS)
# push!(ARGS, "code-pilot-v21")

if length(ARGS) == 0
    # Pull from experiment config
    config = read("../machine-task/config.txt", String)
    version = match(r"experiment_code_version = (.*)", config).captures[1]
else
    version = ARGS[1]
end

println("Processing version: $version")
outdir = "data/$(version)/"
mkpath(outdir)

function ffmap(f, args)
    results = skipmissing(map(f, args))
    while eltype(results) <: AbstractVector
        isempty(results) && return []
        results = skipmissing(reduce(vcat, results))
    end
    collect(results)
end

function pframe(f, data=participants)
    @chain data begin
        @groupby :pid
        @combine $AsTable = ffmap(f, :uid)
    end
end

participants = load_participants(version)
@show nrow(participants)

# %% --------

valid_pids = @chain participants begin
    @rsubset(! :failed_catch)
    @distinct :config
    @groupby :repetition
    @transform :agent = 1:length(:pid)
    @rsubset :agent <= 50
    @with :pid
end

@assert issorted(valid_pids)
@assert issorted(participants.start_time)

@rtransform! participants :excluded = !(:pid in valid_pids)

# %% --------

uid2pid = Dict(participants.uid .=> participants.pid)
pid2uid = Dict(participants.pid .=> participants.uid)

trials = mapreduce(load_trials, vcat, participants.uid)
participants |> CSV.write(outdir * "participants.csv")
@assert participants.pid == 1:nrow(participants)

# %% ===== trials.csv =========================================================

function parse_knowledge(task, manual)
    man_tasks = [e["task"] for e in manual]
    exact = any(mt == task for mt in man_tasks)
    left = any(mt[1] == task[1] for mt in man_tasks)
    right = any(mt[2] == task[2] for mt in man_tasks)
    exact ? :exact : left && right ? :full : left || right ? :partial : :zilch
end

struct Machine
    task::String
    knowledge::Symbol
    n_dial::Int
    kind::Symbol
end

machine_type(m::Machine) = string(
    uppercase(string(m.kind)[1]),
    m.n_dial,
    "-", m.knowledge
)

function effort(machine; action_cost=1, search_cost=3)
    search_multiplier = Dict(
        :exact => 0,
        :full => 0,
        :partial => 0.5,
        :zilch => 1,
    )[machine.knowledge]
    
    machine.n_dial * (action_cost + search_multiplier * search_cost)
end

function parse_machines(t::Trial)
    init = findnextmatch(t.events, "machine.initialize")[1]
    map(init["machines"]) do mach
        task = init["task"]
        knowledge = parse_knowledge(task, mach["manual"])
        n_dial = length(mach["code"])
        kind = Symbol(mach["kind"])
        Machine(task, knowledge, n_dial, kind)
    end
end


# "main.reg-v1-1-4B-D8-g1-32.f6z2" -> "reg-v1-1-4B-D8"
function chain_id(id)
    m = match(r"main\.(reg-v\d[-]\d[-]\d\w-D\d+)", id)
    return m !== nothing ? m.captures[1] : missing
end


df = map(trials) do t
    machines = parse_machines(t)

    eff = effort.(machines)
    ranks = sortperm(eff)

    sol = try
        filtermatch(t.events, "machine.solution")[end]
    catch
        @warn "no solution for $t"
        return missing
    end
    choice = sol["position"] + 1
    # trial_type = join(map(machine_type, machines[ranks]), " vs ")
    trial_type = join(sort(map(machine_type, machines)), " vs ")
    n_click = length(filtermatch(t.events, "machine.select"))

    (;
        t.uid,
        pid = uid2pid[t.uid],
        t.trial_number, 
        trial_type,
        task = machines[choice].task,
        kind = sol["kind"],
        
        chain_id = chain_id(trial_id(t)),
        trial_id = trial_id(t),
        is_catch = is_catch(t),
        is_practice = is_practice(t),
        choose_left = choice == 1,
        choose_best = choice == ranks[1],
        choose_compositional = sol["kind"] == "compositional",

        effort_difference = length(machines) > 1 ? eff[2] - eff[1] : missing,
        duration = duration(t),
        n_click,
        mach1 = machine_type(machines[1]),
        mach2 = length(machines) > 1 ? machine_type(machines[2]) : missing,
    )
end |> skipmissing |> DataFrame
df |> CSV.write(outdir * "trials.csv")

# %% ===== generation solutions json for stimuli generation ===================


solutions = @chain df begin
    @rsubset :pid in valid_pids
    @rsubset !:is_practice !:is_catch
    @rtransform :chain_id = chain_id(:trial_id)
    @rtransform :solution = (;task=:task, kind=:kind)
    @groupby :chain_id
    @combine :solutions = [:solution]
    @with Dict(:chain_id .=> :solutions)
end


@assert all(map(length, values(solutions)) .== 50)

gen_code = match(r"-(g\d+)", version).captures[1]
fp = "../machine-task/stimuli/solutions-$gen_code.json"
json(solutions) |> write(fp)
println("Wrote $fp")

# %% --------

chosen(t::Trial) = filtermatch(t.events, "machine.solution")[end]["position"] + 1

df = pframe() do uid
    trials = load_trials(uid)
    n_guess = ffmap(trials) do t
        @require t.trial_number == 0 || is_catch(t)
        machines = parse_machines(t)
        choice = chosen(t)
        mach = machines[choice]

        @require mach.knowledge == :zilch
        # n_guess = zeros(Int, mach.n_dial)
        guessed = [Set{Int}() for _ in 1:mach.n_dial]
        for e in filtermatch(t.events, "machine.select")
            if e["machineIdx"]+1 == choice
                pos = e["pos"] + 1
                if e["kind"] == "right"
                    pos += (mach.n_dial ÷ 2)
                end

                v = parse(Int,e["val"])
                if v != 0
                    push!(guessed[pos], v)
                end
            end
        end
        map(guessed) do g
            (;t.trial_number, n=length(g))
        end
    end
end
df |> CSV.write(outdir * "n_guess.csv")

# %% --------

pframe() do uid
    events = load_events(uid)
    x = findnextmatch(events, 1, "instructions.bespoke_efficiency.choice")[1]
    (;choice=x["choice"], time=x["time"])
end |> CSV.write(outdir * "bespoke_efficiency.csv")

# %% --------

pframe() do uid
    map(filtermatch(load_events(uid), "browser.focus")) do e
        delete(NamedTuple(e), :event)
    end
end |> CSV.write(outdir * "browser_focus.csv")


# %% ===== instruct_times.csv =================================================

stages = map(filtermatch(load_events(participants.uid[1]), "instructions.runStage")) do e
    e["event"]
end

instruct_times = pframe() do uid
    start_times = map(stages) do stage
        findnextmatch(load_events(uid), stage)[1]["time"]
    end
    map(enumerate(diff(start_times))) do (stage, t)
        (; stage, time=t / 1000)
    end
end

instruct_times |> CSV.write(outdir * "instruct_times.csv")

x = @chain instruct_times begin
    @groupby :stage
    @combine begin
        :time = mean(:time)
        :time_sd = std(:time)
        :time_q10 = quantile(:time, 0.1)
        :time_q90 = quantile(:time, 0.9)
    end
end


# %% ===== events.csv =========================================================

pframe() do uid
    map(load_trials(uid)) do t
        start = findnextmatch(t.events, "machine.initialize")[1]["time"]
        map(eachindex(t.events)) do i
            e = t.events[i]
            if e["event"] == "machine.select"
                (; t.uid, trial_number=t.trial_number, time = (e["time"] - start) / 1000, 
                dial=e["kind"], correct=e["correct"], pos=e["pos"], val=e["val"])
            else
                missing
            end
        end
    end
end |> CSV.write(outdir * "events.csv")

# %% --------

pframe() do uid
    events = load_events(uid)
    ee = map(filtermatch(events, "instructions.showHelp")) do e
        (; stage=e["stage"], text=e["text"])
    end
    filter(ee) do e
        e.stage != "instruct.intro"
    end
end |> CSV.write(outdir * "help.csv")

# %% ===== feedback.csv ========================================================

function ensure_keys!(d, keys)
    for k in keys
        d[k] = get(d, k, missing)
    end
    d
end

function Base.NamedTuple(d::Dict{String})
    NamedTuple(Dict(Symbol(k) => v for (k, v) in d))
end

feedback = pframe() do uid
    events = load_events(uid)
    x = findnextmatch(events, 1, "debrief.submitted")[1]
    NamedTuple(x)
    # (difficulty=x["difficulty"], feedback=x["feedback"])
    # ensure_keys!(d, ["whynot", "feedback"])
end
feedback |> CSV.write(outdir * "feedback.csv")

# %% --------

pframe() do uid
    events = load_events(uid)
    x = findnextmatch(events, 1, "instructionsSurvey.submitted")[1]
    isnothing(x) && return missing
    NamedTuple(x)
    # (difficulty=x["difficulty"], feedback=x["feedback"])
    # ensure_keys!(d, ["whynot", "feedback"])
end |> CSV.write(outdir * "instructions-survey.csv")

# %% --------


times = pframe() do uid
    events = load_events(uid)
    starts = filter(ematch("timeline.start"), events)

    ustarts = unique(e -> e["event"], starts)
    if length(ustarts) < length(starts)
        @warn "repeated events for $uid"
        starts = ustarts
    end

    ends = filter(ematch("timeline.end"), events)
    d = map(starts, ends) do s, e
        block = rsplit(e["event"], "."; limit=2)[end]

        @assert (block == rsplit(s["event"], "."; limit=2)[end])
        block => (e["time"] - s["time"]) / 60000
    end |> Dict{String,Any}
    d["total"] = sum(values(d))
    # d["workerid"] = idents[split(uid, "-")[end]]
    d
end
times |> CSV.write(outdir * "times.csv")

# %% --------

events = load_events("reg-v2-g3-67600da0497fab54effd903e")
starts = filter(ematch("timeline.start"), events)

ustarts = unique(e -> e["event"], starts)

# %% ===== mousetracking ======================================================

pframe() do uid
    events = load_events(uid)
    imap(filtermatch(events, "mouselog")) do i, mlog
        map(mlog["events"]) do e
            (;trial_number=i, x=e["x"], y=e["y"], time=e["time"])
        end
    end
end |> CSV.write(outdir * "mousetracking.csv")


