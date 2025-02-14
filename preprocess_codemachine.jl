# %% --------

include("utils.jl")
include("data.jl")

using DataFrames
using TOML

empty!(ARGS)
push!(ARGS, "code-pilot-v11")

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

participants = load_participants(version; keep_failed=false)
@show nrow(participants)

uid2pid = Dict(participants.uid .=> participants.pid)

trials = mapreduce(load_trials, vcat, participants.uid)
participants |> CSV.write(outdir * "participants.csv")    

# %% --------

tt = filter(trials) do t
    uid2pid[t.uid] == 6
end

tt[2].events
# %% ===== trials.csv =========================================================


struct InformationState
    manual::Vector{@NamedTuple{task::Tuple{Int64,Int64}, compositional::Bool, code::String}}
    task::Tuple{Int64,Int64}
end

parse_task(task) = @chain task collect parse.(Int, _) Tuple

function parse_manual(manual::Vector)
    # tmp
    map(manual) do m
        if m["task"] == "null"
            @warn "null task"
            return missing
        end
        (; task=parse_task(m["task"]), compositional=m["compositional"], code=m["code"])
    end |> skipmissing |> collect
end

bespoke_knowledge(s::InformationState) = any(x->x.task == s.task && !x.compositional, s.manual)

function compositional_knowledge(s::InformationState)
    exact = any(x->x.task == s.task && x.compositional, s.manual)
    left = any(x->x.task[1] == s.task[1] && x.compositional, s.manual)
    right = any(x->x.task[2] == s.task[2] && x.compositional, s.manual)
    exact ? :exact : left && right ? :full : left || right ? :partial : :none
end

knowledge(s::InformationState) = (; bespoke=bespoke_knowledge(s), compositional=compositional_knowledge(s))

function InformationState(t::Trial)
    init = findnextmatch(t.events, "machine.initialize")[1]
    manual = parse_manual(init["manual"])
    task = @chain init["task"] collect parse.(Int, _) Tuple
    InformationState(manual, task)
end

n_try(t::Trial) = length(filtermatch(t.events, "machine.enter"))
n_button(t::Trial) = sum(startswith(get(e, "action", ""), "nextCode") for e in t.events)
solution_type(t::Trial) = filtermatch(t.events, "machine.solution")[end]["event"] |> x -> split(x, ".")[end]

function solution(t::Trial)
    code = filtermatch(t.events, "machine.enter")[end]["code"]
    @assert code in keys(t.events[1]["solutions"])
    if code == "null"
        return nothing
    end
    code
end

function used_manual(t::Trial, info::InformationState)
    code = solution(t)
    if solution_type(t) == "compositional"
        any(info.manual) do m
            m.compositional && (m.code[1:2] == code[1:2] || m.code[3:4] == code[3:4])
        end
    else
        solution(t) in getindex.(info.manual, :code)
    end
end

duration(t::Trial) = (t.events[end]["time"] - t.events[1]["time"]) / 1000


function maybe_time(events, e)
    e = findnextmatch(events, e)[1]
    isnothing(e) ? missing : e["time"]
end

function response_times(t::Trial)
    events = t.events
    start = maybe_time(events, "machine.run")
    rt_select = maybe_time(events, "machine.enter")
    rt_left = maybe_time(events, "machine.button.left")
    rt_right = maybe_time(events, "machine.button.right")
    rt_comp = safe_minimum(skipmissing([rt_left, rt_right]); default=missing)
    rt_bespoke = maybe_time(events, "machine.button.bespoke")
    map(x -> (x - start) / 1000, (;rt_select, rt_comp, rt_bespoke))
end

df = map(trials) do t
    info = InformationState(t)
    (;
        version = t.version,
        pid = uid2pid[t.uid],
        trial_number = t.trial_number,
        duration = duration(t),
        n_try = n_try(t),
        n_button_bespoke = sum(get(e, "action", "") == "nextCode.bespoke" for e in t.events),
        n_button_compositional = sum(get(e, "action", "") in ("nextCode.left", "nextCode.right") for e in t.events),
        solution_type = solution_type(t),
        used_manual = used_manual(t, info),
        knowledge(info)...,
        response_times(t)...,
    )
end |> skipmissing |> collect |> DataFrame

df |> CSV.write(outdir * "trials.csv")


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

# %% ===== button_times.csv =================================================

button_times = pframe() do uid
    map(load_trials(uid)) do t
        map(filtermatch(t.events, "machine.button")) do e
            (; time=e["time"] / 1000, 
            kind=endswith(e["event"], "bespoke") ? :bespoke : :compositional)
        end
    end
end

button_times |> CSV.write(outdir * "button_times.csv")

# %% --------


# %% ===== events.csv =========================================================

participants[6, :]

function solutions(t::Trial)
    x = invert(t.events[1]["solutions"])
    (;bespoke=x["bespoke"], compositional=x["compositional"])
end

pframe() do uid
    map(load_trials(uid)) do t
        sol = solutions(t)
        start = findnextmatch(t.events, "machine.run")[1]["time"]
        map(t.events[3:end]) do e
            if e["event"] == "machine.enter"
                if startswith(e["action"], "select")
                    event = :dial
                    pos = parse(Int, e["action"][end]) + 1
                    c = e["code"][pos] == sol.compositional[pos]
                    b = e["code"][pos] == sol.bespoke[pos]
                    kind = 
                        c && b ? :ambiguous :
                        c ? :compositional :
                        b ? :bespoke :
                        :wrong
                else
                    event = :button
                    kind = endswith(e["action"], "bespoke") ? :bespoke : :compositional
                end
            elseif startswith(e["event"], "machine.solution")
                event = "solution"
                kind = endswith(e["event"], "bespoke") ? :bespoke : :compositional
            else
                return missing
            end
            (; time = (e["time"] - start) / 1000, trial_number=t.trial_number, event, kind)
        end
    end
end |> CSV.write(outdir * "events.csv")

# %% --------

pframe() do uid
    events = load_events(uid)
    map(filtermatch(events, "instructions.showHelp")) do e
        (; stage=e["stage"], text=e["text"])
    end
end

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