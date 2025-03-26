# %% --------

include("utils.jl")
include("data.jl")

using DataFrames
using TOML

empty!(ARGS)
push!(ARGS, "code-pilot-v17")

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
names(participants)
@show nrow(participants)

uid2pid = Dict(participants.uid .=> participants.pid)

trials = mapreduce(load_trials, vcat, participants.uid)
participants |> CSV.write(outdir * "participants.csv")    

# %% --------


task(t::Trial) = @chain t.events[1]["task"] collect parse.(Int, _) Tuple
solution(t::Trial, type) = t.events[1]["solutions"][string(type)]

function check_reuse(uid; baseline=false)
    tt = load_trials(uid)
    flatmap(enumerate(tt)) do (trial_number, t)
        map(filtermatch(t.events, "machine.select")) do e
            # check if the entered number was a solution for a past trial
            # for bespoke, task must match exactly
            # for left/right, task[1] or task[2] must match
            current_task = task(t)
            entered_number = baseline ? string(rand(1:9))[1] : e["val"][1]
            is_previous_solution = any(1:trial_number-1) do prev_num
                prev_t = tt[prev_num]
                prev_task = task(prev_t)
                prev_solution = solution(prev_t, e["kind"])
                prev_solution_used = solution_type(prev_t)

                used_relevant_solution = (prev_solution_used == "bespoke") == (e["kind"] == "bespoke")
                used_relevant_solution || return false

                is_match = (
                    (e["kind"] == "bespoke" && prev_task == current_task) ||
                    (e["kind"] == "left" && prev_task[1] == current_task[1]) ||
                    (e["kind"] == "right" && prev_task[2] == current_task[2])
                )

                is_match && prev_solution[one_index(e["pos"])] == entered_number
            end
            
            is_previous_solution
        end
    end |> sum
end

uid = participants.uid[1]
res = map(participants.uid) do uid
    baseline = repeatedly(100) do
        check_reuse(uid; baseline=true)
    end
    mean(check_reuse(uid) .> baseline)
end




# %% --------




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
        (; task=parse_task(m["task"]), compositional=m["kind"] == "compositional", code=m["code"])
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

n_try(t::Trial) = length(filtermatch(t.events, "machine.select"))
solution_type(t::Trial) = filtermatch(t.events, "machine.solution")[end]["kind"]

function used_manual(t::Trial, info::InformationState)
    return missing
end

duration(t::Trial) = (t.events[end]["time"] - t.events[1]["time"]) / 1000

function maybe_time(events, e)
    e = findnextmatch(events, e)[1]
    isnothing(e) ? missing : e["time"]
end

function response_times(t::Trial)
    events = t.events
    start = maybe_time(events, "machine.run")
    rt_select = maybe_time(events, "machine.select")
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
        # n_button_bespoke = sum(get(e, "action", "") == "nextCode.bespoke" for e in t.events),
        # n_button_compositional = sum(get(e, "action", "") in ("nextCode.left", "nextCode.right") for e in t.events),
        solution_type = solution_type(t),
        # used_manual = used_manual(t, info),
        knowledge(info)...,
        # response_times(t)...,
    )
end |> skipmissing |> collect |> DataFrame

df |> CSV.write(outdir * "trials.csv")

# %% --------
# include("red_black.jl")
# env = RedBlackEnv(;S=5, N=30, K=1, M=[5, 20, 50], )
# sim = simulate(env, 10)

function print_trial(t::Trial)
    start = t.events[1]["time"]
    foreach(t.events[3:end]) do e
        println((e["time"] - start), " ", e["event"], " ", get(e, "kind", ""))
    end
end

# t = filter(trials) do t
#     solution_type(t) == "left"
# end |> first
# print_trial(trials[11])

# %% --------

pframe() do uid
    map(filtermatch(load_events(uid), "browser.focus")) do e
        delete(NamedTuple(e), :event)
    end
end |> CSV.write(outdir * "browser_focus.csv")

# %% --------
# # You can always determine the exact code to make a shape using the manuals.
# # You need to use both machines to complete every round.
# # How many digits are in the blue machine's codes?
# # How many digits are in the orange machine's codes?
# questions = [
#     "full_info",
#     "use_both",
#     "blue_digits",
#     "orange_digits",
# ]

# pframe() do uid
#     e = findnextmatch(load_events(uid), "quiz.check")[1]
#     map(questions, e["answers"], e["correct"]) do question, answer, correct
#         (; question, answer, correct)
#     end
# end |> CSV.write(outdir * "quiz.csv")

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
        start = findnextmatch(t.events, "machine.run")[1]["time"]
        map(eachindex(t.events)) do i
            e = t.events[i]
            if e["event"] == "machine.select"
                (; trial_number=t.trial_number, time = (e["time"] - start) / 1000, 
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