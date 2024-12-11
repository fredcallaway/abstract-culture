include("utils.jl")
include("data.jl")

using DataFrames

# %% --------

versions = ["code-pilot-v4", "code-pilot-v5"]
outdir = "data/code-pilot-v45/"
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

participants = load_participants(versions...)
@show nrow(participants)
uids = participants.uid
trials = mapreduce(load_trials, vcat, participants.uid[2:end])

# %% ===== check for problems =================================================

n_failed = sum(uids) do uid
    events = load_events(uid)
    !isempty(filtermatch(events, "experiment.terminate"))
end
if n_failed > 0
    @warn "$n_failed participants failed the experiment"
end

# %% --------

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

df = map(trials) do t
    # if ismissing(solution(t))
    #     return missing
    # end
    info = InformationState(t)
    (;
        version = t.version,
        uid = t.uid,
        trial_number = t.trial_number,
        duration = duration(t),
        n_try = n_try(t),
        # n_button = n_button(t),
        n_button_bespoke = sum(get(e, "action", "") == "nextCode.bespoke" for e in t.events),
        n_button_compositional = sum(get(e, "action", "") in ("nextCode.left", "nextCode.right") for e in t.events),
        solution_type = solution_type(t),
        used_manual = used_manual(t, info),
        knowledge(info)...
    )
end |> skipmissing |> collect |> DataFrame

df |> CSV.write(outdir * "trials.csv")

# %% --------

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

@chain instruct_times begin
    @groupby :stage
    @combine begin
        :time = mean(:time)
        :time_sd = std(:time)
        :time_q10 = quantile(:time, 0.1)
        :time_q90 = quantile(:time, 0.9)
    end
end

# %% ==================== feedback ====================

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

for x in feedback.special2
    println("- ", x)
end
println("------------ feedback ------------")
for x in feedback.feedback
    if x != ""
        println("- ", x)
    end
end

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
times