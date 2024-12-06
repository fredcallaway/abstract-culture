include("utils.jl")
include("data.jl")

using DataFrames, RCall

# %% --------

versions = ["code-pilot-v1", "code-pilot-v2", "code-pilot-v3"]

FIGS_PATH = "figs/codes/code-pilot-"
@rput FIGS_PATH

R"""
suppressPackageStartupMessages(source("base.r"))
cpal = scale_colour_manual(values=c(
    individual="#73AB84",
    social="#F5760A"
), aesthetics=c("fill", "colour"), name="")

"""

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
@rput participants
uids = participants.uid

# %% --------

pframe() do uid
    events = load_events(uid)
    (;many = findnextmatch(events, 1, "experiment.tooManyTries"))
end

# %% --------

struct InformationState4
    manual::Vector{@NamedTuple{task::Tuple{Int64,Int64}, compositional::Bool, code::String}}
    task::Tuple{Int64,Int64}
end

parse_task(task) = @chain task collect parse.(Int, _) Tuple

function parse_manual(manual::Vector)
    map(manual) do m
        (; task=parse_task(m["task"]), compositional=m["compositional"], code=m["code"])
    end
end

bespoke_knowledge(s::InformationState4) = any(x->x.task == s.task && !x.compositional, s.manual)

function compositional_knowledge(s::InformationState4)
    exact = any(x->x.task == s.task && x.compositional, s.manual)
    left = any(x->x.task[1] == s.task[1] && x.compositional, s.manual)
    right = any(x->x.task[2] == s.task[2] && x.compositional, s.manual)
    exact ? :exact : left && right ? :full : left || right ? :partial : :none
end

knowledge(s::InformationState4) = (; bespoke=bespoke_knowledge(s), compositional=compositional_knowledge(s))

function InformationState4(t::Trial)
    init = findnextmatch(t.events, "machine.initialize")[1]
    manual = parse_manual(init["manual"])
    task = @chain init["task"] collect parse.(Int, _) Tuple
    InformationState4(manual, task)
end

trials = mapreduce(load_trials, vcat, participants.uid)


solution(t::Trial) = @catch_missing filtermatch(t.events, "machine.enter.correct")[1]["code"]
n_try(t::Trial) = length(filtermatch(t.events, "machine.enter"))
n_green(t::Trial) = sum(get(e, "action", "") == "nextCode" for e in t.events)
used_locks(t::Trial) = length(filtermatch(t.events, "machine.locks.lock"))
solution_type(t::Trial) = findnextmatch(t.events, "machine.solved")[1]["solutionType"]
function used_manual(t::Trial, info::InformationState4)
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

info.manual

df = map(trials) do t
    if ismissing(solution(t))
        return missing
    end
    info = InformationState4(t)
    (;
        version = t.version,
        uid = t.uid,
        trial_number = t.trial_number,
        duration = duration(t),
        n_try = n_try(t),
        n_green = n_green(t),
        used_locks = used_locks(t),
        solution_type = solution_type(t),
        used_manual = used_manual(t, info),
        knowledge(info)...
    )
end |> skipmissing |> collect |> DataFrame
df.compositional = string.(df.compositional)
@rput df
df |> CSV.write("tmp/code-pilot.csv")

# %% --------

events = load_events(participants.uid[5])
i1, i2, i3, i4, i5 = map(1:5) do i
    findnextmatch(events, "instructions.runStage.$i")[2]
end

start3 = findnextmatch(events, "instructions.runStage.3")[2]
filtermatch(events[i2:i3], "machine.enter")

# %% --------
stop3 = findnextmatch(events, "instructions.runStage.4")[2]
filtermatch(events[i3:i4], "")

# %% --------



stages = map(filtermatch(load_events(participants.uid[1]), "instructions.runStage")) do e
    e["event"]
end

load_events(df.uid[1])[1:50]

instruct_times = pframe() do uid
    start_times = map(stages) do stage
        findnextmatch(load_events(uid), stage)[1]["time"]
    end
    map(enumerate(diff(start_times))) do (i, t)
        (; i, time=t / 1000)
    end
end

@chain instruct_times begin
    @groupby :i
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
    println(x)
end
for x in feedback.feedback
    println(x)
end

@rput feedback

R"""
feedback %>%
    filter(special == "yes") %>%
    select(pid, special2) %>%
    tibble() %>%
    kable()
"""


# when bespoke=true, proportion of manual solution vs some other solution

# %% --------



trials = mapreduce(load_trials, vcat, uids)

using Dictionaries

function lookup(uid::String, key)
    rows = eachrow(participants)
    i = findfirst(x -> x.uid == uid, rows)
    get(rows[i], key)
end
lookup(t::Trial, key) = lookup(t.uid, key)

# %% --------


function known_path_length(knowledge::Vector{<:Edge}, start, goal)
    g = DiGraph(N_STATE)
    for e in knowledge
        add_edge!(g, e)
    end
    known_path_length = length(a_star(g, start, goal))
end
known_path_length(t::Trial; knowledge=t.knowledge) = known_path_length(knowledge, t.start, t.goal)

tdf = pframe() do uid
    map(load_trials(uid)) do t
        discovered = setdiff(t.traversed, t.knowledge)
        (; t.trial_number, t.start, t.goal, duration=(t.end_time - t.start_time) / 1000,
            path_length=length(t.path), known_path_length=known_path_length(t),
            comp_start_known=Edge(t.start, 5) in t.knowledge,
            comp_goal_known=Edge(5, t.goal) in t.knowledge,
            n_pull=length(t.attempts), n_discovered=length(discovered), n_known=length(t.knowledge), version=uid[3:3]
        )
    end
end
@rput tdf

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
@rput times


# %% ==================== evolution of compositionality ====================

sim = run_sims(30, 10, S=4, N=12, K=7, M=[5, 20, 50], ε=0.14)

serialize("tmp/experiment", (; sim, tdf))
@rput sim

R"""
human = tdf %>%
    filter(start != 5, goal != 5) %>%
    group_by(M, population, generation) %>%
    summarise(compositionality = mean(path_length == 2)) %>%
    mutate(agent = "human")

# df = bind_rows(mutate(sim, agent="model", population=100+population), human)

plt = sim %>%
    ggplot(aes(generation, 1*compositionality, group=population)) +
    geom_line(linewidth=.5, color="#BA1109", alpha=0.3) +
    # geom_line(linewidth=.5, color="#BA1109", alpha=0.5) +
    facet_wrap(~M, labeller=label_glue("{M} Demonstrations")) +
    xbreaks()

fig("evolution_predicitions", w=6, pdf=T)

plt + geom_line(linewidth=1, data=human)
fig("evolution", w=6, pdf=T)

"""

# %% --------

R"""
sim %>%
    filter(population < 11) %>%
    fctrize(M) %>%
    regress(compositionality ~ generation * M)

"""

# %% ==================== ε ====================

R"""
tdf %>%
    filter(known_path_length == 2) %>%
    group_by(pid) %>%
    filter(n() > 1) %>%
    summarise(compositionality=mean(path_length == 2)) %>%
    ggplot(aes(pid, compositionality)) +
    geom_point()

fig()
"""


# %% ==================== effort ====================

R"""
tdf %>%
    ggplot(aes(generation, duration, group=population)) +
    point_line() +
    facet_wrap(~M)
fig()
"""

R"""
tdf %>%
    ggplot(aes(known_path_length, duration)) +
    stat_summary(fun="median") +
    expand_limits(y=0) +
tdf %>%
    group_by(known_path_length) %>%
    drop_extreme(duration) %>%
    ggplot(aes(known_path_length, duration)) +
    points() +
    expand_limits(y=0)

fig(w=5)
"""

R"""
tdf %>%
    ggplot(aes(generation, 1*n_pull, group=population)) +
    point_line() +
    facet_wrap(~M)
fig()
"""

R"""
times %>%
    select(population, generation, instructions, main) %>%
    pivot_longer(c(instructions, main), names_to="block", values_to="time") %>%
    group_by(population, generation, block) %>%
    summarise(time = median(time)) %>%
    ggplot(aes(generation, time, color=block)) +
    geom_line() +
    facet_wrap(~population) +
    scale_x_continuous(n.break=2)

fig(w=5)
"""

R"""
tdf %>%
    ggplot(aes(generation, 1*n_pull, group=population)) +
    point_line()
fig()
"""

R"""
tdf %>%
    group_by(known_path_length) %>%
    drop_extreme(duration, n_pull) %>%
    ggplot(aes(n_pull, duration, color=factor(known_path_length))) +
    points(size=.1) +
    linear_fit()

fig()

"""

R"""
tdf %>%
    group_by(known_path_length) %>%
    drop_extreme(duration, n_pull) %>%
    mutate(P = factor(known_path_length)) %>%
    regress(duration ~ n_pull * P) %>%
    coeftable(intercept=T)
"""

R"""
tdf %>%
    group_by(known_path_length) %>%
    drop_extreme(duration, n_pull) %>%
    filter(known_path_length == 2) %>%
    regress(duration ~ n_pull) %>%
    coeftable(intercept=T)
"""

# %% ==================== information ====================

df = pframe() do uid
    t = load_trials(uid)[1]
    map(t.knowledge) do e
        (; e.src, e.dst)
    end
end
@rput df
R"""
df %>%
    filter(pid < 11) %>%
    fctrize(src, levels=seq(1,5)) %>%
    fctrize(dst, levels=seq(5,9)) %>%
    group_by(population, generation) %>%
    count(src, dst, .drop=F) %>%
    mutate(prop = n / 10) %>%
    ggplot(aes(src, dst, fill=prop)) +
    geom_tile() +
    facet_grid(population~generation)

fig(w=10)
"""


# %% ==================== learning ====================

R"""
plot_trials = function(data, yvar, ...) {
    data %>% ggplot(aes(trial_number, 1*{{yvar}}, ...)) +
    point_line() +
    facet_grid(population~generation)
}
"""

# %% --------

R"""
tdf %>%
    plot_trials(n_pull) +
    geom_hline(yintercept=2) + expand_limits(y=0)

fig(w=10, h=4)
"""


# %% ==================== feedback ====================

function ensure_keys!(d, keys)
    for k in keys
        d[k] = get(d, k, missing)
    end
    d
end

feedback = pframe() do uid
    events = load_events(uid)
    x = findnextmatch(events, 1, "debrief.submitted")[1]
    (difficulty=x["difficulty"], feedback=x["feedback"])
    # ensure_keys!(d, ["whynot", "feedback"])
end

foreach(println, feedback.feedback)

