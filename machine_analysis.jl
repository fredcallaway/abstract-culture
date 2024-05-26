include("utils.jl")
include("data.jl")
include("figure.jl")
include("graph_plots.jl")
include("graph.jl")

using DataFrames, RCall

# %% --------

version = "vM7"

FIGS_PATH = "figs/machine/$version-"
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


participants = load_participants("vM7"; all_generations=true)

if version in ("vM5", "vM7")
    participants = @chain participants begin
        @rtransform! begin
            :M = :generation == 1 ? missing : :M
        end
        flatmap(eachrow(_)) do row
            if ismissing(row.M)
                map([5,20,50]) do M
                    (;row..., M, population = "M$M-1")
                end
            else
                [row]
            end
        end |> DataFrame
    end
end


@rput participants

uids = participants.uid
trials = mapreduce(load_trials, vcat, uids)

function pframe(f, data=participants)
    @chain data begin
        @groupby :population :generation :pid :M
        @combine $AsTable = ffmap(f, :uid)
    end
end

using Dictionaries

function lookup(uid::String, key)
    rows = eachrow(participants)
    i = findfirst(x -> x.uid == uid, rows)
    get(rows[i], key)
end
lookup(t::Trial, key) = lookup(t.uid, key)


# gtrials = Dictionaries.sortkeys(map(g->collect(group(get(:uid), g)), group(generation, trials)))

tdf = pframe() do uid
    n_state = get_params(uid)["nChemical"]
    map(load_trials(uid)) do t
        discovered = setdiff(t.traversed, t.knowledge)
        g = DiGraph(n_state)
        for e in t.knowledge
            add_edge!(g, e)
        end
        known_path_length = length(a_star(g, t.start, t.goal))

        (;t.trial_number, t.start, t.goal, duration=(t.end_time - t.start_time) / 1000,
            path_length = length(t.path), known_path_length,
            comp_start_known = Edge(t.start, 5) in t.knowledge,
            comp_goal_known = Edge(5, t.goal) in t.knowledge,
            n_pull=length(t.attempts), n_discovered=length(discovered), n_known=length(t.knowledge),

            version = uid[3:3]
        )
    end
end
@rput tdf


times = pframe() do uid
    events = load_events(uid)
    starts = filter(ematch("timeline.start"), events)

    ustarts = unique(e->e["event"], starts)
    if length(ustarts) < length(starts)
        @warn "repeated events for $uid"
        starts = ustarts
    end

    ends = filter(ematch("timeline.end"), events)
    d = map(starts, ends) do s, e
        block = rsplit(e["event"], "."; limit=2)[end]

        @assert (block == rsplit(s["event"], "."; limit=2)[end])
        block => (e["time"] - s["time"]) / 60000
    end |> Dict{String, Any}
    # d["workerid"] = idents[split(uid, "-")[end]]
    d
end
@rput times

# %% ==================== debug ====================
repeats = ("65ab052d1e112ab8611d762d", "664ab329e0b7a59dc3afcf69", "6650da43a82f01840447bcbe", "6650f3da72e0dcc860312e62")

ffmap(trials) do t
    @require t.trial_number == 1
    @require lookup(t, :workerid) in repeats
    (M = lookup(t, :M), n_known = length(t.knowledge))
end

# %% --------

i = findfirst(trials) do t
    t.trial_number == 1 && lookup(t, :workerid) == "664ed5f3fdd2443855330873"
end

trials[i].knowledge

R"""
X = tdf %>%
    filter(M==5, generation==5) %>%
    filter(start != 5, goal != 5) %>%
    group_by(pid, known_path_length) %>%
    summarise(
        n_case = n(),
        n_two_step = sum(path_length == 2)
    ) %>%
    pivot_longer(c(n_case, n_two_step), names_to="name", values_to="value") %>%
    fctrize(known_path_length, levels=c(0,1,2)) %>%
    ggplot(aes(known_path_length, value, fill=name)) +
    geom_bar(stat="identity", alpha=1, position="identity") +
    scale_colour_manual(values=c(
        n_case = "darkgray",
        n_two_step = "red"
    ), aesthetics=c("fill", "colour"), name="") +
    facet_wrap(~pid) +
    ggtitle("M = 5") +
    ylab("Count")
fig(w=7, h=4)
"""

R"""
participants %>%
    filter(workerid %in% repeats) %>%
    select(population, generation)

"""




R"""
X %>%
    left_join(select(participants, pid, workerid)) %>%
    filter(workerid %in% repeats)
"""

R"""
participants %>%
    filter(workerid %in% repeats) %>%
    select(pid)

"""

R"""
tdf %>%
    filter(start != 5, goal != 5) %>%
    group_by(M, population, generation) %>%


# two-step solutions are common even when they aren't known
tdf %>%
    filter(M==5, generation==5) %>%
    filter(start != 5, goal != 5) %>%
    filter(known_path_length != 1) %>%
    group_by(comp_start_known, comp_goal_known) %>%
    summarise(
        n = n(),
        two_step = mean(path_length == 2)
    )

# 136 and 140 are using two-step even when both are not known
# 136 and 139 (also 135) have high compositionality rates
# conditions: 92 89 30
tdf %>%
    left_join(select(participants, pid, condition)) %>%
    filter(M==20, generation==5) %>%
    filter(start != 5, goal != 5) %>%
    # filter(!comp_start_known, !comp_goal_known) %>%
    group_by(pid, condition) %>%
    summarise(
        n = n(),
        two_step = mean(path_length == 2)
    )

participants %>% filter(pid == 121) %>% select(generation)
"""

t = filter(trials) do t
    lookup(t, :pid) == 121
end |> first
lookup(t, :generation)
t.knowledge
t.start, t.goal
lookup(t, :condition)
participants[1:3, :]
get_params(t.uid)["tasks"]

# generation 4 configs line up

R"""
tdf %>%
    filter(pid == 140) %>%
    select(comp_start_known, comp_goal_known, path_length, duration, n_pull)



"""


# %% --------

R"""
repeats = c("65ab052d1e112ab8611d762d", "664ab329e0b7a59dc3afcf69", "6650da43a82f01840447bcbe", "6650f3da72e0dcc860312e62")
participants %>%
    filter(workerid %in% repeats) %>%
    select(M, wid)

"""
R"""
participants %>%
    filter(generation > 1) %>%
    group_by(workerid) %>%
    filter(n() > 1)  %>%
    select(workerid, pop_name, generation) %>%
    arrange(workerid)

participants %>%
    filter(generation == 4, pop_name == "M5") %>%
    select(uid)
"""

# %% --------


all_observed = ffmap(trials) do t
    @require lookup(t, :generation) == 4
    @require lookup(t, :M) == 5
    @require t.trial_number == 1
    t.knowledge
end

mean(all_observed) do edge
    edge.src == 5 || edge.dst == 5
end


# %% --------



possible = unique(reduce(vcat, get_solutions(pop, 4)))

# %% --------


R"""
tdf %>%
    group_by(M, generation, pid) %>%
    summarise(
        comp_known = mean(known_path_length == 2),
        comp_used = mean(path_length == 2)
    ) %>%
    ggplot(aes(generation, comp_known)) +
    point_line() +
    facet_wrap(~M)

fig()
"""

# %% ==================== evolution of compositionality ====================

sim = run_sims(30, 11, S=4, N=12, K=7, M=[5, 20, 50], ε=.14)
@rput sim

t = filter(trials) do t
    endswith(t.uid, "wd09aa4d")
end |> first
t.knowledge

R"""

participants %>%
    filter(M==5, generation==5) %>%
    left_join(
        tdf %>%
        filter(start != 5, goal !=5)
        group_by(pid) %>%
        summarise(compositionality = mean(path_length == 2))
    ) %>%
    select(workerid, compositionality, condition)

"""

R"""
human = tdf %>%
    filter(start != 5, goal != 5) %>%
    group_by(M, population, generation) %>%
    summarise(compositionality = mean(path_length == 2)) %>%
    mutate(agent = "human")

# df = bind_rows(mutate(sim, agent="model", population=100+population), human)

sim %>%
    ggplot(aes(generation, 1*compositionality, group=population)) +
    geom_line(linewidth=.5, color="#18BAFB", alpha=0.5) +
    geom_line(linewidth=1, data=human) +
    ylab("Two-Step Solution Rate") +
    facet_wrap(~M)

fig(w=5)

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
        (;e.src, e.dst)
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
