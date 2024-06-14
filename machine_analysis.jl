include("utils.jl")
include("data.jl")
include("figure.jl")
include("graph_plots.jl")
include("graph.jl")

using DataFrames, RCall

# %% --------

N_STATE = 11
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



function known_path_length(knowledge::Vector{<:Edge}, start, goal)
    g = DiGraph(N_STATE)
    for e in t.knowledge
        add_edge!(g, e)
    end
    known_path_length = length(a_star(g, t.start, t.goal))
end
known_path_length(t::Trial; knowledge=t.knowledge) = known_path_length(knowledge, t.start, t.goal)

tdf = pframe() do uid
    map(load_trials(uid)) do t
        discovered = setdiff(t.traversed, t.knowledge)
        (;t.trial_number, t.start, t.goal, duration=(t.end_time - t.start_time) / 1000,
            path_length = length(t.path), known_path_length=known_path_length(t),
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


# %% ==================== evolution of compositionality ====================

sim = run_sims(30, 11, S=4, N=12, K=7, M=[5, 20, 50], ε=.14)
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
    geom_line(linewidth=.5, color="#18BAFB", alpha=0.5) +
    facet_wrap(~M, labeller=label_glue("{M} Demonstrations"))

fig("evolution_predicitions", w=6, dpi=500)

plt + geom_line(linewidth=1, data=human)
fig("evolution", w=6, dpi=500)


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
