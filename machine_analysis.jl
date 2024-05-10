include("utils.jl")
include("data.jl")
include("figure.jl")
include("graph_plots.jl")
include("graph.jl")

using DataFrames, RCall

# %% --------

version = "v4"

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

participants = load_participants("v4.0", "v4.0g3", "v4.0g3b", "v4.0g4", "v4.0g6", "v4.0g7", "v4.0g8", "v4.0g9", "v4.0g10")
# @rsubset! participants :pid > 3  # old version??
# @rsubset! participants :uid ∉ ("v1.1-w6c294f2", "v1.1-wd2788fb")  # cheated
uids = participants.uid

trials = mapreduce(load_trials, vcat, uids)

function pframe(f, data=participants)
    @chain data begin
        @groupby :generation :pid
        @combine $AsTable = ffmap(f, :uid)
    end
end

function tframe(f, data=participants)
    pframe(data) do uid
        map(load_trials(uid)) do t
            (;t.trial_number, f(t)...)
        end
    end
end


using Dictionaries

GENERATIONS = Dict(participants.uid .=> participants.generation)
generation(uid::String)::Int = GENERATIONS[uid]
generation(t::Trial)::Int = generation(t.uid)
gtrials = Dictionaries.sortkeys(map(g->collect(group(get(:uid), g)), group(generation, trials)))

# %% --------

function run_sims(params::AbstractArray{<:NamedTuple}; generations=30)
    dataframe(params, parallel=true) do prm
        prm = delete(prm, :population)
        env = Environment(;prm..., discovery_cost=4., travel_cost=1.)
        # @require env.N ≥ env.M
        map(enumerate(simulate(env, generations))) do (generation, pop)
            compositionality = mean(pop) do solutions
                mean(solutions) do edges
                    length(edges) > 1
                end
            end
            F = normalize(edge_frequency(env, pop))
            (;generation, compositionality, entropy=entropy(F), unique_edges=sum(F .> 0))
        end
    end
end

function run_sims(repeats=1, generations=30; kws...)
    run_sims(grid(;kws..., population=1:repeats); generations)
end


# %% ==================== generations ====================

function edge_counts(trials::Vector{Trial}; S=6)
    X = zeros(Int, S, S)
    for e in flatmap(get(:path), trials)
        X[e.src, e.dst] += 1
    end
    X
end

df = map(pairs(gtrials)) do (generation, trials)
    F = normalize(edge_counts(reduce(vcat, trials)))
    (;generation, entropy=entropy(F), unique_edges=sum(F .> 0))
end |> DataFrame
@rput df

baseline_ent = entropy(ones(30) / 30)
@rput baseline_ent


# %% --------

sim = run_sims(100, 10, S=6, M=10, N=10, K=10, discovery_cost=4, travel_cost=1);
@rput sim

R"""

data = bind_rows(
    mutate(sim, agent="model"),
    mutate(df, agent="human")
) %>% fctrize(agent, levels=c("model", "human"))

data %>%
    ggplot(aes(generation, unique_edges, color=agent)) +
    geom_line(data=filter(data, population < 10), mapping=aes(group=population), linewidth=.1) +
    mean_line(min_n=0) +
    # geom_hline(yintercept=30) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
data %>% ggplot(aes(generation, entropy, color=agent)) +
    geom_line(data=filter(data, population < 10), mapping=aes(group=population), linewidth=.1) +
    mean_line(min_n=0) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    plot_layout(guides = 'collect') +
    labs(y="Edge Entropy") &
    scale_colour_manual(values=c(
        human="black",
        model="#18BAFB"
    ), aesthetics=c("fill", "colour"), name="")

fig("edges_entropy", w=6)
"""

# %% ==================== solution graphs ====================


n_gen = maximum(participants.generation)
facet_grid("graphs-generation", n_gen, 1) do col, row
    try
        E = 3 .* normalize(edge_counts(reduce(vcat, gtrials[col])))
        # E[E .> 0] .= .15
        plot_edge_frequency!(E)
    catch
        ax = current_axis()
        hidedecorations!(ax); hidespines!(ax)
    end
end

# %% --------

function plot_chain(name, env, seed=1; generations=10, chunk_size=1)
    Random.seed!(seed)
    sims = repeatedly(3) do
        sim = simulate(env, generations);
        map(chunks(sim, chunk_size)) do chunk
            reduce(vcat, reduce(vcat, chunk))
        end
    end

    facet_grid(name, length(sims[1]), length(sims)) do col, row
        pop = sims[row][col]
        plot_edge_frequency!(env, pop)
    end
end


plot_chain("model-graphs", Environment(S=6, M=10, N=10, K=10, discovery_cost=4, travel_cost=1.))
plot_chain("structure-v4K1", Environment(S=6, M=10, N=10, K=1, discovery_cost=4, travel_cost=1.))
plot_chain("structure-v4K1N30", Environment(S=6, M=10, N=30, K=1, discovery_cost=4, travel_cost=1.))


# %% ==================== individuals ====================

df = flatmap(pairs(gtrials)) do (generation, pop_trials)
    map(pop_trials) do trials
        F = normalize(edge_counts(trials))
        (;generation, trials[1].uid, entropy=entropy(F), unique_edges=sum(F .> 0))
    end
end |> DataFrame
@rput df


R"""
data = df %>% mutate(agent = "human")
data %>%
    ggplot(aes(generation, unique_edges, color=agent)) +
    geom_point(data=filter(data), mapping=aes(group=uid), linewidth=.1, alpha=0.1) +
    mean_line(min_n=0) +
    # geom_hline(yintercept=30) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
data %>% ggplot(aes(generation, entropy, color=agent)) +
    geom_line(data=filter(data), mapping=aes(group=uid), linewidth=.1) +
    mean_line(min_n=0) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    plot_layout(guides = 'collect') +
    labs(y="Edge Entropy") &
    scale_colour_manual(values=c(
        human="black",
        model="#18BAFB"
    ), aesthetics=c("fill", "colour"), name="")

fig(w=6)
"""

# %% --------


facet_grid("machine/$version-graphs-individual", n_gen, 10) do col, row
    try
        E = 3 .* normalize(edge_counts(gtrials[row][col]))
        # E[E .> 0] .= .15
        plot_edge_frequency!(E)
    catch
        ax = current_axis()
        hidedecorations!(ax); hidespines!(ax)
    end
end
# %% --------

sim = run_sims(100, 8, S=6, M=10, N=10, K=10, discovery_cost=4, travel_cost=1);
@rput sim

R"""

data = bind_rows(
    mutate(sim, agent="model"),
    mutate(df, agent="human")
) %>% fctrize(agent, levels=c("model", "human"))

data %>%
    ggplot(aes(generation, unique_edges, color=agent)) +
    geom_line(data=filter(data, population < 10), mapping=aes(group=population), linewidth=.1) +
    mean_line(min_n=0) +
    # geom_hline(yintercept=30) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
data %>% ggplot(aes(generation, entropy, color=agent)) +
    geom_line(data=filter(data, population < 10), mapping=aes(group=population), linewidth=.1) +
    mean_line(min_n=0) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    plot_layout(guides = 'collect') +
    labs(y="Edge Entropy") &
    scale_colour_manual(values=c(
        human="black",
        model="#18BAFB"
    ), aesthetics=c("fill", "colour"), name="")

fig("edges_entropy", w=6)
"""



# %% ==================== scratch ====================

flatmap(uids) do uid
    events_ = load_events(uid)
    map(filtermatch(events_, "machine.run")) do x
        x["transitions"]
    end
end |> unique


# %% --------

tdf = pframe() do uid
    map(load_trials(uid)) do t
        discovered = setdiff(t.traversed, t.knowledge)
        g = DiGraph(6)
        for e in t.knowledge
            add_edge!(g, e)
        end
        known_path_length = length(a_star(g, t.start, t.goal))
        (;t.trial_number, path_length = length(t.path), n_pull=length(t.attempts), known_path_length,
            n_discovered=length(discovered), n_known=length(t.knowledge))
    end
end
@rput tdf

# %% --------


R"""
tdf %>%
    group_by(generation, pid) %>%
    summarise(compositionality = mean(path_length > 1)) %>%
    ggplot(aes(generation, compositionality)) +
    point_line() +
    expand_limits(y=0)
fig("compositionality")
"""

R"""
tdf %>%
    group_by(generation, pid) %>%
    summarise(n_pull = mean(n_pull)) %>%
    ggplot(aes(generation, n_pull)) +
    point_line() +
    expand_limits(y=0)
fig("n_pull")
"""

R"""
tdf %>%
    ungroup() %>%
    group_by(known_path_length) %>%
    summarise(discovery=mean(n_discovered > 0)) %>%
    ggplot(aes(known_path_length, discovery)) +
    geom_bar(stat="identity")
fig()
"""

R"""
tdf %>%
    # filter(trial_number == 1) %>%
    group_by(generation, pid) %>%
    summarise(discovery = mean(n_discovered>0)) %>%
    ggplot(aes(generation, discovery)) +
    point_line() +
    expand_limits(y=0)
fig("discovery")
"""

R"""
tdf %>%
    filter(trial_number == 1) %>%
    group_by(generation, pid) %>%
    summarise(n_known = mean(n_known)) %>%
    ggplot(aes(generation, n_known)) +
    point_line() +
    expand_limits(y=0)
fig("known")
"""


# %% ==================== scratch ====================

trials = mapreduce(load_trials, vcat, participants.uid)

df = pframe() do uid
    map(load_trials(uid)) do t
        @require !isempty(t.path)
        discovered = setdiff(t.traversed, t.knowledge)
        (;
            t.trial_number,
            solution_type = ("black", "red")[length(t.path)],
            n_pull=length(t.attempts),
            n_discovered=length(discovered),
            has_black = Edge(t.start, t.goal) in t.knowledge,
            has_red_start = Edge(t.start, 5) in t.knowledge,
            has_red_end = Edge(5, t.goal) in t.knowledge,
            made_red = any(x->x[3] == 5, t.attempts),
            tried_red = any(x->x[1] == 5, t.attempts),
        )
    end
end
@rput df

R"""
df %>%
    ggplot(aes(trial_number, pid, color=solution_type)) +
    geom_point() +
    scale_colour_manual(values=c(
        "black", "red"
    ), aesthetics=c("fill", "colour"), name="solution")
fig("solution-grid")
"""

R"""
df %>%
    ggplot(aes(trial_number, 1*has_black)) +
    point_line()
fig()
"""

R"""
df %>%
    group_by(has_red_start, has_black) %>%
    summarise(mean(tried_red)) %>% kable
"""

R"""
df %>%
    props(solution_type)
"""

# %% --------
map(trials) do t
    sum(t.knowledge) do k

end


# %% --------


# %% --------

pulls = pframe() do uid
    map(load_trials(uid)) do t
        g = DiGraph(9)
        for e in t.knowledge
            add_edge!(g, e)
        end
        imap(t.attempts) do pull_number, (c1, s, c2)
            all_paths = collect(all_simple_paths(g, t.start, t.goal))

            pull = (;
                t.trial_number,
                pull_number,
                has_black_solution = any(p -> length(p) == 2, all_paths),
                has_red_solution = any(p -> length(p) == 2, all_paths),
                has_red_start = Edge(t.start, 5) in edges(g),
                has_red_end = Edge(5, t.goal) in edges(g),
                attempt_type =
                    c1 == t.start ? "start" :
                    c1 == 5 ? "red" :
                    "other",
                result =
                    isnothing(c2) ? "nothing" :
                    c2 == t.goal ? "goal" :
                    c2 == 5 ? "red" :
                    "other",
            )
            if !isnothing(c2)
                add_edge!(g, Edge(c1, c2))
            end
            pull
        end
    end
end
@rput pulls


# %% --------

R"""
df %>% filter(has_red_start) %>%

"""

R"""
df %>% ggplot(aes(trial_number, path_length, group=pid)) +
    geom_line(alpha=0.2, lw=1)
fig()
"""

R"""
df %>%
    ggplot(aes(path_length == n_pull)) +
    geom_quasirandom() + ylab("lever pulls")

fig()
"""

R"""
df %>%
    mutate(known_solution = path_length == n_pull) %>%
    ggplot(aes(trial_number, 1*known_solution)) +
    point_line()
fig()
"""

# %% ==================== timing ====================

# idents = Dict(v => k for (k, v) in JSON.parsefile("../machine-task/data/raw/$version/identifiers.json"))


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

# %% --------

@rput times

reuse_df

R"""
times %>%
    left_join(
    reuse_df %>%
        filter(!exact_match) %>%
        agg(social_reuse)
    ) %>%
    ggplot(aes(social_reuse, main)) +
    geom_point() +
    labs(y="time on task", x="")

fig()
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

println(feedback.feedback)

# "- " * join(feedback.whynot |> skipmissing |> collect, "\n - ") |> println


# %% ==================== old ====================


map(uids) do uid
    g = DiGraph(9)
    for e in initial_knowledge(uid)
        add_edge!(g, e)
    end
    (;start, goal) = load_trials(uid)[1]
    a_star(g, start, goal)
end

# %% --------


# TODO fix for middle graph
function known_solution_rate(S, M)
    all_edges = map(collect(Iterators.product(1:S, 1:S))[:]) do (a, b)
        @require a != b
        Edge(a, b)
    end |> skipmissing |> collect

    monte_carlo() do
    g = DiGraph(S)
        for e in sample(all_edges, M; replace=false)
            add_edge!(g, e)
        end

        mean(all_edges) do e
            length(a_star(g, e.src, e.dst)) > 0
        end
    end
end

# %% --------


