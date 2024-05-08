include("utils.jl")
include("data.jl")
include("figure.jl")
include("graph_plots.jl")

using DataFrames, RCall

# %% --------

version = "4"
@rput version
R"""
suppressPackageStartupMessages(source("base.r"))
FIGS_PATH = glue("figs/machine/{version}-")
cpal = scale_colour_manual(values=c(
    individual="#73AB84",
    social="#F5760A"
), aesthetics=c("fill", "colour"), name="")

"""


# %% --------

load_events("v4.0-wb30a1fa")[4]["PARAMS"]["example"]
# %% --------

function ffmap(f, args)
    results = skipmissing(map(f, args))
    while eltype(results) <: AbstractVector
        isempty(results) && return []
        results = skipmissing(reduce(vcat, results))
    end
    collect(results)
end

participants = load_participants("v4.0")
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

participants

# %% ==================== scratch ====================


flatmap(uids) do uid
    events_ = load_events(uid)
    map(filtermatch(events_, "machine.run")) do x
        x["transitions"]
    end
end |> unique


# %% --------

df = pframe() do uid
    map(load_trials(uid)) do t
        discovered = setdiff(t.traversed, t.knowledge)
        (;t.trial_number, path_length = length(t.path), n_pull=length(t.attempts), n_discovered=length(discovered))
    end
end
@rput df

R"""
df %>%
    group_by(generation, pid) %>%
    summarise(compositionality = mean(path_length > 1)) %>%
    ggplot(aes(generation, compositionality)) +
    point_line() +
    expand_limits(y=0)
fig()
"""


# %% ==================== trials ====================


df = pframe() do uid
    map(load_trials(uid)) do t
        discovered = setdiff(t.traversed, t.knowledge)
        (;t.trial_number, path_length = length(t.path), n_pull=length(t.attempts), n_discovered=length(discovered))
    end
end
@rput df

R"""
df %>%
    ggplot(aes(trial_number, n_pull, color=information_type)) +
    point_line() +
    expand_limits(y=0)
fig()
"""

R"""
df %>%
    ggplot(aes(trial_number, path_length, color=information_type)) +
    point_line() +
    expand_limits(y=0)
fig()
"""

# %% --------

R"""
df %>%
    mutate(half=if_else(trial_number>5, "late", "early")) %>%
    group_by(half, information_type, pid) %>%
    summarise(compositionality = mean(path_length > 1)) %>%
    ggplot(aes(half, compositionality, color=information_type, group=information_type)) +
    geom_quasirandom(alpha=0.3) +
    point_line()

fig(w=4)
"""


R"""

df %>%
    mutate(half=if_else(trial_number>5, "late", "early")) %>%
    group_by(half, information_type, pid) %>%
    summarise(n_pull = mean(n_pull)) %>%
    ggplot(aes(half, n_pull, color=information_type, group=information_type)) +
    geom_quasirandom(alpha=0.3) +
    point_line()

fig(w=4)
"""


# %% ==================== is information conserved ====================

INFO_TYPES = Dict(participants.uid .=> participants.information_type)
info_type(uid::String) = INFO_TYPES[uid]
info_type(t::Trial) = info_type(t.uid)


info_type(uids[1])
g = DiGraph(6)
for e in flatmap(get(:path), load_trials(uids[1]))
    add_edge!(g, e)
end

# %% --------

function edge_counts(S::Int, edges)
    X = zeros(S, S)
    for e in edges
        X[e.src, e.dst] += 1
    end
    X
end


function edge_frequency(trials::Vector{Trial}; S=6)
    X = zeros(S, S)
    for e in flatmap(get(:path), trials)
        X[e.src, e.dst] += 1
    end
    X ./ length(trials)
end

function plot_edge_frequency!(E::Matrix; S=6)
    g = complete_digraph(S)

    edge_width = 30 .* [E[e.src, e.dst] for e in edges(g)]
    arrow_size = map(edge_width) do ew
        ew == 0 && return 0
        max(3, 5. * ew^.89)
    end
    gp = plot!(g; arrow_size, edge_width)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end


gtrials = collect(map(g->collect(group(get(:uid), g)), group(info_type, trials)))

facet_grid(10, 3) do col, row
    try
        plot_edge_frequency!(edge_frequency(gtrials[row][col]))
    catch
        ax = current_axis()
        hidedecorations!(ax); hidespines!(ax)
    end
end

# %% --------


solutions = map(trials) do t
    Tuple.(t.path)
end

tt = sample(trials, 10; replace=false)
map(get(:path), tt)
figure() do
    E = edge_frequency(tt)
    E[E .> 0] .= .1
    plot_edge_frequency!(E)
end
# %% --------


# doesn't work bc everyone has a different hub
# g_edges = map(group(info_type, trials)) do trials
#     flatmap(trials) do t
#         t.path
#     end
# end






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


