include("utils.jl")
include("data.jl")

using DataFrames, RCall

version = "v2.1"
@rput version
# %% --------

R"""
suppressPackageStartupMessages(source("base.r"))
FIGS_PATH = glue("figs/machine/{version}-")
cpal = scale_colour_manual(values=c(
    individual="#73AB84",
    social="#F5760A"
), aesthetics=c("fill", "colour"), name="")

"""
# %% --------

function ffmap(f, args)
    results = skipmissing(map(f, args))
    while eltype(results) <: AbstractVector
        isempty(results) && return []
        results = skipmissing(reduce(vcat, results))
    end
    collect(results)
end

participants = load_participants(version)
uids = participants.uid

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

R"""

"""


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

idents = Dict(v => k for (k, v) in JSON.parsefile("../machine-task/data/raw/$version/identifiers.json"))


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
    d["workerid"] = idents[split(uid, "-")[end]]
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


