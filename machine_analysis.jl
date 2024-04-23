include("utils.jl")
include("data.jl")

using DataFrames, RCall

version = "v1"
# %% --------

R"""
suppressPackageStartupMessages(source("base.r"))
FIGS_PATH = "figs/v1-"
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

participants = @rsubset load_participants(version) :complete
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

get.(filtermatch(uids[1], "machine.run"), "recipes", missing)
get.(filtermatch(uids[1], "machine.run"), "goal", missing)

get.(filtermatch(uids[3], "machine.run"), "recipes", missing)

# %% --------


trials = mapreduce(load_trials, vcat, participants.uid)

uid = participants.uid[1]


workerid(participants.uid[1])
experiment.initialize

t = load_trials(participants.uid[1])[1]
t.knowledge

t.start
t.goal
t.knowledge


# %% --------


# %% --------




df = pframe() do uid
    map(load_trials(uid)) do t
        discovered = setdiff(t.traversed, t.knowledge)
        (;t.trial_number, path_length = length(t.path), n_pull=length(t.attempts), n_discovered=length(discovered))
    end
end
@rput df

# %% --------

function initial_knowledge(uid)
    edges = map(filtermatch(uid, "experiment.initialize")[1]["PARAMS"]["recipes"]) do (a,b,c)
        Edge(a+1, c+1)
    end
    alts = filtermatch(uids[1], "instruct.example.alt")
    # i'm not sure if this actually gets into the recipe book
    if !isempty(alts)
        push!(edges, Edge(alts[end]["chem1"] + 1, alts[end]["chem2"] + 1))
    end
    edges
end


map(uids) do uid
    g = DiGraph(5)
    for e in initial_knowledge(uid)
        add_edge!(g, e)
    end
    (;start, goal) = load_trials(uid)[1]
    a_star(g, start, goal)
end

# %% --------

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


filtermatch(uid, "experiment.initialize")[1]["PARAMS"]["transitions"]



initial_knowledge(uids[3])

SimpleDiGraphFromIterator


load_trials(uids[2])[1].traversed
load_trials(uids[2])[1].knowledge

filtermatch(uids[2], "machine.run")[1]["recipes"]
filtermatch(uids[2], "machine.run")[1]["recipes"]


# %% --------



R"""
df %>% with(mean(path_length == n_pull))
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

idents = Dict(v => k for (k, v) in JSON.parsefile("../blocks-task/data/raw/$version/identifiers.json"))


times = pframe() do uid
    events = load_events(uid)
    starts = filter(ematch("experiment.timeline.start"), events)

    ustarts = unique(e->e["event"], starts)
    if length(ustarts) < length(starts)
        @warn "repeated events for $uid"
        starts = ustarts
    end

    ends = filter(ematch("experiment.timeline.end"), events)
    d = map(starts, ends) do s, e
        block = rsplit(e["event"], "."; limit=2)[end]

        @assert (block == rsplit(s["event"], "."; limit=2)[end])
        block => (e["time"] - s["time"]) / 60000
    end |> Dict{String, Any}
    d["workerid"] = idents[split(uid, "-")[end]]
    d["solved"] = sum(load_trials(uid)) do t
        !t.practice && !ismissing(t.solution)
    end
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
    @infiltrate
    x = findnextmatch(events, 1, "debrief.submitted")[1]
    (difficulty=x["difficulty"], feedback=x["feedback"])
    # ensure_keys!(d, ["whynot", "feedback"])
end

feedback.feedback

# "- " * join(feedback.whynot |> skipmissing |> collect, "\n - ") |> println
