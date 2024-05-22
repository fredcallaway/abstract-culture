using StatsBase
using JSON
using Random
using Memoize

include("utils.jl")
include("data.jl")
include("graph.jl")

n_mode = 6
version = "vM5"
gen = parse(Int, ARGS[1])

# Random.seed!(hash(version))  # this shouldn't be necessary but just in case

@kwdef struct Population
    name::String
    id::Int
    env::Environment
    rng::MersenneTwister
    T::Matrix{Int}
end

function Population(name, id; S=4, N=20, K=5, M)
    env = red_black_env(;S, N, K, M)
    rng = MersenneTwister(hash(version * string(id)))
    n_state = nv(env.graph)
    T = zeros(Int, n_state, n_state)
    for t in tasks(env)
        T[t...] = rand(rng, 1:n_mode)
    end
    Population(name, id, env, rng, T)
end

function edges_to_recipes(pop::Population, edges)
    map(edges) do e
        a, b = Tuple(e)
        [a, pop.T[a, b], b]
    end
end

@memoize function get_solutions(pop, gen)
    env = pop.env
    @assert gen ≥ 1

    participants = load_participants("$(version)g$gen")
    @assert all(participants.generation .== gen)
    @assert all(participants.complete)

    @rsubset! participants :population == string(pop.name, "-", pop.id)
    if nrow(participants) != env.N
        error("Incorrect number of participants: $(nrow(participants))")
    end

    # make sure the transition structure hasn't changed
    jsonT = JSON.parse(json(zero_index(transpose(pop.T))))
    @assert all(participants.uid) do uid
        get_params(uid)["transitions"] == jsonT
    end

    trials = mapreduce(load_trials, vcat, participants.uid)
    map(trials) do t
        t.path
    end
end

function sample_tasks(pop::Population)
    env = pop.env
    sample(tasks(env), env.K, replace=true)
end

function sample_recipes(pop::Population, gen::Int)
    if gen == 1
        return Vector{Int}[]
    end
    observed_solutions = sample(get_solutions(pop, gen-1), pop.env.M; replace=true)
    obs_edges = unique(reduce(vcat, observed_solutions))
    edges_to_recipes(pop, obs_edges)
end

function write_configs(pop::Population, generation::Int)
    dir = "../machine-task/static/json/$(pop.name)-$(pop.id)"
    mkpath(dir)
    foreach(1:300) do i
        write("$dir/$i.json", json((;
            pop_name = pop.name,
            pop_id = pop.id,
            generation,
            transitions = zero_index(transpose(pop.T)),
            tasks = zero_index(sample_tasks(pop)),
            recipes = zero_index(sample_recipes(pop, generation)),
            nChemical = nv(pop.env.graph),
            nMode = n_mode,
        )))
    end
end

populations = [
    Population("M5", 1; M=5),
    Population("M20", 1; M=20),
    Population("M50", 1; M=50),
]

mkpath("envs")
for pop in populations
    serialize("envs/$(pop.name)", pop.env)
    write_configs(pop, gen)
end

