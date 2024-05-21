using StatsBase
using JSON
using Random

include("utils.jl")
include("data.jl")
include("graph.jl")

generation = parse(Int, ARGS[1])
println("Generating generation $generation")
n_mode = 8

version = "vM5"
Random.seed!(hash(version))

env = red_black_env(S=4, N=10, K=5, M=20)

n_state = nv(env.graph)
T = zeros(Int, n_state, n_state)
for t in tasks(env)
    T[t...] = rand(1:n_mode)
end

function edges_to_recipes(edges)
    map(edges) do e
        a, b = Tuple(e)
        [a, T[a, b], b]
    end
end

zero_index(x::Int) = x - 1
zero_index(x::AbstractArray) = map(zero_index, x)
zero_index(x::Tuple) = map(zero_index, x)
zero_index(x::NamedTuple) = map(zero_index, x)

function get_solutions(gen)
    if gen == 0
        gen0 = full_edges(env, simulate(env, 1)[1])
        reduce(vcat, gen0)
    else
        participants = load_participants("$(version)g$gen")
        @rsubset! participants begin
            :generation == gen
            :complete
        end
        uids = sample(participants.uid, env.N; replace=false)

        # make sure the transition structure hasn't changed
        jsonT = JSON.parse(json(zero_index(transpose(T))))
        @assert all(uids) do uid
            get_params(uid)["transitions"] == jsonT
        end

        trials = mapreduce(load_trials, vcat, uids)
        map(trials) do t
            t.path
        end
    end
end

function sample_recipes(gen)
    observed_solutions = sample(get_solutions(gen-1), env.M; replace=true)
    obs_edges = unique(reduce(vcat, observed_solutions))
    edges_to_recipes(obs_edges)
end

foreach(1:30) do i
    write("../machine-task/static/json/middle-$i.json", json((;
        generation,
        transitions = zero_index(transpose(T)),
        tasks = zero_index(sample(tasks(env), env.K, replace=true)),
        recipes = zero_index(sample_recipes(generation)),
        nChemical = nv(env.graph),
        nMode = n_mode,
    )))
end

