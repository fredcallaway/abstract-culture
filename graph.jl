using Memoize
using Graphs

include("utils.jl")
# include("find_compositions.jl")

@kwdef struct Environment
    S::Int = 5  # number of states
    M::Int = 5  # number of models
    K::Int = 1  # number of tasks per agent
    N::Real = 100 # population size
    ε::Float64 = 0. # lapse rate

    discovery_cost::Float64 = 1.
    travel_cost::Float64 = 0.2
    hub_discount::Float64 = 0.

    # internal variables
    graph::DiGraph = complete_digraph(S)
    edge_cost::Matrix{Float64} = begin
        X = (discovery_cost + travel_cost) .+ .01 * rand(S, S)
        X[1,:] .-= hub_discount
        X[:,1] .-= hub_discount
        X
    end
    tmp::Matrix{Float64} = zeros(S, S)
end

function initial_population(env::Environment)
    normalize!(ones(env.S, env.S))
end

@memoize function taskdist(env::Environment)
    S = env.S
    filter(collect(Iterators.product(1:S, 1:S))) do (s,g)
        s != g
    end
end

function transition(env::Environment, pop::Matrix{Float64})
    (;N, M, discovery_cost, travel_cost, hub_discount, graph, edge_cost) = env

    pop1 = zeros(size(pop))
    wts =  Weights(@view pop[:])
    n_obs = min(sum(!isequal(0), wts), M)


    for i in 1:N
        observed = sample(eachindex(pop), wts, n_obs; replace=false)
        for i in observed
            edge_cost[i] -= 1.
        end

        (start, goal) = rand(taskdist(env))
        path = a_star(graph, start, goal, edge_cost)

        # restore original edge_cost
        for i in observed
            edge_cost[i] += 1.
        end

        for edge in path
            pop1[edge.src, edge.dst] += 1
        end
    end
    normalize!(pop1)
end

function simulate(env::Environment, n_gen; init=initial_population(env))
    pop = init
    # note: initial pop is a dummy, containing no cultural knowledge
    repeatedly(n_gen) do
        pop = transition(env, pop)
        pop
    end
end
