using Memoize
using Graphs

include("utils.jl")
# include("find_compositions.jl")

# use hub if travel_cost + rand_cost < hub_discount


@kwdef struct Environment
    S::Int = 5  # number of states
    M::Int = 5  # number of models
    K::Int = 1  # number of tasks per agent
    N::Real = 100 # population size
    ε::Float64 = 0. # lapse rate

    discovery_cost::Float64 = 1.
    travel_cost::Float64 = 0.2
    hub_discount::Float64 = 0.0
    rand_cost::Float64 = .01

    # internal variables
    graph::DiGraph = complete_digraph(S)
    edge_cost::Matrix{Float64} = begin
        X = (discovery_cost + travel_cost) .+ rand_cost * rand(S, S)
        X[1,:] .-= hub_discount
        X[:,1] .-= hub_discount
        X
    end
    tmp::Matrix{Float64} = zeros(S, S)
end

function initial_population(env::Environment)
    fill(Int[], env.N)
end

@memoize function taskdist(env::Environment)
    S = env.S
    filter(collect(Iterators.product(1:S, 1:S))) do (s,g)
        s != g
    end
end


function transition(env::Environment, pop::Vector{Vector{Int}})
    (;N, M, S, discovery_cost, travel_cost, hub_discount, graph, edge_cost) = env

    pop1 = zeros(size(pop))
    map(1:N) do i
        models = sample(pop, M; replace=false)

        observed = Set{Int}()
        for edges in models
            for e in edges
                push!(observed, e)
            end
        end
        for e in observed
            edge_cost[e] -= discovery_cost
        end

        (start, goal) = rand(taskdist(env))
        path = a_star(graph, start, goal, edge_cost)

        # restore original edge_cost
        for e in observed
            edge_cost[e] += discovery_cost
        end

        map(path) do e
            LinearIndices((S, S))[e.src, e.dst]
        end
    end
end

function simulate(env::Environment, n_gen; init=initial_population(env))
    pop = init
    # note: initial pop is a dummy, containing no cultural knowledge
    repeatedly(n_gen) do
        pop = transition(env, pop)
        pop
    end
end
