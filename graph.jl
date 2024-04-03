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
    travel_cost::Float64 = 0.01
    hub_discovery_discount::Float64 = 0.0
    hub_travel_discount::Float64 = 0.0
    rand_cost::Float64 = 1e-8

    # internal variables
    graph::DiGraph = complete_digraph(S)
    # base_cost::Matrix{Float64} = begin
    #     X = @. (discovery_cost + travel_cost) * ones(S, S)
    #     X[1,:] .-= hub_discount
    #     X[:,1] .-= hub_discount
    #     X
    # end
    tmp::Matrix{Float64} = zeros(S, S)
end

function edge_costs(env, observed)
    (;discovery_cost, travel_cost, hub_discovery_discount, hub_travel_discount, rand_cost) = env
    X = env.tmp
    if rand_cost > 0
        rand!(X)
        @. X = rand_cost * (X - 0.5)
    else
        zeros!(X)
    end

    C = CartesianIndices(X)
    for e in eachindex(X)
        src, dst = C[e].I
        is_hub = src == 1 || dst == 1

        X[e] += travel_cost - is_hub * hub_travel_discount
        if e ∉ observed
            X[e] += discovery_cost - is_hub * hub_discovery_discount
        end
    end
    X
end

function initial_population(env::Environment)
    fill(Int[], env.N)
end

@memoize function tasks(env::Environment)
    S = env.S
    filter(collect(Iterators.product(1:S, 1:S))) do (s,g)
        s != g
    end
end

function observed_edges(behavior)
    observed = Set{Int}()
    for edges in behavior
        for e in edges
            push!(observed, e)
        end
    end
    observed
end


function transition(env::Environment, pop::Vector{Vector{Int}})
    (;S, M, K, N, graph) = env
    map(1:N) do i
        models = sample(pop, M; replace=false)
        observed = observed_edges(models)

        (start, goal) = rand(tasks(env))
        path = a_star(graph, start, goal, edge_costs(env, observed))

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
