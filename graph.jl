using Memoize
using Graphs

include("utils.jl")
# include("find_compositions.jl")

# use hub if travel_cost + rand_cost < hub_discount


@kwdef struct Environment
    graph::SimpleDiGraph{Int64}

    M::Int = 5  # number of models
    K::Int = 1  # number of tasks per agent
    N::Real = 100 # population size
    ε::Float64 = 0. # lapse rate

    starts::Vector{Int} = collect(vertices(graph))
    goals::Vector{Int} = collect(vertices(graph))
    hub::Int = 0

    discovery_cost::Float64 = 1.
    travel_cost::Float64 = 0.01
    hub_discovery_discount::Float64 = 0.0
    hub_travel_discount::Float64 = 0.0
    rand_cost::Float64 = 1e-8

    # internal variables
    # base_cost::Matrix{Float64} = begin
    #     X = @. (discovery_cost + travel_cost) * ones(S, S)
    #     X[1,:] .-= hub_discount
    #     X[:,1] .-= hub_discount
    #     X
    # end
    tmp::Matrix{Float64} = zeros(nv(graph), nv(graph))
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
    fill([Int[]], env.N)
end

@memoize function tasks(env::Environment)
    filter(collect(Iterators.product(env.starts, env.goals))) do (s,g)
        s != g
    end
end

function observed_edges(pop_behavior)
    observed = Set{Int}()
    for behavior in pop_behavior
        for edges in behavior
            for e in edges
                push!(observed, e)
            end
        end
    end
    observed
end

function transition(env::Environment, pop::Vector{Vector{Vector{Int}}})
    (;M, K, N, graph, ε) = env
    @assert env.hub_discovery_discount == 0  # assumed below
    map(1:N) do i
        all_solutions = reduce(vcat, pop)
        if all(isempty, all_solutions)
            observed = Set{Int}()
        else
            models = sample(all_solutions, M; replace=false)
            observed = observed_edges(models)
        end
        # all_edges = SplitApplyCombine.flatten(pop)
        # observed = Set(sample(all_edges, min(length(all_edges), M); replace=false))

        E = edge_costs(env, observed)
        map(1:K) do i
            (start, goal) = rand(tasks(env))
            if rand() < ε
                path = [Edge(start, goal)]
            else
                path = a_star(graph, start, goal, E)
            end
            @infiltrate isempty(path)
            @assert !isempty(path)
            path_edges = map(path) do e
                LinearIndices((nv(graph), nv(graph)))[e.src, e.dst]
            end
            for i in path_edges
                if i ∉ observed
                    E[i] -= env.discovery_cost
                    @assert E[i] > 0
                    push!(observed, i)
                end
            end
            path_edges
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


function edge_counts(env::Environment, pop)
    S = nv(env.graph)
    X = zeros(Int, S, S)
    for solutions in pop
        for path in solutions
            for edge in path
                X[edge] += 1
            end
        end
    end
    X
end

function red_black_graph(n_start, n_goal)
    graph = DiGraph(n_start + n_goal + 1)
    for s in 1:(n_start+1)
        for g in (n_start+1):nv(graph)
            if s ≠ g
                add_edge!(graph, s, g)
            end
        end
    end
    graph
end

function compositionality(pop::Vector{Vector{Vector{Int64}}})
    mean(pop) do solutions
        mean(solutions) do edges
            length(edges) > 1
        end
    end
end

function full_edges(env, pop)
    S = nv(env.graph)
    C = CartesianIndices((S, S))
    map(pop) do solutions
        map(solutions) do path
            map(path) do edge
                Edge(C[edge].I)
            end
        end
    end
end

function red_black_env(;S, kws...)
    graph = red_black_graph(S, S)
    Environment(;graph, starts=1:S+1, goals=S+1:2S+1, kws...)
end

function run_sims(params::AbstractArray{<:NamedTuple}; generations=30)
    dataframe(params, parallel=true) do prm
        prm = delete(prm, :population)
        env = red_black_env(;prm...)
        @require env.N * env.K ≥ env.M
        map(enumerate(simulate(env, generations))) do (generation, pop)
            compositionality = mean(pop) do solutions
                mean(solutions) do edges
                    length(edges) > 1
                end
            end
            (;generation, compositionality)
        end
    end
end

function run_sims(repeats=1, generations=30; kws...)
    run_sims(grid(;kws..., population=1:repeats); generations)
end
