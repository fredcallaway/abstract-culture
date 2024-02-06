using Combinatorics

function find_compositions(tasks)
    tasks = unique(tasks)
    S, G = invert(tasks)
    S = unique(S)
    G = unique(G)


    best_cost = base_cost = length(tasks)
    optimal = Tuple{Vector{Int64}, Vector{Int64}}[]

    compositions = product(powerset(S), powerset(G))
    foreach(compositions) do (ss, gg)
        black_cost = sum(tasks) do (s, g)
            s in ss && g in gg ? 0 : 1
        end
        red_cost = length(ss) + length(gg)
        total_cost = black_cost + red_cost
        if total_cost < best_cost
            best_cost = total_cost
            empty!(optimal)
            push!(optimal, (ss, gg))
        elseif total_cost == best_cost && total_cost < base_cost
            push!(optimal, (ss, gg))
        end
    end
    optimal
end

# %% --------

using Graphs
using MetaGraphs
using GraphMakie

function task_graph(env, tasks)
    graph = MetaGraph(2env.n)
    for i in vertices(graph)
        set_prop!(graph, i, :label, i)
    end

    for (s, g) in tasks
        add_edge!(graph, s, g+env.n)
    end
    graph
end

function composable_nodes(graph)
    graph = copy(graph)
    # remove loners
    while !isempty(vertices(graph))
        for i in vertices(graph)
            if degree(graph, i) < 2
                rem_vertex!(graph, i)
                break  # vertex indices invalidated, start at beginning
            elseif i == length(vertices(graph))
                @goto done
            end
        end
    end
    @label done
    # check for at least one node with 3 neighbors
    nodes = collect(vertices(graph))
    for comp in connected_components(graph)
        has3 = any(comp) do i
            degree(graph, i) > 2
        end
        if !has3
            nodes
            setdiff!(nodes, comp)
        end
    end
    map(nodes) do i
        get_prop(graph, i, :label)
    end
end

# %% --------

env = Environment(k=1, m=10, n=10; ε=0.)
tasks = rand(taskdist(env).vals, 30)


# %% --------

# tasks = copy(taskdist(env).vals)
# setdiff!(tasks, [(1,1), (1,2), (1,3)])

env = Environment(k=1, m=10, n=10; ε=0.)
tasks = rand(taskdist(env).vals, 20)
@time find_compositions(tasks)
# tasks = [
#     (1, 1),
#     (1, 2),
#     (1, 3),
#     (2, 1),
#     (2, 2),
#     (3, 3),
#     (2, 1),
#     (3, 2),
# ]

graph = task_graph(env, tasks)
figure() do
    layout = (g) -> map(vertices(g)) do i
        Point(mod1(i, env.n)/2, div(i-1, env.n))
    end

    # red = length(vertices(graph)) + 1
    cnodes = composable_nodes(graph)
    node_color = fill(:black, length(vertices(graph)))
    node_color[cnodes] .= :red
    edge_color = [:black for i in 1:ne(graph)]
    for (i, e) in enumerate(edges(graph))
        if e.src in cnodes && e.dst in cnodes
            edge_color[i] = :red
        end
    end
    sum(edge_color .== :red)
    # for n in cnodes, n1 in cnodes
    #     rem_edge!(graph, n, n1)
    # end
    graphplot!(graph; layout, node_size=20, node_color, edge_color)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    # ax.aspect = DataAspect()
end