using Combinatorics

function find_compositions(tasks)
    tasks = unique(tasks)
    S, G = invert(tasks)
    S = unique(S)
    G = unique(G)


    best_cost = length(tasks)
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
        elseif total_cost == best_cost
            push!(optimal, (ss, gg))
        end
    end
    optimal
end


find_compositions(rand(taskdist(env), 10))

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

function compositional_graph(graph)
    graph = copy(graph)
    for step in vertices(graph)
        isempty(vertices(graph)) && return graph
        for i in vertices(graph)
            if degree(graph, i) < 2
                println("remove $i ", get_prop(graph, i, :label))
                rem_vertex!(graph, i)
                break
            elseif i == length(vertices(graph))
                return graph
            end
        end
    end
end

function composable_nodes(graph)
    cg = compositional_graph(graph)
    map(vertices(cg)) do i
        get_prop(cg, i, :label)
    end
end


env = Environment(k=1, m=10, n=3; ε=0.)

tasks = rand(taskdist(env).vals, 10)
# tasks = copy(taskdist(env).vals)
# setdiff!(tasks, [(1,1), (1,2), (1,3)])

graph = task_graph(env, tasks)

find_compositions(tasks)

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
    # for n in cnodes, n1 in cnodes
    #     rem_edge!(graph, n, n1)
    # end
    graphplot!(graph; layout, node_size=20, node_color, edge_color)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end