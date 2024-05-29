using GraphMakie
include("red_black.jl")
include("find_compositions.jl")
include("figure.jl")

using Graphs, MetaGraphs

function task_graph(env, tasks, observed, cnodes)
    graph = MetaGraph(2env.n)
    for i in vertices(graph)
        set_prop!(graph, i, :label, i)
    end

    for (s, g) in tasks
        add_edge!(graph, s, g)
    end

    for beh in observed
        if beh.red
            for i in (beh.s, beh.g)
                @assert set_prop!(graph, i, :observed, true)
            end
        else
            set_prop!(graph, beh.s, beh.g, :observed, true)
        end
    end
    for i in eachindex(cnodes)
        if cnodes[i]
            set_prop!(graph, i, :learned, true)
        end
    end
    graph
end

function plot_task_graph(graph)
    layout = (g) -> map(vertices(g)) do i
        Point(mod1(i, env.n)/2, div(i-1, env.n))
    end
    node_color = map(vertices(graph)) do i
        if has_prop(graph, i, :observed)
            :blue
        elseif has_prop(graph, i, :learned)
            :red
        else
            :black
        end
    end
    edge_color = map(edges(graph)) do e
        if has_prop(graph, e.src, e.dst, :observed)
            :blue
        elseif has_prop(graph, e.src, :learned) && has_prop(graph, e.dst, :learned)
            :lightgray
        else
            :black
        end
    end
    graphplot!(graph; layout, node_size=25, node_color, edge_color, edge_width=2.5)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    # ax.aspect = DataAspect()
end


# %% --------

env = Environment(k=1, m=10, n=4, ε=0., p_r=1.)
tasks = [
    (1, 5),
    (1, 6),
    # (1, 8),
    (2, 5),
    (2, 6),
    (2, 7)
    # (3, 8),
    # (2, 5),
    # (3, 6),
]

observed = [
    # Behavior(1, 5, false),
    # Behavior(1, 2, false),
    # Behavior(3, 4, true),
]

graph = task_graph(env, tasks, observed, [])


# %% --------

K = env.K
fill!(K, false)
for b in observed
    learn!(K, b)
end

find_compositions!(env, tasks)


graph = task_graph(env, tasks, observed, [red_known(K, i) for i in 1:2env.n])
figure() do
    plot_task_graph(graph)
end
