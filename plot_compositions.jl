using GraphMakie
include("black_red.jl")
include("find_compositions.jl")
include("figure.jl")

# %% --------


env = Environment(k=1, m=10, n=4; ε=0.)
tasks = [
    (1, 1),
    (1, 2),
    (1, 4),
    (2, 1),
    (2, 2),
    (3, 4),
    (2, 1),
    (3, 2),
]

observed = [
    Behavior(1, 1, false),
    Behavior(1, 2, false),
    Behavior(3, 4, true),
]


graph = task_graph(env, tasks, observed)

figure() do
    node_color = map(vertices(graph)) do i
        if has_prop(graph, i, :observed)
            :blue
        else
            :black
        end
    end
    edge_color = map(edges(graph)) do e
        if has_prop(graph, e.src, e.dst, :observed)
            :blue
        else
            :black
        end
    end
    plot_task_graph(graph; node_color, edge_color)
end

# %% --------

env = Environment(k=10, m=10, n=4; ε=0.)

observed = map(rand(taskdist(env), env.m)) do (s, g)
    Behavior(s, g, rand(Bernoulli(0.4)))
end

tasks = rand(taskdist(env), env.k)

graph = task_graph(env, tasks, observed)
cnodes = composable_nodes(graph)

figure() do
    node_color = map(vertices(graph)) do i
        if has_prop(graph, i, :observed)
            :blue
        elseif i in cnodes
            :red
        else
            :black
        end
    end
    edge_color = map(edges(graph)) do e
        if has_prop(graph, e.src, e.dst, :observed)
            :blue
        elseif e.src in cnodes && e.dst in cnodes
            :lightgray
        else
            :black
        end
    end
    plot_task_graph(graph; node_color, edge_color)
end


