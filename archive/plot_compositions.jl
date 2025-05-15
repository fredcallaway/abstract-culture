using GraphMakie
include("red_black.jl")
include("find_compositions.jl")
include("figure.jl")

using Graphs, MetaGraphs

function task_graph(env, tasks, observed, cnodes)
    graph = MetaGraph(2env.S)
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
        Point(mod1(i, env.S)/2, div(i-1, env.S))
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



function parse_tasks(X)
    @chain begin
        collect(CartesianIndices(X))
        filter(i -> X[i] > 0, _)
        map(i -> i.I, _)
        map(((a, b),) -> (a, b + size(X, 1)), _)
    end
end

function parse_observed(X)
    res = Behavior[]
    for idx in CartesianIndices(X)
        (a, b) = idx.I
        if X[idx] > 0
            push!(res, Behavior(a, b+size(X, 1), X[idx] == 2))
        end
    end
    res
end

assigned_tasks = parse_tasks([
    1 1 1
    1 1 1
    1 1 1
])


observed = parse_observed([
    0 0 0
    1 1 1
    0 0 0
])

env = RedBlackEnv(K=1, D=10, S=3)

graph = task_graph(env, assigned_tasks, observed, [])

# figure() do
#     plot_task_graph(graph)
# end


knowledge = env.knowledege
fill!(knowledge, false)
for b in observed
    learn!(knowledge, b)
end

find_compositions!(env, assigned_tasks)


graph = task_graph(env, assigned_tasks, observed, [red_known(knowledge, i) for i in 1:2env.S])
figure() do
    plot_task_graph(graph)
end
