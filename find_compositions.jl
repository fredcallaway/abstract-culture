using Graphs
using MetaGraphs

function plot_task_graph(graph; node_color=:black, edge_color=:black)
    layout = (g) -> map(vertices(g)) do i
        Point(mod1(i, env.n)/2, div(i-1, env.n))
    end
    graphplot!(graph; layout, node_size=25, node_color, edge_color, edge_width=2.5)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    # ax.aspect = DataAspect()
end

function task_graph(env, tasks, observed)
    graph = MetaGraph(2env.n)
    for i in vertices(graph)
        set_prop!(graph, i, :label, i)
    end

    for (s, g) in tasks
        add_edge!(graph, s, g+env.n)
    end

    for beh in observed
        if beh.red
            for i in (beh.s, beh.g+env.n)
                set_prop!(graph, i, :observed, true)
            end
        else
            set_prop!(graph, beh.s, beh.g+env.n, :observed, true)
        end
    end
    graph
end

black_known(graph, i, j) = has_prop(graph, i, j, :observed)
red_known(graph, i) = has_prop(graph, i, :observed)

function n_unknown_black(graph, i)
    sum(neighbors(graph, i); init=0) do j
        !black_known(graph, i, j)
    end
    # WARNING we're not allowing for partially learned compositional solutions
end

function composable_nodes(graph)::Vector{Int}
    graph = copy(graph)
    # remove loners
    while !isempty(vertices(graph))
        for i in vertices(graph)
            if !red_known(graph, i) && n_unknown_black(graph, i) < 2
                rem_vertex!(graph, i)
                break  # vertex indices invalidated, start at beginning
            elseif i == length(vertices(graph))
                @goto done
            end
        end
    end
    @label done
    # check for at least one node with savings
    nodes = collect(vertices(graph))
    for comp in connected_components(graph)
        # good = any(comp) do i
        #     red_known(graph, i) || n_unknown_black(graph, i) > 2
        # end
        good = sum(comp) do i
            n_unknown_black(graph, i) / 2 - !red_known(graph, i)
        end > 0
        if !good
            setdiff!(nodes, comp)
        end
    end
    map(nodes) do i
        get_prop(graph, i, :label)
    end
end


function find_compositions(env, tasks, observed)
    graph = task_graph(env, tasks, observed)
    cnodes = composable_nodes(graph)
    res = (Int[], Int[])
    for n in cnodes
        push!(res[div(n-1, env.n) + 1], mod1(n, env.n))
    end
    res
end

