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


function has_two_neighbors(graph, i, keep)
    n = 0
    for j in neighbors(graph, i)
        n += keep[j]
        n == 2 && return true
    end
    false
end

function composable_nodes!(graph, known)::Vector{Int}
    keep = Set(vertices(graph))
    keep = trues(vertices(graph))
    # remove loners

    @label top
    for i in vertices(graph)
        if keep[i] && i ∉ known && !has_two_neighbors(graph, i, keep)
            keep[i] = false
            @goto top
        end
    end
    vmap = rem_vertices!(graph, findall(!, keep))

    # check for at least one node with savings
    for comp in connected_components(graph)
        # good = any(comp) do i
        #     red_known(graph, i) || n_unknown_black(graph, i) > 2
        # end
        good = sum(comp) do i
            degree(graph, i) / 2 - (vmap[i] ∉ known)
        end > 0

        if !good
            for i in vmap[comp]
                keep[i] = false
            end
        end
    end
    findall(keep)
    # union!(keep, known)
end


function extract_knowledge(env, observed)
    red = Set{Int}()
    black = Set{Tuple{Int, Int}}()
    for b in observed
        (b.s == 0 || b.g == 0) && continue
        if b.red
            push!(red, b.s)
            push!(red, b.g)
        else
            push!(black, (b.s, b.g))
        end
    end
    (red, black)
end

function find_compositions(env, tasks, observed)
    known_red, known_black = extract_knowledge(env, observed)

    graph = Graph(2env.n)
    for (s, g) in tasks
        (s, g) in known_black && continue
        add_edge!(graph, s, g)
    end
    composable_nodes!(graph, known_red)
end



# tasks, observed = first(jobs)
# find_compositions(env, tasks, observed)