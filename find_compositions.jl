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


function composable_nodes!(graph, known)::Vector{Int}
    keep = Set(vertices(graph))
    keep = trues(vertices(graph))
    # remove loners

    @label top
    for i in vertices(graph)
        if keep[i] && i ∉ known && sum(keep[j] for j in neighbors(graph, i); init=0) < 2
            keep[i] = false
            @goto top
        end
    end

    # check for at least one node with savings
    # nodes = collect(vertices(graph))


    vmap = rem_vertices!(graph, findall(!, keep))

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
        if b.red
            push!(red, b.s)
            push!(red, b.g + env.n)
        else
            push!(black, (b.s, b.g + env.n))
        end
    end
    (red, black)
end

function find_compositions(env, tasks, observed)
    tasks = map(tasks) do (s, g)
        (s, g+env.n)
    end

    known_red, known_black = extract_knowledge(env, observed)
    setdiff!(tasks, known_black)

    graph = Graph(2env.n)
    for (s, g) in tasks
        add_edge!(graph, s, g)
    end

    cnodes = composable_nodes!(graph, known_red)
    res = (Int[], Int[])
    for n in cnodes
        push!(res[div(n-1, env.n) + 1], mod1(n, env.n))
    end
    res
end

# tasks, observed = first(jobs)
# find_compositions(env, tasks, observed)