using Graphs
# using MetaGraphs

function has_two_neighbors(graph, keep, i)
    n = 0
    for j in neighbors(graph, i)
        n += keep[j]
        n == 1 && return true
    end
    false
end

function find_compositions!(env, tasks)
    knowledge = env.knowledege  # this is the agent's knowledge (stored in env to avoid allocations)

    # the algorithm assumes all of this for now
    # @assert env.p_r == 0
    # @assert env.p_0 == 0
    # @assert env.p_brr == 0

    # build task graph
    graph = Graph(2env.S)
    for (s, g) in tasks
        if !black_known(knowledge, s, g)
            # we need to learn this task
            add_edge!(graph, s, g)
        end
    end

    keep = trues(vertices(graph))  # red nodes we will have in final solution

    # don't learn red nodes that participate in less than two solutions
    @label top
    for i in vertices(graph)
        if keep[i] && !red_known(knowledge, i) && !has_two_neighbors(graph, keep, i)
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
        savings = sum(comp) do i
            degree(graph, i) / 2 - !red_known(knowledge, vmap[i])
        end

        keep_comp = savings == 0 ? rand(Bernoulli(env.p_r)) : savings > 0
        if !keep_comp
            for i in vmap[comp]
                keep[i] = false
            end
        end
    end

    for i in eachindex(keep)
        if keep[i]
            learn_red!(knowledge, i)
        end
    end
end
