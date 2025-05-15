include("figure.jl")
using GraphMakie
using NetworkLayout

function Makie.plot!(g::AbstractGraph; kws...)
    defaults = (
        node_color="gray",
        node_size=40,
        node_strokewidth=2,
        edge_width=2,
        arrow_size=15,
        arrow_shift=:end,
        # layout=Shell()
    )
    kws = merge(defaults, kws)
    gp = graphplot!(g; kws...)
    ax = current_axis()
    ax.yautolimitmargin = (0.1, 0.1)
    ax.xautolimitmargin = (0.1, 0.1)
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end



function edge_frequency(env, pop)
    X = zeros(env.S, env.S)
    for pp in pop
        for path in pp
            for edge in path
                X[edge] += 1
            end
        end
    end
    normalize(X)
end

function plot_edge_frequency!(env, pop)
    E = edge_frequency(env, pop)
    edge_width = 30 .* [E[e.src, e.dst] for e in edges(env.graph)]
    arrow_size = map(edge_width) do ew
        ew == 0 && return 0
        max(3, 5. * ew^.89)
    end
    gp = plot!(env.graph; arrow_size, edge_width)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end

function edge_frequency(trials::Vector{Trial}; S=6)
    X = zeros(S, S)
    for e in flatmap(get(:path), trials)
        X[e.src, e.dst] += 1
    end
    normalize(X)
end

function plot_edge_frequency!(E::Matrix; S=6)
    g = complete_digraph(S)

    edge_width = 30 .* [E[e.src, e.dst] for e in edges(g)]
    arrow_size = map(edge_width) do ew
        ew == 0 && return 0
        max(3., 5. * ew^.89)
    end
    gp = plot!(g; arrow_size, edge_width)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end

