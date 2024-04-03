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
        layout=Shell()
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
    for path in pop
        for edge in path
            X[edge] += 1
        end
    end
    X ./ length(pop)
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