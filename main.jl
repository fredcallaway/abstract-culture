@everywhere include("graph.jl")

# %% --------

include("figure.jl")
using GraphMakie
using NetworkLayout
include("r.jl")

# %% --------

R"""
FIGS_PATH = "figs/graphs/"
"""

# %% --------
using RemoteREPL
connect_repl("d16")
# macro remote(expr)
#     expr
#     # RemoteREPL.@remote expr
# end

@remote gethostname()

# %% --------

function run_sims(repeats=1; kws...)
    g = grid(;kws..., population=1:repeats)
    dataframe(g; parallel=true) do prm
        prm = delete(prm, :population)
        env = Environment(;prm...)
        map(enumerate(simulate(env, 100))) do (generation, pop)
            compositionality = mean(pop) do edges
                length(edges) > 1
            end
            (;generation, compositionality)

            # this is incorrect; edges are indexed in full matrix, not edgelist
            # edge_counts = counts(SplitApplyCombine.flatten(pop), 1:ne(env.graph))

            # state_counts = zeros(Int, env.S)
            # all_edges = collect(edges(env.graph))
            # foreach(enumerate(edge_counts)) do (e, c)
            #     s = all_edges[e].dst
            #     state_counts[s] += c
            # end
            # (;generation, compositionality, edge_counts, state_counts)
        end
    end
end

run_sims(5, )

# %% ==================== plot environment ====================

task = @asyncremote begin
    g = grid(;S=2:30, M=0:5:150, population=1:20)
    dataframe(g; parallel=true) do (;M, S)
        env = Environment(;M, S, N=500, travel_cost=.01, rand_cost=.001)
        map(enumerate(simulate(env, 100))) do (generation, pop)
            compositionality = mean(pop) do edges
                length(edges) > 1
            end
            (;generation, compositionality)

            # this is incorrect; edges are indexed in full matrix, not edgelist
            # edge_counts = counts(SplitApplyCombine.flatten(pop), 1:ne(env.graph))

            # state_counts = zeros(Int, env.S)
            # all_edges = collect(edges(env.graph))
            # foreach(enumerate(edge_counts)) do (e, c)
            #     s = all_edges[e].dst
            #     state_counts[s] += c
            # end
            # (;generation, compositionality, edge_counts, state_counts)
        end
    end
end

df_ms = fetch(task)
@rput df_ms

# %% ==================== vary M and S ====================

R"""
df_ms %>%
    filter(generation == 100) %>%
    agg(compositionality, c(S, M)) %>%
    ggplot(aes(S, M, fill=compositionality)) +
    geom_tile() +
    scale_fill_continuous_sequential(h1=0, h2=NA, c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(expand=F)


fig("heatmap", w=4)
"""

# %% --------
keep_s = 4:8:28
keep_m = 10:30:100

R"""
df_ms %>%
    filter(S %in% $keep_s, M %in% $keep_m) %>%
    filter(generation < 31) %>% 
    ggplot(aes(generation, compositionality)) +
    geom_line(aes(group=population), linewidth=.2, alpha=0.5) +
    facet_grid(S ~ M)

fig(w=5,h=5)
"""

# %% --------





# %% ==================== vary hub cost ====================


facet_grid(3) do col, row
    env = Environment(S=6, M=20, N=500, hub_discount=0.0, travel_cost=-0.005)
    sim = simulate(env, 100);
    pop = sim[end]
    plot_edge_frequency!(env, pop)
    # plot!([0,1], [0, col])
end

# %% --------


df = run_sims(10; S=[6], M=[20], N=[500], hub_discount=[.105])





# %% --------


g = grid(
    S = [10],
    N = [500],
    M = [100],
    repeat = 1:10,
    hub_discount=0:.05:.2,
)
df = dataframe(g; parallel=true) do kws
    kws = delete(kws, :repeat)
    env = Environment(;kws...)
    pop = simulate(env, 100)[end]

    compositionality = mean(pop) do edges
        length(edges) > 1
    end
    (;compositionality)
end
@rput df

R"""
df %>%
    ggplot(aes(hub_discount, compositionality)) +
    point_line()

fig()
"""

# %% ==================== plots ====================
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
    gp = graphplot!(env.graph; kws...)
    ax = current_axis()
    ax.yautolimitmargin = (0.1, 0.1)
    ax.xautolimitmargin = (0.1, 0.1)
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()

end

env = Environment(S=5)
figure("graphs/graph", size=(300,300)) do
    Makie.plot!(env.graph)
end

# %% --------

function edge_frequency(env, pop)
    X = zeros(env.S, env.S)
    for path in pop
        for edge in path
            X[edge] += 1
        end
    end
    normalize!(X)
end

function plot_edge_frequency!(env, pop)
    E = edge_frequency(env, pop)
    edge_width = 40 .* [E[e.src, e.dst] for e in edges(env.graph)]
    gp = plot!(env.graph; arrow_size=10 .* (edge_width .^ 0.5), edge_width)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end

facet_grid(3) do col, row
    env = Environment(S=6, M=10, N=500)
    sim = simulate(env, 100);
    pop = sim[end]
    plot_edge_frequency!(env, pop)
end

# %% --------

C = @bywrap df [:S, :M] mean(:compositionality)


# e%% --------

env = Environment(S=6, M=10, N=100, hub_discount=0.11)
monte_carlo(100) do
    sim = simulate(env, 30; init=pop)
    visit_rates = sum(sim[end]; dims=2)[:]
    hubbiness, hub = findmax(visit_rates)
    hubbiness /= (1/env.S)
end

# %% --------

env = Environment(S=5, M=8, N=100, hub_discount=0.05)
sim = simulate(env, 100)

pop = sim[end]
figure() do
    g = complete_digraph(5)
    edge_width = 20 .* [pop[e.src, e.dst] for e in edges(g)]
    gp = graphplot!(g, layout=Shell(), node_size=25, arrow_size=10 .* (edge_width .^ 0.5), arrow_shift=:end; edge_width)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end

# %% --------
env = Environment(S=5, M=10, N=30, hub_discount=0.)

sim = simulate(env, 100);

fig, ax, gp = graphplot(env.graph, layout=Shell(), node_size=25, arrow_size=ones(env.S^2-env.S), arrow_shift=:end; edge_width=ones(env.S^2-env.S))
ax = current_axis()
hidedecorations!(ax); hidespines!(ax)
ax.aspect = DataAspect()

# animation settings
framerate = 10

record(fig, "anim.mp4", sim; framerate = framerate) do pop
    E = edge_frequency(env, pop)
    edge_width = 20 .* [E[e.src, e.dst] for e in edges(env.graph)]
    gp.edge_width[] = edge_width
    gp.arrow_size[] = 10 .* (edge_width .^ 0.5)
    gp.arrow_size[]
end
run(`open anim.mp4`)