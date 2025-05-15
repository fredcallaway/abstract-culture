include("remote.jl")
connect_repl("hb120")
# RemoteREPL._repl_client_connection = nothing
@remote gethostname()

# %% --------

@both @everywhere include("graph.jl")
include("graph_plots.jl")
include("r.jl")

# %% --------

FIGS_PATH = "figs/graphs/"
@rput FIGS_PATH

# %% ==================== social learning ====================

env = Environment(S=5, discovery_cost=4., travel_cost=1.)
(;S, M, K, N, graph) = env

figure() do
    plot!(env.graph)
end

function num_paths(S)
    map(0:S-2) do n
        factorial(S - 2) ÷ factorial(S - 2 - n)
    end
end

function possible_paths(env, s, g)
    (;S, graph) = env
    map(all_simple_paths(graph, s, g)) do path
        map(sliding_window(path, 2)) do (src, dst)
            LinearIndices((S, S))[src, dst]
        end
    end
end

function possible_paths(env)
    (;S, graph) = env
    flatmap(tasks(env)) do (s, g)
        possible_paths(env, s, g)
    end
end

function observation_probability(env::Environment, len, N=100000)
    paths = unique(Set.(filter(x->length(x) == len, possible_paths(env))))
    my_paths = possible_paths(env, 1, 2)
    observation_probability(paths, my_paths, env.M, N)
end

function observation_probability(paths, my_paths, M, N)
    sort!(my_paths, by=length)
    c = zeros(Int, maximum(length.(my_paths)))
    for i in 1:N
        observed = reduce(union, rand(paths, M))
        for p in my_paths
            if all(e in observed for e in p)
                c[length(p)] += 1
                break
            end
        end
    end
    c ./ N
end


# %% ==================== evolution ====================

@both function run_sims(params::AbstractArray{<:NamedTuple}; generations=30)
    dataframe(params, parallel=true) do prm
        prm = delete(prm, :population)
        env = Environment(;prm..., discovery_cost=8., travel_cost=1.)
        @require env.N * env.K ≥ env.M
        map(enumerate(simulate(env, generations))) do (generation, pop)
            compositionality = mean(pop) do solutions
                mean(solutions) do edges
                    length(edges) > 1
                end
            end
            F = normalize(edge_frequency(env, pop))
            (;generation, compositionality, entropy=entropy(F), unique_edges=sum(F .> 0))
        end
    end
end

@both function run_sims(repeats=1, generations=30; kws...)
    run_sims(grid(;kws..., population=1:repeats); generations)
end


# %% ==================== simple ====================

@time df = run_sims(100, 10, S=6, M=10, N=10, K=10, discovery_cost=4, travel_cost=1);
@rput df

R"""
df %>%
    ggplot(aes(generation, unique_edges, color=factor(K))) +
    point_line()
fig()
"""

# %% ==================== vary M ====================

@time df = run_sims(100, 10, S=[6], M=[10], N=[5, 10, 15, 20]);
@rput df

R"""
df %>%
    ggplot(aes(generation, compositionality, color=factor(N))) +
    point_line()
fig()
"""


# %% ==================== vary M and S ====================

task = @asyncremote run_sims(10, S=2:30, M=0:5:150, N=[500])
df_zoom = fetch(task)

# %% --------

# df_ms = fetch(task)
serialize("tmp/df_ms", df_ms)
@rput df_ms

R"""
heat = df_ms %>%
    filter(generation == max(generation)) %>%
    agg(compositionality, c(S, M)) %>%
    ggplot(aes(S, M, fill=compositionality)) +
    geom_tile() +
    scale_fill_continuous_sequential(h1=0, h2=NA, c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(expand=F)


fig("heatmap", w=4)
"""

# %% ==================== zoom in ====================

task = @asyncremote run_sims(10, S=2:60, M=0:60, N=[500])
fetch(task)

# %% --------

# df_ms = fetch(task)
serialize("tmp/df_zoom", df_zoom)
@rput df_zoom

R"""
heat2 = df_zoom %>%
    filter(generation == max(generation)) %>%
    agg(compositionality, c(S, M)) %>%
    ggplot(aes(S, M, fill=compositionality)) +
    geom_tile() +
    scale_fill_continuous_sequential(h1=0, h2=NA, c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(expand=F)


fig("heatmap_zoom", w=4)
"""



# %% ==================== vary a lot ====================

df = run_sims(30, S=[6, 8], M=[10, 15, 20], N=[10, 15], K=[1,5])
@rput df
R"""

df %>%
    ggplot(aes(generation, unique_edges, color=factor(M), linetype=factor(N))) +
    # geom_line(mapping=aes(group=population), linewidth=.1) +
    mean_line(min_n=0) +
    # geom_hline(yintercept=30) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
df %>% ggplot(aes(generation, entropy, color=factor(M), linetype=factor(N))) +
    # geom_line(mapping=aes(group=population), linewidth=.1) +
    mean_line(min_n=0) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    plot_layout(guides = 'collect') +
    labs(y="Edge Entropy") &
    facet_grid(S ~ K, labeller=label_both)

fig(w=8, h=4)
"""


# %% ==================== dynamics ====================


task = run_sims(10, S = [5, 10, 20], M = [10, 20, 40])
df = fetch(task)
@rput df
R"""
df %>%
    # filter(S %in% $keep_s, M %in% $keep_m) %>%
    filter(generation < 31) %>%
    ggplot(aes(generation, compositionality)) +
    geom_line(aes(group=population), linewidth=.2, alpha=0.5) +
    facet_grid(M ~ S, labeller=label_glue(cols='S={S}', rows='M={M}'))

fig("MS-evolution", w=5,h=4)
"""

# %% ==================== population size ====================

task = @async run_sims(500, S = [5, 10, 20], M = [10, 20, 40], N=[10, 20, 40, 80])
df_n = fetch(task)
@rput df_n

R"""
df_n %>%
    ggplot(aes(generation, compositionality, color=factor(N))) +
    # geom_line(mapping=aes(group = interaction(N, population)), data=filter(df_n, population < 5), linewidth=.2, alpha=.7) +
    lines(mean, linewidth=.7) +
    # ylab("proportion using red") +
    teals_pal(rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(M ~ S, labeller=label_glue(cols='S={S}', rows='M={M}'))
fig("popsize", w=5, h=4)
"""

# %% --------

R"""
df_n %>%
    ggplot(aes(generation, compositionality, color=factor(M/N))) +
    # geom_line(mapping=aes(group = interaction(N, population)), data=filter(df_n, population < 5), linewidth=.2, alpha=.7) +
    lines(mean, linewidth=.7) +
    # ylab("proportion using red") +
    teals_pal(rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(M ~ S, labeller=label_glue(cols='S={S}', rows='M={M}'))
fig("tmp", w=5, h=4)
"""


# %% ==================== structure ====================

env = Environment(S=6)
figure("graphs/graph", size=(300,300)) do
    plot!(env.graph)
end

# %% --------

function plot_chain(name, env, seed=1; generations=10, chunk_size=1)
    Random.seed!(seed)
    sims = repeatedly(3) do
        sim = simulate(env, generations);
        map(chunks(sim, chunk_size)) do chunk
            reduce(vcat, reduce(vcat, chunk))
        end
    end

    facet_grid(name, length(sims[1]), length(sims)) do col, row
        pop = sims[row][col]
        plot_edge_frequency!(env, pop)
    end
end


plot_chain("structure-v4", Environment(S=6, M=10, N=10, K=10, discovery_cost=4, travel_cost=1.))
plot_chain("structure-v4K1", Environment(S=6, M=10, N=10, K=1, discovery_cost=8, travel_cost=1.))

plot_chain("structure-v4K1alt", Environment(S=6, M=20, N=20, K=1, discovery_cost=4, travel_cost=1.))

plot_chain("structure-v4K1N30", Environment(S=6, M=10, N=30, K=1, discovery_cost=4, travel_cost=1.))

# %% --------

plot_chain("structure10", Environment(S=6, M=10, N=10, discovery_cost=4, travel_cost=1.); generations=8, chunk_size=1)

# %% --------


plot_chain("structure10", Environment(S=5, M=10, N=10))
plot_chain("structure50", Environment(S=5, M=10, N=50); generations=30, chunk_size=5)
plot_chain("structure-hub10", Environment(S=5, M=10, N=10, hub_travel_discount=.005); generations=30, chunk_size=5)
plot_chain("structure-hub50", Environment(S=5, M=10, N=50, hub_travel_discount=.005); generations=30, chunk_size=5)

# %% --------

# env = Environment(S=5, M=10, N=10)

# sim = simulate(env, 100);

# fig, ax, gp = graphplot(env.graph, layout=Shell(), node_size=25, arrow_size=ones(env.S^2-env.S), arrow_shift=:end; edge_width=ones(env.S^2-env.S))

# ax = current_axis()
# hidedecorations!(ax); hidespines!(ax)
# ax.aspect = DataAspect()

# # animation settings
# framerate = 10

# record(fig, "anim.mp4", sim; framerate = framerate) do pop
#     E = edge_frequency(env, pop)
#     edge_width = 20 .* [E[e.src, e.dst] for e in edges(env.graph)]
#     gp.edge_width[] = edge_width
#     gp.arrow_size[] = 10 .* (edge_width .^ 0.5)
#     gp.arrow_size[]
# end
# run(`open anim.mp4`)

# %% ==================== vary hub cost ====================

df_hub = run_sims(10, S=[10], M=[15], N=[500], hub_travel_discount=range(.004, .006, step=.0005), rand_cost=[.002])
@rput df_hub

R"""

df_hub %>%
    ggplot(aes(generation, compositionality, color=factor(hub_travel_discount))) +
    lines() +
    # geom_line(aes(group=interaction(hub_travel_discount, population)), linewidth=.2, alpha=0.5) +
    # geom_line(aes(group=interaction(hub_travel_discount, population)), linewidth=.2, alpha=0.5) +
    discrete_sequential("Greens", name="hub discount") +
fig()
"""
