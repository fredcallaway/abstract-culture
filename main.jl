include("remote.jl")
connect_repl("d16")
@remote gethostname()

# %% --------

@both @everywhere include("graph.jl")
include("graph_plots.jl")
include("r.jl")

# %% --------

R"""
FIGS_PATH = "figs/graphs/"
"""

# %% ==================== social learning ====================

env = Environment(S=5)
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

observation_probability(Environment(S=5, M=10), 1)



# %% ==================== evolution ====================


@both function run_sims(repeats=1, generations=30; kws...)
    g = grid(;kws..., population=1:repeats)
    dataframe(g; parallel=true) do prm
        prm = delete(prm, :population)
        env = Environment(;prm...)
        map(enumerate(simulate(env, 100))) do (generation, pop)
            compositionality = mean(pop) do edges
                length(edges) > 1
            end
            (;generation, compositionality)
        end
    end
end


# %% ==================== vary M and S ====================

task = @asyncremote run_sims(10, S=2:30, M=0:5:150, N=[500], travel_cost=[0.])

# %% --------

# df_ms = fetch(task)
serialize("tmp/df_ms", df_ms)
@rput df_ms

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
# keep_s = 4:8:28
# keep_m = 10:30:100

env = Environment(hub_travel_discount=.005)

edge_costs(env, [])

task = @asyncremote run_sims(10, S = [5, 10, 20], M = [10, 20, 40], hub_travel_discount=[.005])
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


# %% ==================== vary hub cost ====================


df = run_sims(5, S=[10], M=[10], hub_discount=[.01])
@rput df

R"""
df %>% filter(generation < 31) %>%
    ggplot(aes(generation, compositionality)) +
    geom_line(aes(group=population), linewidth=.2, alpha=0.5)
fig()
"""

# %% --------

facet_grid(3) do col, row
    env = Environment(S=5, M=15, N=500, travel_cost=0.)
    sim = simulate(env, 100);
    pop = sim[end]
    plot_edge_frequency!(env, pop)
    # plot!([0,1], [0, col])
end

# %% --------

facet_grid(3) do col, row
    env = Environment(S=5, M=15, N=500, hub_discount=0.001, travel_cost=0.01)
    sim = simulate(env, 30);
    pop = sim[end]
    plot_edge_frequency!(env, pop)
    # plot!([0,1], [0, col])
end

# %% --------



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


env = Environment(S=5)
figure("graphs/graph", size=(300,300)) do
    Makie.plot!(env.graph)
end

# %% --------


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