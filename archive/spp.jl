@everywhere include("red_black.jl")
include("r.jl")

R"""
FIGS_PATH = "figs/spp/"
MAKE_PDF = FALSE
DPI = 500
"""

# %% ==================== individual ====================
g = grid(
    n = [160, 80, 40, 20],
    k_prop = [0; 0.5:.1:2.5; 3]
)

indiv = dataframe(g; parallel=true) do prm
    k = max(1, round(Int, prm.k_prop * prm.n))
    prm = delete(prm, :k_prop)
    env = Environment(;m=0, N=1000, foresight=true, p_r=0., k, prm...)
    (;k, red = red_rate(simulate(env, 1)[1]))
end

@rput indiv

R"""
indiv %>%
    ggplot(aes(k_prop, red, color=factor(n))) +
    geom_line() +
    discrete_sequential("Purples") + rev_legend +
    labs(x = "Tasks per Goal (k/n)", y="Compositionality Rate", color="Goals (n)")


fig("individual")
"""

# %% ==================== heatmap ====================

prob_black(n, m) = prob_observe(1 / (n^2), m)
prob_red(n, m) = prob_observe(1 / n, m)^2

prob_black(100, 1000)
prob_black(1000, 10000)

prob_red(100, 1000)

# %% --------

g = grid(
    m=5:5:5000,
    n=5:5:500
)

df = dataframe(g) do (;n, m)
    black = prob_observe(1 / (n^2), m)
    red = prob_observe(1 / n, m)^2
    (;black, red)
end

df2 = map(1:500) do n
    res = optimize(1, 20n) do m
        black = prob_observe(1 / (n^2), m)
        red = prob_observe(1 / n, m)^2
        -(red - black)
    end
    (;n, n2=n^2, m=res.minimizer, val=-res.minimum)
end |> DataFrame

@rput df df2

R"""
df2 = filter(df2, val > 0)
df %>%
    ggplot(aes(n, m, fill=red-black)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    geom_line(aes(y=n2, fill=NULL), df2, linewidth=.5, linetype="dashed") +
    no_gridlines +
    geom_abline() +
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(xlim=c(0,500), ylim=c(0,5000), expand=F) +
    labs(fill="Compositional Learning Advantage\n", x="Goals (n)", y="Social Observations (m)")

fig("advantage_heat", w=4)

"""

# %% ==================== evolve ====================


function run_sim_finite(;n_gen=30,
                 p_0 = [0],
                 p_brr = [0.],
                 p_r = [1.],
                 n = [5, 10, 20],
                 m = 2 .* [5, 10, 20],
                 k = [1],
                 N = [500],
                 foresight = [false],
                 repeats = 5,
                 )

    g = grid(; p_0, p_brr, p_r, n, m, k, N, foresight, pop=1:repeats)
    df = dataframe(g; parallel=true) do prm
        env = Environment(;delete(prm, :pop)...)
        sim = simulate(env, n_gen)
        map(enumerate(sim)) do (gen, pop)
            (;gen, red = red_rate(pop))
        end
    end
    @rput df
    df
end

# %% --------

vary_k = run_sim_finite(N=[500], k=[25, 30, 35], n=[40], m=[80], p_r=[0.], foresight=[true], repeats=10, n_gen=50)
@rput vary_k

R"""
vary_k %>%
    ggplot(aes(gen, red, color=factor(k))) +
    geom_line(mapping=aes(group = interaction(k, pop)), data=filter(vary_k, pop < 11), linewidth=.2, alpha=1) +
    lines(mean, linewidth=.7) +
    labs(x="Generation", y="Compositionality Rate", color="Tasks (k)") +
    discrete_sequential("Purple-Orange", rev=T) + rev_legend +
    ylim(0, 1)
fig("vary_k", w=4)
"""

vary_k

# %% --------

init = dataframe(grid(p_0=[.01, .03, .05], pop=1:10); parallel=true) do (;p_0)
    env = Environment(;N=500, p_0=0, k=25, n=40, m=80, p_r=0., foresight=true)
    init = transition(mutate(env; p_0), initial_population(env))
    sim = simulate(env, 29; init)
    map(enumerate([[init]; sim])) do (gen, pop)
        (;gen, k, red=red_rate(pop))
    end
end

@rput init

R"""
init %>% ggplot(aes(gen, red, color=factor(p_0), group=interaction(p_0, pop))) +
    geom_line(mapping=aes(group = interaction(p_0, pop)), data=filter(init, pop < 11), linewidth=.2, alpha=1) +
    lines(mean, linewidth=.7) +
    discrete_sequential("Blues", name="Initial Rate") +
    labs(x="Generation", y="Compositionality Rate") +
    coord_cartesian(xlim=c(NULL), ylim=c(0, 1))

fig("init", w=4)
"""


# %% ==================== vary N ====================

vary_N = run_sim_finite(N=[50, 100, 200], k=[30], n=[40], m=[80], p_r=[0.], foresight=[true], repeats=100, n_gen=50)
@rput vary_N
R"""

vary_N %>%
    ggplot(aes(gen, red, color=factor(N))) +
    geom_line(mapping=aes(group = interaction(N, pop)), data=filter(vary_N, pop < 5), linewidth=.2, alpha=.7) +
    lines(mean, linewidth=.7) +
    ylab("proportion using red") +
    teals_pal(rev=T) +
    ylim(0, 1)
fig("vary_N")
"""




