@everywhere include("red_black.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim
# %% --------

R"""
FIGS_PATH = "figs/neuromonster/"
MAKE_PDF = FALSE
DPI = 500
RED = "#C82506"

facet_grid = function(form, labeller=purrr::partial(label_both, sep = " = "), ...) {
    ggplot2::facet_grid(form, labeller=labeller, ...)
}
"""

# %% ==================== setup ====================

function run_sim_infinite(;n_gen=50, init=NaN,
                 p_0 = 1e-6,
                 p_brr = 0.,
                 p_r = 1.,
                 S = [5, 10, 20],
                 M = [10, 20, 40])

    g = grid(; p_0, p_brr, p_r, S, M)
    df = dataframe(g) do prm
        env = Environment(;prm...)
        sim = simulate(env, n_gen; init)
        map(enumerate(sim)) do (gen, red)
            (;gen, red)
        end
    end
    @rput df
    df
end

R"""


plot_red = function(...) {
    df %>%
        ggplot(aes(gen, red, ...)) +
        geom_line() +
        facet_grid(M ~ S)

}
"""

function run_sim_finite(;n_gen=30, init=NaN,
                 p_0 = [0],
                 p_brr = [0.],
                 p_r = [1.],
                 S = [5, 10, 20],
                 M = [10, 20, 40],
                 k = [1],
                 N = [500],
                 foresight = [false],
                 repeats = 5)

    g = grid(; p_0, p_brr, p_r, S, M, k, N, foresight, pop=1:repeats)
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

# %% ==================== learning ====================

g = grid(
    M=1:160,
    S=[5,10,20]
)

df = dataframe(g) do (;S, M)
    black = prob_observe(1 / (S^2), M)
    # prob observe one of the desired red edges (not both)
    red = prob_observe(1 / S, M)^2
    (;black, red)
end

@rput df

R"""
df %>%
    pivot_longer(c(red, black), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(M, value, color=name)) +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(x="M (number of demonstrations)", y="p(observe solution)") +
    facet_wrap(~S, nrow=1, labeller=label_glue('S = {S}',))

fig("p_observe", w=6, h=2.3)
"""
# %% --------

g = grid(
    M=[5, 25, 125],
    S=[5, 10, 20],
)

df = dataframe(g) do (;S, M)
    black = prob_observe(1 / (S^2), M)
    # prob observe one of the desired red edges (not both)
    red = prob_observe(1 / S, M)^2
    (;black, red)
end

@rput df

R"""
df %>%
    rename(idiosyncratic=black, compositional=red) %>%
    pivot_longer(c(idiosyncratic, compositional), names_to="name", values_to="value", names_prefix="") %>%
    fctrize(name, levels=c("idiosyncratic", "compositional")) %>%
    ggplot(aes(name, value, fill=name)) +
    scale_colour_manual(values=c(
        idiosyncratic=BLACK, compositional=RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_bar(stat="identity") +
    labs(x="", y="p(observe solution)") +
    no_xaxis_ticks +
    ybreaks(2) +
    facet_grid(S~M) +


fig("p_observe_bars", h=3, w=4)
"""

R"""
df %>%
    mutate(diff = red - black) %>%
    ggplot(aes(x=0, diff)) +
    geom_bar(stat="identity", fill=RED, width=.1) +
    labs(x="", y="p(observe solution)") +
    no_xaxis_ticks +
    ybreaks(2) +
    facet_grid(S~M) +


fig("p_observe_diff", h=3, w=4)
"""

# %% ==================== heatmap ====================

g = grid(
    M=2:1:100,
    S=2:1:100
)

df = dataframe(g) do (;S, M)
    black = prob_observe(1 / (S^2), M)
    red = prob_observe(1 / S, M)^2
    (;black, red)
end

@rput df

R"""
df %>%
    mutate(S = S^2) %>% filter(S < 101) %>%
    ggplot(aes(S, M, fill=red-black)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines +
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(expand=F, ylim=c(2,100)) +
    labs(fill="Compositional Learning Advantage\n", x="Environment Size (S)", y="Demonstrations (M)")

fig("advantage_heat_alt", w=4)

"""

# %% --------

R"""
plot_advantage = function(df, ...) {
    df %>%
        ggplot(aes(..., color=factor(S))) +
        geom_line() +
        scale_color_discrete_sequential("Purples", l1=20, l2=80, c2=40, name="# Tasks")
}
"""


R"""
df %>%
    mutate(S = S^2) %>% filter(S < 101) %>%
    plot_advantage(M, red-black)
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    # no_gridlines +
    # scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +

fig("advantage_lines_alt", w=4)

"""


R"""
indi %>%
    mutate(discovery=red_discovery, travel=red_travel) %>%
    mutate(S = S^2) %>% filter(S < 101) %>%
    plot_advantage(K, advantage) +
    facet_grid(travel~discovery) +
    geom_hline(yintercept=0)

fig("indi_advantage", w=8, h=7)

"""

# %% --------

g = grid(
    M=2:1:100,
    S=(2:10)
)
df = dataframe(g) do (;M, S)
    env = RedBlackEnv(;M, S, p_0=.01)
    red, gen = get_limit(env; max_gen=1000)
    [(;gen, red)]
end
@rput df

R"""
df %>%
    mutate(S = S^2) %>%
    plot_advantage(M, red)

fig("limit_lines", w=4.2)
"""
# %% --------

R"""
df %>%
    mutate(S = S^2) %>%
    ggplot(aes(S, M, fill=red)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    # geom_line(aes(y=n2, fill=NULL), df2, linewidth=.5, linetype="dashed") +
    no_gridlines +
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    labs(fill="Asymptotic Compositionality", x="Goals (S)", y="Social Observations (M)") +

fig("limit_heat", w=4.2)
"""


# %% ==================== M and S ====================

# run_sim_infinite(p_0=[1e-6], p_brr = [0.], p_r = [1])

run_sim_infinite(p_0=[1e-6], S=[4], M=[10])


R"""
df %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED) +
    ylab("Compositionality")

fig("basic_single", w=4, h=2)
"""

# %% --------

run_sim_infinite(p_0=[1e-6], S=[4], M=[2,8,32])

R"""
df %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED) +
    facet_wrap(~ M, labeller=label_glue('M={M}')) +
    # facet_grid(S ~ M, labeller=label_glue(cols='M={M}', rows='S={S}'))
    no_legend

fig("basic", w=5, h=2)
"""


# %% --------


g = grid(
    M=[2,8,32],
    S=[4]
)

learning = dataframe(g) do (;S, M)
    black = prob_observe(1 / (S^2), M)
    # prob observe one of the desired red edges (not both)
    red = prob_observe(1 / S, M)^2
    (;black, red)
end

R"""
learning %>%
    mutate(diff = red - black)
"""

# %% ==================== individual ====================

g = collect(grid(K=2:1:100, S=2:1:100, red_travel=[0., .05, .1, .2], red_discovery=[.5, .75, .95, 1]))
result = deserialize("tmp/individual")
indi = map(g, result) do g, r
    (;g..., r...)
end |> DataFrame

@rput indi
# %% --------

R"""
indi %>%
    mutate(discovery=red_discovery, travel=red_travel) %>%
    mutate(S = S^2) %>% filter(S < 101) %>%
    ggplot(aes(K, advantage, color=factor(S))) +
    geom_line() +
    facet_grid(travel~discovery)

fig("indi_advantage", w=8, h=7)

"""

# %% --------


R"""
indi %>%
    mutate(discovery=red_discovery, travel=red_travel) %>%
    ggplot(aes(S, K, fill=advantage)) +
    geom_tile() +
    no_gridlines +
    # scale_fill_continuous_sequential(h1=0, h2=NA, c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    scale_fill_continuous_diverging() +
    coord_cartesian(expand=F, ylim=c(2,100)) +
    labs(fill="Individual Compositional Advantage\n", x="Environment Size (S)", y="Number of Trials (K)") +
    facet_grid(travel~discovery)
fig(w=8, h=7)
"""

R"""
df %>%
    mutate(discovery=red_discovery, travel=red_travel) %>%
    mutate(S = S^2) %>% filter(S < 101) %>%
    ggplot(aes(S, K, fill=advantage > 0)) +
    geom_tile() +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_gridlines +
    # scale_fill_continuous_sequential(h1=0, h2=NA, c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    # scale_fill_continuous_diverging() +
    coord_cartesian(expand=F, ylim=c(2,100)) +
    facet_grid(travel~discovery)
    labs(fill="Individual Compositional Advantage\n", x="Environment Size (S)", y="Number of Trials (K)") +
fig(w=8, h=7)
"""