@everywhere include("red_black.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim
# %% --------

R"""
FIGS_PATH = "figs/spp2/"
MAKE_PDF = TRUE
DPI = 500
RED = "#C82506"
theme_set(theme_bw(base_size = 12))
theme_update(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.major.y = element_line(color="#EDEDED"),
    strip.background = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position="right",
    panel.spacing = unit(1, "lines"),
)


facet_grid = function(form, labeller=purrr::partial(label_both, sep = " = "), ...) {
    ggplot2::facet_grid(form, labeller=labeller, ...)
}
facet_S = facet_wrap(~S, nrow=1, labeller=label_glue('{S} Possible Tasks'))

"""

# %% ==================== setup ====================

function run_sim_infinite(;n_gen=30, init=NaN,
                 p_0 = 1e-6,
                 p_brr = 0.,
                 p_r = 1.,
                 S = 10,
                 M = 25)

    g = grid(; p_0, p_brr, p_r, S, M)
    df = dataframe(g) do prm
        env = RedBlackEnv(;prm...)
        sim = simulate(env, n_gen; init)
        map(enumerate(sim)) do (gen, red)
            (;gen, red)
        end
    end
    @rput df
    df
end

# %% ==================== learning ====================

g = grid(
    M=1:200,
    S=[5,10,20]
)

df = dataframe(g) do (;S, M)
    black = prob_observe(1 / (S^2), M)
    # prob observe both red edges
    red = prob_observe(1 / S, M)^2
    (;black, red)
end

@rput df


R"""
theme_set(theme_bw(base_size = 12))
theme_update(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.major.y = element_line(color="#EDEDED"),
    strip.background = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position="right",
    panel.spacing = unit(1, "lines"),
)

df %>%
    mutate(S = S**2) %>%
    pivot_longer(c(red, black), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(M, value, color=name)) +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(y="p(observe solution)") +
    facet_S

fig("p_observe", w=6, h=2.3, scale=.8)
"""

# %% ==================== learning advantage ====================

g = grid(
    M=2:1:100,
    S=2:10
)

df = dataframe(g) do (;S, M)
    black = prob_observe(1 / (S^2), M)
    red = prob_observe(1 / S, M)^2
    (;black, red)
end

@rput df

# %% --------

R"""
plot_advantage = function(df, ...) {
    df %>%
        ggplot(aes(..., color=fct_rev(factor(S)))) +
        geom_line() +
        scale_color_discrete_sequential("Purples", l1=20, l2=80, c2=40, name="# Tasks")
}

df %>%
    mutate(S = S^2, advantage = red-black) %>%
    plot_advantage(M, advantage)

    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    # no_gridlines +
    # scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +

fig("advantage_lines", w=4.5, h=3)
"""

# %% ==================== cost ====================

@kwdef struct Costs
    black_travel = 0.
    red_travel = 0.
    black_discovery = 1.
    red_discovery = 1.
end

costs = Costs()

g = grid(
    M=0:200,
    S=[5,10,20]
)

df = dataframe(g) do (;S, M)
    b = prob_observe(1 / (S^2), M)
    # prob observe ONE red edges (not both)
    r = prob_observe(1 / S, M)
    (;
        black = costs.black_travel + ¬b * costs.black_discovery,
        red = costs.red_travel +
            r^2 * 0 +
            2 * r * ¬r * costs.red_discovery +
            ¬r * ¬r * 2 * costs.red_discovery
    )
end

@rput df


R"""
df %>%
    mutate(S = S**2) %>%
    pivot_longer(c(red, black), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(M, value, color=name)) +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(y="learning cost") +
    facet_S

fig("learning_cost", w=6, h=2.3)
"""

R"""
df %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    ggplot(aes(S, M, fill=black-red)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines +
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format())
    # coord_cartesian(expand=F, ylim=c(2,100)) +
    # labs(fill="Compositional Learning Advantage\n", x="Environment Size (S)", y="Demonstrations (M)")

fig("advantage_heat_alt", w=4)
"""

# %% ==================== cost advantage ====================

g = grid(
    M=1:100,
    S=1:100,
)

df = dataframe(g) do (;S, M)
    b = prob_observe(1 / (S^2), M)
    # prob observe ONE red edges (not both)
    r = prob_observe(1 / S, M)
    (;
        black = costs.black_travel + ¬b * costs.black_discovery,
        red = costs.red_travel +
            r^2 * 0 +
            2 * r * ¬r * costs.red_discovery +
            ¬r * ¬r * 2costs.red_discovery
    )
end

@rput df

R"""

df %>%
    mutate(S = S^2, advantage = black-red) %>%
    plot_advantage(M, advantage)

fig("cost_advantage_heat", w=4.5, h=3)

"""

# %% --------

R"""

df %>%
    mutate(S = S^2, advantage = black-red) %>%
    plot_advantage(M, advantage)

fig("cost_advantage_lines", w=4.5, h=3)

"""

# %% ==================== individual ====================

indi = deserialize("tmp/individual-jun14")
@rput indi


R"""
indi %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    ggplot(aes(S, K, fill=advantage)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines +
    scale_fill_continuous_diverging() +
    facet_grid(red_discovery ~ red_travel)
    # coord_cartesian(expand=F, ylim=c(2,100)) +
    # labs(fill="Compositional Learning Advantage\n", x="Environment Size (S)", y="Demonstrations (M)")

fig("tmp", w=6, h=5)
"""

R"""
df %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    ggplot(aes(S, M, fill=black-red)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines +
    scale_fill_continuous_diverging()
    # coord_cartesian(expand=F, ylim=c(2,100)) +
    # labs(fill="Compositional Learning Advantage\n", x="Environment Size (S)", y="Demonstrations (M)")

fig("tmp")
"""

# %% --------


R"""
indi %>%
    # filter(K > 2) %>%
    # mutate(discovery=red_discovery, travel=red_travel) %>%
    filter(red_discovery == 0.95, red_travel == 0) %>%
    mutate(S = S^2) %>% filter(S < 101) %>%
    plot_advantage(K, advantage) +
    geom_hline(yintercept=0)
    # expand_limits(y=2)

fig("indi_advantage", w=4.5, h=3)
"""
# %% --------

R"""
indi %>%
    mutate(discovery=red_discovery, travel=red_travel) %>%
    filter(S < 11) %>%
    filter(discovery == 1., travel == 0) %>%
    filter(advantage > 0)
"""


R"""
indi %>%
    mutate(discovery=red_discovery, travel=red_travel) %>%
    mutate(S = S^2) %>% filter(S < 101) %>%
    mutate(advantage = advantage / K) %>%
    plot_advantage(K, advantage) +
    facet_grid(travel~discovery) +
    geom_hline(yintercept=0)

fig("indi_advantage", w=8, h=7)
"""


R"""
indi %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    ggplot(aes(S, M, fill=red-black)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines +
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(expand=F, ylim=c(2,100)) +
    labs(fill="Compositional Learning Advantage\n", x="Environment Size (S)", y="Demonstrations (M)")

fig("advantage_heat_alt", w=4)

"""


# %% ==================== evolution ====================

# run_sim_infinite(p_0=[1e-6], p_brr = [0.], p_r = [1])

run_sim_infinite(S=10, M=25)

R"""

df %>%
    ggplot(aes(gen, red))
fig("basic_empty", w=4.5, h=2.5)

df %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED)

fig("basic_single", w=4.5, h=2.5)
"""

# %% ====================  ====================

run_sim_infinite(S=[3, 5, 7], M=[3, 15, 75])

R"""
df %>%
    mutate(S = S**2) %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED) +
    facet_grid(M ~ S, labeller=label_glue(rows='{M} Demos', cols='{S} Tasks')) +
    no_legend

fig("SM", w=5, h=3.5)
"""

# %% ==================== limits ====================


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
    plot_advantage(M, red) +
    ylab("Asymptotic Compositionality")

fig("limit_lines", w=4.5, h=3)
"""

# %% ==================== innovation ====================

run_sim_infinite(p_0 = .1 .^ (3:9))

R"""
df %>% ggplot(aes(gen, red, color=factor(p_0))) +
    geom_line() +
    scale_color_discrete_sequential("Oranges", name="Innovation Rate", l2=80, c2=40) +
    rev_legend

fig("neither", w=4)
"""

# %% --------

run_sim_infinite(p_0 = .1 .^ (3:9), S=[3, 5, 7], M=[5, 10, 20], n_gen=100)

R"""
df %>% ggplot(aes(gen, red, color=factor(p_0))) +
    geom_line() +
    scale_color_discrete_sequential("Oranges", name="Innovation Rate", l2=80, c2=40) +
    rev_legend +
    facet_grid(M ~ S, labeller=label_glue(rows='{M} Demos', cols='{S} Tasks'))

fig("neither_facets", w=6, h=3.5)
"""

# %% ==================== completion ====================


run_sim_infinite(p_r = .5 .^ (0:3), n_gen=100)

R"""
df %>% ggplot(aes(gen, red, color=factor(p_r))) +
    scale_color_discrete_sequential("TealGrn", name="Completion Rate", rev=T, l2=80) +
    geom_line() +
    scale_color_discrete_sequential("TealGrn", name="Completion Rate") +
    rev_legend

fig("partial", w=4)
"""

# %% --------

run_sim_infinite(p_r = .5 .^ (0:3), S=[3, 5, 7], M=[5, 10, 20], n_gen=100)

R"""
df %>% ggplot(aes(gen, red, color=factor(p_r))) +
    geom_line() +
    scale_color_discrete_sequential("TealGrn", name="Completion Rate", rev=T, l2=80) +
    rev_legend +
    facet_grid(M ~ S, labeller=label_glue(rows='{M} Demos', cols='{S} Tasks')) +


fig("partial_facets", w=6, h=3.5)
"""

