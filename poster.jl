@everywhere include("red_black.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim
# %% --------

R"""
FIGS_PATH = "spp-poster/"
MAKE_PDF = TRUE
DPI = 500
WIDTH = 5.2
HEIGHT = 5
RED = "#C82506"
library(ggrastr)
theme_set(theme_classic(base_size = 32))
theme_update(
    text = element_text(family = "Helvetica"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    # panel.border = element_rect(color = "black", fill = NA, size = 3),
    panel.grid.major.y = element_blank(),
    # panel.grid.major.y = element_line(color="#EDEDED"),
    strip.background = element_blank(),
    strip.text.x = element_text(size=32),
    strip.text.y = element_text(size=32),
    legend.position="right",
    panel.spacing = unit(1, "lines"),
)

theme_update(
    axis.title.x = element_text_transform(transform = fancy_name),
    axis.title.y = element_text_transform(transform = fancy_name),
    # legend.text = element_text(),
    legend.title = element_text_transform(transform = fancy_name_compact),
    strip.text = element_text_transform(transform = fancy_name)
)

update_geom_defaults("line", list(linewidth = 2.5))
update_geom_defaults("pointrange", list(size=.3))

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

# %% ==================== cost ====================
@kwdef struct Costs
    black_travel::Float64 = 0.
    red_travel::Float64 = 0.1
    black_discovery::Float64 = 1.
    red_discovery::Float64 = 1.
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
    filter(S==10) %>%
    pivot_longer(c(red, black), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(M, value, color=name)) +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(y="learning cost")

fig("learning_cost")
"""


# %% ==================== cost advantage ====================

@kwdef struct Costs
    black_travel::Float64 = 0.
    red_travel::Float64 = 0.05
    black_discovery::Float64 = 1.
    red_discovery::Float64 = .9
end

costs = Costs()

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
theme_update(
    legend.key.width=unit(0.5, "inches"),
    legend.key.height=unit(0.9, "inches"),
)

advantage_heat = list(
    rasterise(geom_tile(), dpi=500),
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines,
    labs(fill=""),
    scale_fill_continuous_diverging(h1=197, h2=10, c1=200, l1=20, l2=70, p1=1, p2=1, limits=c(-.85, .85)),
    coord_fixed(expand=F),
    theme(axis.line = element_blank())
)

df %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    mutate(advantage = black - red) %>%
    ggplot(aes(M, S, fill=advantage)) +
    advantage_heat

    # labs(fill="Compositional Learning Advantage\n", x="Environment Size (S)", y="Demonstrations (M)")


fig("advantage_heat", w=9, h=6)
"""

# %% ==================== individual ====================

indi = deserialize("tmp/individual-jun14")
@rput indi


R"""
indi %>%
    filter(red_discovery == .9, red_travel == .05) %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    mutate(advantage = advantage / K) %>%
    ggplot(aes(K, S, fill=advantage)) +
    advantage_heat


fig("indi_advantage_heat", w=9, h=6)
"""

# %% ==================== evolution ====================

# run_sim_infinite(p_0=[1e-6], p_brr = [0.], p_r = [1])

run_sim_infinite(S=10, M=25)

R"""

df %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED)

fig("basic_single", w=7.5)
"""

# %% ====================  ====================

run_sim_infinite(S=[3, 5, 7], M=[3, 15, 75])

R"""
df %>%
    mutate(S = S**2) %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED) +
    facet_grid(M ~ S) +
    no_legend +
    theme(
        panel.border = element_rect(color = "black", fill = NA, size = 3)
    ) +
    no_axes

fig("SM", w=10, h=10)
"""

# %% ==================== limits ====================


g = grid(
    M=1:1:100,
    S=1:100
)
df = dataframe(g) do (;M, S)
    env = RedBlackEnv(;M, S, p_0=.01)
    env = RedBlackEnv(M=10, S=5)
    get_limit(env; max_gen=1000)
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

# %% ==================== limit heatmap ====================

g = grid(
    M=2:1:100,
    S=(2:100)
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

