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
    # prob observe one of the desired red edges (not both)
    red = prob_observe(1 / S, M)^2
    (;black, red)
end

@rput df

R"""
facet_S = facet_wrap(~S, nrow=1, labeller=label_glue('{S} Possible Tasks'))
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

fig("p_observe", w=6, h=2.3)
"""

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
    labs(y="p(observe solution)") +
    facet_S

fig("p_observe_single", w=2.3, h=2.3)
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

R"""
df %>%
    mutate(S = S^2, advantage = red-black) %>%
    plot_advantage(M, advantage)
"""

# %% ==================== individual ====================

g = collect(grid(K=2:1:100, S=2:1:100, red_travel=[0., .05, .1, .2], red_discovery=[.5, .75, .95, 1]))
result = deserialize("tmp/individual")
indi = map(g, result) do g, r
    (;g..., r...)
end |> DataFrame

@rput indi

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
    filter(K > 2) %>%
    mutate(discovery=red_discovery, travel=red_travel) %>%
    filter(discovery == 0.95, travel == 0) %>%
    mutate(S = S^2) %>% filter(S < 101) %>%
    mutate(advantage = advantage / K) %>%
    plot_advantage(K, advantage) +
    geom_hline(yintercept=0)
    # expand_limits(y=2)

fig("indi_single", w=4.5, h=3)

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

