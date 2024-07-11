@everywhere include("red_black.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim

# %% ==================== setup ====================

function run_sim_infinite(;n_gen=30, init=NaN,
                 p_0 = 1e-6,
                 p_brr = 0.,
                 p_r = 1.,
                 S = 10,
                 M = 25
                 )

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

function run_sim_finite(;n_gen=30, init=NaN,
                 p_0 = 1e-6,
                 p_brr = 0.,
                 p_r = 1.,
                 S = 10,
                 M = 25,
                 K = 1,
                 N = 10,
                 repeats = 5
                 )

    g = grid(; p_0, p_brr, p_r, S, M, K, N, pop=1:repeats)
    df = dataframe(g; parallel=true) do prm
        env = RedBlackEnv(;delete(prm, :pop)...)
        sim = simulate(env, n_gen)
        map(enumerate(sim)) do (gen, pop)
            (;gen, red = red_rate(pop))
        end
    end
    @rput df
    df
end

R"""
RED = "#CB0900"
TEAL = "#07A9C0"
FIGS_PATH = "figs/paper/"
MAKE_PDF = TRUE

cpal = scale_colour_manual(values=c(
    idiosyncratic=TEAL,
    compositional=RED
), aesthetics=c("fill", "colour"), name="")

"""

# %% ==================== individual cost ====================

expected_unique(N, K) = N * (1 - (1 - 1/N)^K)


indi_cost = dataframe(grid(S=1:100, K=1:100)) do (;S, K)
    (;
        compositional = 2 * expected_unique(S, K),
        idiosyncratic = expected_unique(S^2, K),
    )
end
@rput indi_cost

# %% --------

R"""
indi_cost %>%
    filter(S == 5) %>%
    pivot_longer(c(idiosyncratic, compositional), names_to="name", values_to="value") %>%
    ggplot(aes(K, value, color=name)) +
    geom_line() +
    cpal +
    labs(y="Discovery Cost") +
    top_legend

fig("indi_cost", w=3)
"""

R"""
indi_cost %>%
    filter(S %in% c(5, 10, 20)) %>%
    pivot_longer(c(idiosyncratic, compositional), names_to="name", values_to="value") %>%
    ggplot(aes(K, value, color=name)) +
    geom_line() +
    cpal +
    facet_wrap(~S, scales="free_x") +
    labs(y="Discovery Cost") +
    no_legend

fig("indi_cost_panels", w=6)
"""

# %% --------

R"""
library(ggrastr)
advantage_heat = list(
    rasterise(geom_tile(), dpi=500),
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines,
    labs(fill="Relative Cost"),
    # scale_fill_continuous_diverging(h1=197, h2=10, c1=200, l1=20, l2=70, p1=1, p2=1, limits=c(-1, 1))
    scale_fill_continuous_diverging(h1=197, h2=10, c1=200, l1=20, l2=90, p1=1, p2=1, rev=T),
    coord_fixed(expand=F)
)

indi_cost %>%
    mutate(
        compositional = compositional,
        advantage = -(idiosyncratic - compositional),
        # advantage = 2 * (advantage > 0) - 1
    ) %>%
    ggplot(aes(K, S, fill=advantage)) +
    advantage_heat

fig("indi_cost_heat")
"""


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
library(ggrastr)
advantage_heat = list(
    rasterise(geom_tile(), dpi=500),
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines,
    labs(fill=""),
    scale_fill_continuous_diverging(h1=197, h2=10, c1=200, l1=20, l2=70, p1=1, p2=1, limits=c(-.85, .85)),
    coord_fixed(expand=F)
)

df %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    mutate(advantage = black - red) %>%
    ggplot(aes(M, S, fill=advantage)) +
    advantage_heat

    # labs(fill="Compositional Learning Advantage\n", x="Environment Size (S)", y="Demonstrations (M)")


fig("advantage_heat")
"""

# %% ==================== individual ====================

indi = deserialize("tmp/individual-jun14")
@rput indi


R"""
p = indi %>%
    filter(red_discovery == .9, red_travel == .05) %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    mutate(advantage = advantage / K) %>%
    ggplot(aes(K, S, fill=advantage)) +
    advantage_heat


fig("indi_advantage_heat")
"""

# %% --------

R"""

indi %>%
    filter(red_discovery == .9, red_travel == .05) %>%
    ggplot(aes(K, advantage / K, color=name)) +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(y="learning cost")

fig("learning_cost")
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

# %% ==================== limits ====================


g = grid(
    M=1:1:100,
    S=1:100
)
df = dataframe(g) do (;M, S)
    env = RedBlackEnv(;M, S, p_0=.01)
    red, gen = get_limit(env; max_gen=1000)
    [(;gen, red)]
end
@rput df

R"""
df %>%
    ggplot(aes(M, S, fill=red)) +
    advantage_heat +
    scale_fill_continuous_sequential(h1=10, h2=NA, c1=200,  l1=20, l2=70, p1=1, p2=1,
        labels=scales::percent_format(), limits=c(0,1)
    ) +
    theme(
        legend.key.height=unit(0.7, "inches")
    )
    # scale_fill_continuous_sequential(h1=10, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +

fig("asymptotic", w=8.5)
"""

# %% ==================== SM ====================


run_sim_infinite(S=[4, 20], M=[10, 30]; n_gen=30)

R"""
df %>%
    mutate(S = fct_rev(factor(S))) %>%
    rename(D = M) %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED) +
    # facet_grid(S ~ M, labeller=label_value) +
    facet_grid(S ~ D) +
    no_legend +
    # coord_cartesian(xlim=c(-1, 31), ylim=c(-.1, 1.1)) +
    theme(
        # strip.text.x = element_blank(),
        # strip.text.y = element_blank(),
        axis.line=element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 3)
    ) +
    no_xaxis_ticks + no_yaxis_ticks

fig("SM", w=WIDTH+1, h=HEIGHT+1)
"""

R"""

do_plot = function(...) {
    df %>%
        filter(...) %>%
        ggplot(aes(gen, red)) +
        geom_line(color=RED) +
        expand_limits(y=c(0,1)) +
        theme(
            # axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            # axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
        )
}

(do_plot(M==10, S==20) | do_plot(M==30, S==20)) /
(do_plot(M==10, S==4) | do_plot(M==30, S==4))

fig("SM2")
"""


# %% ==================== innovation ====================

run_sim_infinite(p_0 = .1 .^ (3:2:9))

R"""
df %>% ggplot(aes(gen, red, color=factor(p_0))) +
    geom_line() +
    scale_color_discrete_sequential("Oranges", name="Innovation Probability", l2=80, c2=40) +
    rev_legend +
    theme(legend.key.width=unit(0.6, "inches"))

fig("innovation", w=8.5)
"""


# %% ==================== completion ====================


run_sim_infinite(p_r = [1, .75, .5, .25], n_gen=30)

R"""
df %>% ggplot(aes(gen, red, color=factor(p_r))) +
    geom_line() +
    scale_color_discrete_sequential("TealGrn", name="Completion Probability") +
    rev_legend +
    theme(legend.key.width=unit(0.6, "inches"))

fig("completion", w=8.5)
"""

# %% ==================== experiment ====================

(;sim, tdf) = deserialize("tmp/experiment")
@rput sim tdf


R"""
human = tdf %>%
    filter(start != 5, goal != 5) %>%
    group_by(M, population, generation) %>%
    summarise(compositionality = mean(path_length == 2)) %>%
    mutate(agent = "human")

# df = bind_rows(mutate(sim, agent="model", population=100+population), human)

sim %>%
    filter(generation < 9) %>%
    ggplot(aes(generation, 1*compositionality, group=population)) +
    geom_line(linewidth=1, color="#BA1109", alpha=0.3) +
    # geom_line(linewidth=.5, color="#BA1109", alpha=0.5) +
    geom_line(data=human) +
    facet_wrap(~M, labeller=label_glue("{M} Demos"), scales="free_y") +
    expand_limits(y=1.)


fig("experiment", w=14,  h=5, pdf=T)
"""


# %% ==================== individual with memory ====================



run_sim_finite(;n_gen=30, p_0 = .01, repeats=10, N=30)

R"""
df %>%
    ggplot(aes(gen, red)) +
    geom_line(aes(group=pop), linewidth=.5, alpha=.5) +
    mean_line(color=RED)

fig()
"""

# %% --------
M = 80

social = run_sim_finite(;n_gen=30, p_r=0.5, p_0 = .005, repeats=100, N=M, K=1, M=M)
indiv = run_sim_finite(;n_gen=30, p_r=0.5, p_0 = .005, repeats=1000, N=1, K=M, M=M)
@rput social indiv

R"""
d = bind_named(social, indiv)

ggplot(d, aes(gen, red, color=name)) +
    geom_line(aes(group=interaction(pop, name)), filter(d, pop < 10), linewidth=.5, alpha=.5) +
    mean_line()

fig(w=6)
"""