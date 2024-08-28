@everywhere include("red_black.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim


function run_sim_infinite(;n_gen=30,
                 init=NaN,
                 p_0 = 1e-6,
                 p_brr = 0.,
                 p_r = 0.,
                 S = 10,
                 M = 25
                 )

    g = grid(; p_0, p_brr, p_r, S, M, init)
    df = dataframe(g) do prm

        env = RedBlackEnv(;delete(prm, :init)...)
        sim = simulate(env, n_gen; prm.init)
        map(enumerate(sim)) do (gen, compositionality)
            (;gen=gen-1, compositionality)
        end
    end
    @rput df
    df
end

R"""
advantage_heat = list(
    rasterise(geom_tile(), dpi=500),
    no_gridlines,
    coord_fixed(expand=F)
)
"""

# %% ==================== evolution ====================




# %% --------


# run_sim_infinite(p_0=[1e-6], p_brr = [0.], p_r = [1])

run_sim_infinite(S=10, M=300, p_0=.01)

R"""
df %>%
    ggplot(aes(gen, compositionality)) +
    geom_line(color=RED) + expand_limits(y=0)

fig("basic_single")
"""

# %% --------


run_sim_infinite(S=10, M=[5, 20, 200])

R"""
df %>%
    ggplot(aes(gen, compositionality)) +
    facet_wrap(~M) +
    geom_line(color=RED) +
    facet_wrap(~M, labeller=label_glue("{M} demos"))

fig("basic_varyM", w=6)
"""


# %% --------

run_sim_infinite(S=10, M=[40, 80, 160], init=[.04, .1])

R"""
df %>%
    ggplot(aes(gen, compositionality, color=factor(init))) +
    geom_line() + expand_limits(y=c(0, 1)) +
    scale_colour_manual(values=c(
        PINK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_point(data=filter(df, gen == 0)) +
    facet_wrap(~M, labeller=label_glue("{M} demos"))

fig("basic_varyMinit", w=6)
"""

# %% --------

run_sim_infinite(S = 20 .* [1, 2], M = 50 .* [1, 2, 4, 8, 16], init=[.01, .1], n_gen=20)

R"""
df %>%
    ggplot(aes(gen, compositionality, color=factor(init))) +
    geom_line() + expand_limits(y=c(0, 1)) +
    scale_colour_manual(values=c(
        PINK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_point(data=filter(df, gen == 0)) +
    facet_grid(S~M) +
    ybreaks(3) + xbreaks(3) + no_gridlines

fig("basic_varyMSinit", w=6, h=2.8)
"""

# %% --------


run_sim_infinite(S = 10 .* [1, 2, 4], M = 60 .* [1, 2, 4, 8], init=0.05)

R"""
df %>%
    ggplot(aes(gen, compositionality)) +
    geom_line(color=RED) + expand_limits(y=c(0, 1)) +
    no_legend +
    # geom_point(data=filter(df, gen == 0), color=RED) +
    facet_grid(S~M)

fig("basic_varyMS", w=6, h=4)
"""

# %% ==================== separate panels ====================

S = 10; M = 100; init = .05
run_sim_infinite(;S = 5 .* [1, 2, 3, 4], M, init)

R"""
plot_panel = function(df, var) {
    df %>%
        ggplot(aes(gen, compositionality, color=factor({{var}}))) +
        geom_line() +
        expand_limits(y=c(0, 1))
        # no_legend +
}

plot_panel(df, S) + scale_color_discrete_sequential(palette = "TealGrn", l1=40, l2=80, c2=40)
fig()
"""

# %% --------

run_sim_infinite(;S, M=50 .* [1,2,3,4], init)

R"""
plot_panel(df, M) + scale_color_discrete_sequential(palette = "TealGrn", l1=40, l2=80, c2=40)
fig()
"""




# %% ==================== limits ====================

asymptotic = dataframe(grid(M=1:1:100, S=1:100)) do (;M, S)
    env = RedBlackEnv(;M, S, p_0=1e-8, p_r=1, p_brr=0.)
    compositional, gen = get_limit(env; max_gen=1000)
    [(;gen, compositional)]
end
@rput asymptotic

R"""
asymptotic %>%
    ggplot(aes(M, S, fill=compositional)) +
    advantage_heat +
    scale_fill_continuous_sequential(h1=350, h2=NA, c1=180, l1=20, l2=95, p1=1, p2=1.5,
        name="Asymptotic Compositionality", labels=scales::percent_format())
    # scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=1.5, rev=F),
    # scale_fill_continuous_sequential(h1=10, h2=NA, c1=200,  l1=20, l2=70, p1=1, p2=1, limits=c(0,1))
    # scale_fill_continuous_sequential(h1=10, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +

fig("asymptotic", w=4)
"""

# %% ==================== SM ====================


run_sim_infinite(S=[4, 20], M=[10, 30]; n_gen=30)

R"""
df %>%
    mutate(S = fct_rev(factor(S))) %>%
    rename(D = M) %>%
    ggplot(aes(gen, compositional)) +
    geom_line(color=RED) +
    # facet_grid(S ~ M, labeller=label_value) +
    facet_grid(S ~ D) +
    no_legend +
    # coord_cartesian(xlim=c(-1, 31), ylim=c(-.1, 1.1)) +
    theme(
        # strip.text.x = element_blank(),
        # strip.text.y = element_blank(),
        axis.line=element_blank(),
        panel.border = element_rect(color = "idiosyncratic", fill = NA, size = 3)
    ) +
    no_xaxis_ticks + no_yaxis_ticks

fig("SM", w=WIDTH+1, h=HEIGHT+1)
"""

R"""

do_plot = function(...) {
    df %>%
        filter(...) %>%
        ggplot(aes(gen, compositional)) +
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
df %>% ggplot(aes(gen, compositionality, color=factor(p_0))) +
    geom_line() +
    scale_color_discrete_sequential("Oranges", name="Innovation Probability", l2=80, c2=40) +
    rev_legend

fig("innovation")
"""


# %% ==================== completion ====================


run_sim_infinite(p_r = [1, .75, .5, .25], n_gen=30)

R"""
df %>% ggplot(aes(gen, compositionality, color=factor(p_r))) +
    geom_line() +
    scale_color_discrete_sequential("TealGrn", name="Completion Probability") +
    rev_legend

fig("completion")
"""
