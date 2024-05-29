@everywhere include("black_red.jl")
@everywhere using NamedTupleTools
include("r.jl")

# %% ==================== setup ====================

function run_sim_infinite(;n_gen=30, init=NaN,
                 p_0 = [1e-6],
                 p_brr = [0.5],
                 p_r = [0.5],
                 n = [5, 10, 20],
                 m = 2 .* [5, 10, 20])

    g = grid(; p_0, p_brr, p_r, n, m)
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
FIGS_PATH = "figs/velez/"
MAKE_PDF = FALSE
DPI = 500
plot_red = function(...) {
    df %>%
        ggplot(aes(gen, red, ...)) +
        geom_line() +
        facet_grid(m ~ n, labeller=label_glue(cols='S={n}', rows='M={m}'))

}
"""



# %% ==================== m and n ====================


run_sim_infinite(p_0 = [.01], m=[10,40,80])

R"""
df %>%
    filter(n==10, m==20) %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED) +
    ylab("Compositionality")

fig("basic_single", w=4, h=2)
"""


R"""
plot_red(color="red") +
    scale_colour_manual(values=c(RED)) +
    no_legend

fig("basic", w=6)
"""

# %% ==================== neither ====================

run_sim_infinite(p_0 = .1 .^ (1:6))

R"""
plot_red(color=factor(p_0)) +
    discrete_sequential("Oranges", name="Spontaneous Compositionality Rate") +
    rev_legend +

fig("neither", w=7.5)
"""

# %% ==================== both ====================

run_sim_infinite(p_brr = 0:.2:1, m=10 .* [5, 10, 20])

R"""
plot_red(color=factor(p_brr)) +
    discrete_sequential("Purples", name="p(red|both)") +
    rev_legend

fig("both")
"""

# %% ==================== partial ====================

run_sim_infinite(p_r = 0:.2:1; n_gen=100)

R"""
plot_red(color=factor(p_r)) +
    discrete_sequential("TealGrn", name="Compositional Completion Rate") +
    rev_legend

fig("partial", w=7.5)
"""

# %% --------


simulate(Environment(p_0 = .01, p_r=1., p_brr=0., n=100, m=300), 100; init=1.)


# %% ==================== partial ====================


g = grid(
    p_0 = [1/1_000_000],
    p_brr = [0.],
    p_r = 0:.2:1,
    n = [5, 10, 20],
    m = 2 .* [5, 10, 20],
)

df = dataframe(g) do prm
    env = Environment(;prm...)
    sim = simulate(env, 100)
    map(enumerate(sim)) do (gen, red)
        (;gen, red)
    end
end
@rput df

R"""
df %>%
    ggplot(aes(gen, red, color=factor(p_r))) +
    # ggplot(aes(gen, red, color=factor(p_r))) +
    geom_line() +
    ylab("proportion using red") +
    expand_limits(y=0.4) +
    discrete_sequential("Greens", name="p(red|partial)") +
    rev_legend +
    facet_grid(m ~ n, labeller=label_glue(cols='S={n}', rows='M={m}')) +
fig("partial", w=5.5, h=4)
"""

# %% ==================== event horizon ====================

df = dataframe(grid(init=[.01, .02, .04, .08], p_r = [0, .01, .05, .1, .2])) do (;init, p_r)
    sim = simulate(Environment(;n=10, m=80, p_0=1e-6, p_r), 30; init)
    map(enumerate(sim)) do (gen, red)
        (;gen, red)
    end
end

@rput df

R"""
df %>% ggplot(aes(gen, red, color=factor(p_r))) +
    geom_line() +
    discrete_sequential("Greens", name="p(red|partial)") +
    ylab("proportion using red") +
    rev_legend +
    facet_wrap(~init, labeller=label_glue("init = {init}"), nrow=1)

fig("horizon", w=7, h=2)
"""

# %% ==================== m and n in limit ====================


g = grid(
    p_0 = [.001, .01, .1],
    m = 2 .^ (0:5),
    n = 1:32,
)

df = dataframe(g) do (;p_0, m, n)
    env = Environment(;p_0, m, n)
    red, gen = get_limit(env)
    [(;gen, red)]
end
@rput df

R"""
df %>%
    filter(p_0 == .001) %>%
    ggplot(aes(n, red, color=factor(m))) +
    geom_line() +
    ylab("proportion using red") +
    teals_pal(rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE))
fig("red_black_limits", w=5,)
"""



# %% ==================== finite populations ====================

function run_sim_finite(;n_gen=30, init=NaN,
                 p_0 = [0],
                 p_brr = [0.],
                 p_r = [1.],
                 n = [5, 10, 20],
                 m = 2 .* [5, 10, 20],
                 k = [1],
                 N = [500],
                 foresight = [false],
                 repeats = 5)

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

inf = run_sim_infinite(p_0 = [.01])
finite = run_sim_finite(N=[10000], p_0=[.01], repeats=5)

@rput finite inf

R"""
finite %>%
    ggplot(aes(gen, red)) +
    geom_line(data=inf, color="red", linewidth=1) +
    geom_line(mapping=aes(group=pop), linewidth=.5, alpha=0.5) +
    ylab("proportion using red") +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(m ~ n, labeller=label_glue(
        cols='n = {n}',
        rows='m = {m}'
    ))
fig("finite", w=5.5, h=4)
"""

# %% ==================== vary N ====================

run_sim_finite(N=[10,40,160,640], p_0=[.01], repeats=100)


R"""

df %>%
    ggplot(aes(gen, red, color=factor(N))) +
    geom_line(mapping=aes(group = interaction(N, pop)), data=filter(df, pop < 5), linewidth=.2, alpha=.7) +
    lines(mean, linewidth=.7) +
    ylab("Compositionality") +
    teals_pal(rev=T, name="Population Size") +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(m ~ n, labeller=label_glue(
        cols='S = {n}',
        rows='M = {m}'
    ))
fig("vary_N", w=7.5, h=4)
"""

# %% ==================== vary k ====================

df = run_sim_finite(N=[50], k=[1,4,16,64], p_0=[.01], repeats=100)

R"""
df %>%
    ggplot(aes(gen, red, color=factor(k))) +
    geom_line(mapping=aes(group = interaction(k, pop)), data=filter(df, pop < 5), linewidth=.2, alpha=.7) +
    lines(mean, linewidth=.7) +
    ylab("proportion using red") +
    discrete_sequential("Purple-Orange", rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(m ~ n, labeller=label_glue(
        cols='n = {n}',
        rows='m = {m}'
    ))
fig("vary_k", w=5.5, h=4)
"""

# %% ==================== foresight ====================

df = run_sim_finite(N=[50], k=[1,4,16,64], foresight=[true], p_r = [0.5], p_0 = [0.], repeats=100, n_gen=100)


R"""
df %>%
    ggplot(aes(gen, red, color=factor(k))) +
    geom_line(mapping=aes(group = interaction(k, pop)), data=filter(df, pop < 5), linewidth=.2, alpha=.7) +
    lines(mean, linewidth=.7) +
    ylab("proportion using red") +
    discrete_sequential("Purple-Orange", rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(m ~ n, labeller=label_glue(
        cols='n = {n}',
        rows='m = {m}'
    ))
fig("foresight", w=5.5, h=4)
"""

