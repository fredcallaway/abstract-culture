include("red_black.jl")
include("r.jl")
using Optim

R"""
FIGS_PATH = "figs/velez/"
MAKE_PDF = FALSE
DPI = 500
"""

# %% --------

g = grid(
    n=1:40,
    m=[1,2,4,8]
)

df = dataframe(g) do (;n, m)
    black = prob_observe(1 / (n^2), m)
    # prob observe one of the desired red edges (not both)
    red = prob_observe(1 / n, m)^2
    (;black, red)
end

@rput df


R"""

df %>%
    pivot_longer(c(red, black), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(n, value, color=name)) +
    scale_colour_manual(values=c(BLACK, RED ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(x="S (number of starts/goals)", y="p(observe my solution)") +
    facet_wrap(~m, nrow=1, labeller=label_glue('M = {m}',))

fig("p_observe", w=6, h=2)
"""

# %% --------

g = grid(
    m=1:160,
    n=[1,2,4,8]
)

df = dataframe(g) do (;n, m)
    black = prob_observe(1 / (n^2), m)
    # prob observe one of the desired red edges (not both)
    red = prob_observe(1 / n, m)^2
    (;black, red)
end

@rput df

R"""
df %>%
    pivot_longer(c(red, black), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(m, value, color=name)) +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(x="m (number of observations)", y="p(observe my solution)") +
    facet_wrap(~n, nrow=1, labeller=label_glue('S = {n}',))

fig("p_observe2", w=6, h=2)
"""
# %% --------

R"""
df %>%
    filter(n==8) %>%
    pivot_longer(c(red, black), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(m, value, color=name)) +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(x="M", y="p(observe)") +
    facet_wrap(~n, nrow=1, labeller=label_glue('S = {n}',))

fig("p_observe2_simple", w=2, h=2)
"""

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
    geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    geom_line(aes(y=n2, fill=NULL), df2, linewidth=.5, linetype="dashed") +
    no_gridlines +
    geom_abline() +
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1,p2=1) +
    coord_cartesian(xlim=c(0,500), ylim=c(0,5000), expand=F) +
    labs(fill="red advantage", x="n (number of starts/goals)", y="m (number of observations)", )

fig("advantage_heat", w=4, h=3)
"""


# %% --------

g = grid(
    n = [5, 10, 20],
    m = 2 .* [5, 10, 20],
    red_rate = 0:.1:1
)

df = dataframe(g) do (;n, m, red_rate)
    black = prob_observe((1-red_rate) / (n^2), m)
    # prob observe one of the desired red edges (not both)
    red = prob_observe(red_rate / n, m)^2
    either = 1 - (1 - black) * (1 - red)
    (;black, red, either)
end

@rput df

R"""
df %>%
    pivot_longer(c(black, red, either), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(red_rate, value, color=name)) +
    geom_line() +
    scale_colour_manual(values=c(BLACK, GRAY, RED), aesthetics=c("fill", "colour"), name="") +
    facet_grid(m ~ n, labeller=label_glue(cols='n={n}', rows='m={m}')) +
    scale_x_continuous(name="red frequency in previous generation", n.breaks=3) +
    scale_y_continuous(name="p(observe ...)", n.breaks=3) +
    gridlines

fig("p_observe_alt", w=6, h=4.5)
"""

