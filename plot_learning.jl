include("red_black.jl")
include("r.jl")
using Optim

R"""
FIGS_PATH = "figs/velez/"
MAKE_PDF = FALSE
DPI = 500
"""

# %% ==================== solution based dynamics ====================




p_red = .2
(;S, D) = env
r = prob_observe(p_red * (2S - 1) / S^2, D)
b = prob_observe(¬p_red / S^2, D)

r * ¬b  # WRONG: ignores dependence


expectation(Binomial(D, p_red)) do n_comp_demo
    b = prob_observe(1 / (S^2), D - n_comp_demo)
    r = prob_observe((2S-1) / S^2, n_comp_demo)
    r * ¬b
    # prob_learn_red(env, b, r)
end

C = p_red .* normalize(ones(S, S))
B = ¬p_red .* normalize(ones(S, S))
P = cat(C, B; dims=3)

C

dist = Multinomial(D, P[:])
p = monte_carlo() do
    x = rand(dist)
    x = reshape(x, size(P))
    (sum(x[1, :, 1]) > 0 || sum(x[:, 1, 1]) > 0) && (x[S^2 + 1] == 0)
end





# %% --------
i = 1; j = 1
S = 20
D = 30

env = RedBlackEnv(;S, D, p_r=1.)
@show transition(env, .2)  # 0.5038308899803658

p_c = p_red * (2S - 1) / S^2
p_b = (1 - p_red) / S^2

(1 - p_b)^D * (
    1 - (1 - (p_c / (1 - p_b))) ^ D
    # 1 - (1 - p_c / (1 - p_b)) ^ D
)


# %% --------

x = 1:10

mean(x .<= 3)
mean(x .>= 7)

# %% --------

k = 2

# %% --------

1 - (
    (1 - .3) ^ k +
    (1 - .4) ^ k -
    (1 - .3 - .4) ^ k
)
# %% --------

k = 3

monte_carlo(1000000) do
    x = rand(1:10, k)
    any(i->i <= 3, x) && any(i->i >= 7, x)
end



# %% ==================== new learning ====================


df = dataframe(grid(S=[5, 10, 20])) do (;S)
    map([1:10S; 10S:S:3_000]) do D
        p_red = .5
        p_c = p_red * (2S - 1) / S^2
        p_b = (1 - p_red) / S^2

        both = 1 - ((1 - p_c)^D + (1 - p_b)^D - (1 - p_c - p_b)^D)
        compositional = (1 - p_b)^D * (
            1 - (1 - (p_c / (1 - p_b))) ^ D
            # 1 - (1 - p_c / (1 - p_b)) ^ D
        )
        neither = (1 - p_c - p_b) ^ D
        idiosyncratic = (1 - both - compositional - neither)

        (;D, both, compositional, neither, idiosyncratic)
    end
end
@rput df


R"""
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

df %>%
    # filter(D < 1000) %>%
    mutate(facet = fct_case_when(
        S == 5 ~ "Small Env (S=5)",
        S == 10 ~ "Medium Env (S=10)",
        S == 20 ~ "Large Env (S=20)",
        TRUE ~ glue("S = {S})"
    ))) %>%
    pivot_longer(c(both, compositional, neither, idiosyncratic), names_to="name", values_to="value", names_prefix="") %>%
    mutate(name = factor(name, ordered=T, levels=c("neither", "idiosyncratic", "compositional", "both"))) %>%
    # mutate(name = factor(name, ordered=T, levels=c("both", "compositional", "idiosyncratic", "neither"))) %>%
    ggplot(aes(D, value, fill=name)) +
    # no_legend +
    geom_area(position = "stack") +
    scale_colour_manual(aesthetics=c("fill", "colour"), name="", values=c(
        both="#318895", compositional=RED, idiosyncratic=TEAL, neither="#66D6E6"
    )) +
    # geom_line() +
    # labs(x="S (number of starts/goals)", y="p(observe my solution)") +
    scale_x_log10(labels=scales::comma) +
    coord_cartesian(expand=F) +
    # scale_y_log10(labels=scales::comma) +
    facet_wrap(~facet, nrow=1, labeller=identity) +
    gridlines +
    labs(x="Number of demonstrations", y="observation probability")
    theme()

fig("p_observe_alt", w=8)
"""

# %% --------
df = dataframe(grid(S=2:100)) do (;S)
    idiosyncratic = optimize(0, 100 * S^2) do D
        idiosyncratic = prob_observe(1 / (S^2), D)
        abs(idiosyncratic - 0.5)
    end
    compositional = optimize(0, 100 * S) do D
        r = prob_observe(1 / S, D)
        compositional = r ^ 2
        abs(compositional - 0.5)
    end
    map(Optim.minimizer, (;idiosyncratic, compositional))
end
@rput df


R"""
df %>%
    # filter(S < 21) %>%
    pivot_longer(c(idiosyncratic, compositional), names_to="name", values_to="value") %>%
    # filter(name == "idiosyncratic") %>%
    ggplot(aes(S, value, color=name)) +
    cpal + no_legend +
    geom_line() +
    # geom_line(mapping=aes(y = 1.2 * S), color="black", linetype="dotted", linewidth=.5) +
    # geom_line(mapping=aes(y = 0.7 * S^2), color="black", linetype="dotted", linewidth=.5) +
    geom_abline() +
    gridlines +
    scale_x_log10() +
    scale_y_log10() +
    ylab("Demonstrations for\n 50% Observation Prob")
fig("necessary")
"""




# %% ==================== old ====================


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

