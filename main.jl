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
        map(enumerate(sim)) do (gen, compositionality)
            (;gen, compositionality)
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
            (;gen, compositionality = red_rate(pop))
        end
    end
    @rput df
    df
end

R"""
RED = "#E62A65"
TEAL = "#07A9C0"
FIGS_PATH = " ~/obsidian/Web/cultural-abstractions/figs/abstract/2024-07-12-"
MAKE_PDF = FALSE

cpal = scale_colour_manual(values=c(
    idiosyncratic=TEAL,
    compositional=RED,
    `-1` = TEAL,
    `1` = RED
), aesthetics=c("fill", "colour"), name="")

facet_grid = function(form, labeller=purrr::partial(label_both, sep = " = "), ...) {
    ggplot2::facet_grid(form, labeller=labeller, ...)
}

facet_wrap = function(form, labeller=purrr::partial(label_both, sep = " = "), ...) {
    ggplot2::facet_wrap(form, labeller=labeller, ...)
}

"""

# %% ==================== individual cost ====================

expected_unique(N, K) = N * (1 - (1 - 1/N)^K)

function individual_costs(;S, K, γ=1.)
    (;
        compositional = 2 * expected_unique(S, K),
        idiosyncratic = expected_unique(S^2, K),
    )
end
individual_costs(x::NamedTuple) = individual_costs(;x...)

indi_cost = dataframe(individual_costs, grid(S=1:100, K=1:100))
@rput indi_cost

indi_equality = map(3:100) do S
    res = optimize(1, S^2) do K
        a, b = individual_costs(S, K)
        abs(a - b)
    end
    K = res.minimizer
    @assert abs(res.minimum) < 1e-5
    return (;S, K)
end |> DataFrame
@rput indi_equality

@with indi_equality :K ./ :S

# %% --------

R"""
indi_cost %>%
    filter(S == 20) %>%
    pivot_longer(c(idiosyncratic, compositional), names_to="name", values_to="cost") %>%
    ggplot(aes(K, cost, color=name)) +
    geom_vline(xintercept=filter(indi_equality, S==20)$K, linewidth=.4) +
    geom_line() +
    cpal +
    labs(y="Discovery Cost") +
    top_legend

fig("indi_cost", h=3)
"""


# %% --------


R"""
library(ggrastr)
advantage_heat = list(
    rasterise(geom_tile(), dpi=500),
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    no_gridlines,
    labs(fill="Compositional Savings"),
    # scale_fill_continuous_diverging(h1=197, h2=10, c1=200, l1=20, l2=70, p1=1, p2=1, limits=c(-1, 1))
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=1.5, rev=F),
    coord_fixed(expand=F)
)

indi_cost %>%
    mutate(
        compositional = compositional,
        advantage = idiosyncratic - compositional,
        # advantage = 2 * (advantage > 0) - 1
    ) %>%
    ggplot(aes(K, S, fill=advantage)) +
    advantage_heat +
    geom_line(aes(fill=NaN), data=filter(indi_equality, K <= 100), linewidth=.4, linetype="dashed")


fig("indi_cost_heat", w=4)
"""

# %% ==================== discounting ====================


indi_discount = dataframe(grid(S=1:101, γ=[.95, .96, .97, .98, .99, 1.])) do (;S, γ)
    maxK = 1000
    x = map(invert(individual_costs.(S, 0:maxK))) do c
        y = diff(c) .* γ .^ (0:maxK-1)
        cumsum(y)
    end
    map(invert(x), 1:maxK) do t, K
        (;t..., K)
    end
end

@rput indi_discount

# %% --------

R"""
indi_discount %>%
    filter(S == 20, γ %in% c(.95, 1.), K<100) %>%
    pivot_longer(c(idiosyncratic, compositional), names_to="name", values_to="value") %>%
    ggplot(aes(K, value, color=name, alpha=factor(γ), linewidth=factor(γ))) +
    scale_alpha_manual(values=c(
        `1` = 0.2,
        `0.95` = 1
    )) +
    scale_linewidth_manual(values=c(
        `1` = 0.5,
        `0.95` = 1
    )) +
    coord_cartesian(xlim=c(NULL), ylim=c(0, 50)) +
    # scale_linetype_manual(values=c(
    #     `1` = "solid",
    #     `0.95` = "dashed"
    # )) +
    geom_line() +
    ylab("Discounted Cost") +
    cpal + no_legend

fig("indi_discount")
"""

R"""
indi_discount %>%
    tibble %>%
    filter(compositional < idiosyncratic) %>%
    filter(S == 20, γ %in% c(.95, 1.), K<100) %>%
    group_by(γ) %>%
    filter(K == min(K))
"""

R"""
discount_boundaries = indi_discount %>%
    tibble %>%
    filter(compositional < idiosyncratic) %>%
    group_by(K, γ) %>%
    filter(S == max(S), S < 101)

discount_boundaries %>%
    filter(K < 301) %>%
    ggplot(aes(K, S, color=factor(γ))) +
    no_gridlines +
    # coord_cartesian(expand=F) +
    coord_cartesian(expand=F) +
    geom_line(linewidth=.8) +
    scale_color_discrete_sequential("Purples", name="Discount Factor", l2=80, c2=20) +
    rev_legend

fig("indi_discount_boundaries")
"""

R"""
indi_discount %>%
    filter(γ %in% c(.95, .97, .99)) %>%
    ggplot(aes(K, S, fill=idiosyncratic - compositional)) +
    advantage_heat +
    geom_line(
        data=discount_boundaries %>% filter(γ %in% c(.95, .97, .99)),
        linewidth=.4, linetype="dashed",
    ) +
    # geom_line(aes(fill=NaN), data=filter(indi_equality, K <= 100), linewidth=.4, linetype="dashed") +
    facet_wrap(~γ) +
    labs(fill="Discounted Compositional Savings") +
    coord_cartesian()

fig("indi_discount_heat", w=7)
"""


# %% ==================== social cost ====================

@kwdef struct Costs
    black_travel::Float64 = 0.
    red_travel::Float64 = 0.
    black_discovery::Float64  = 1.
    red_discovery::Float64 = 1.
end

function social_costs(S, M; costs = Costs(), revert_to_idio=false)
    b = prob_observe(1 / (S^2), M)
    # prob observe ONE compositional edges (not both)
    r = prob_observe(1 / S, M)
    idiosyncratic = costs.black_travel + ¬b * costs.black_discovery
    compositional = costs.red_travel +
        r^2 * 0 +
        2 * r * ¬r * costs.red_discovery +
        ¬r * ¬r * 2 * costs.red_discovery
    if revert_to_idio
        compositional = min(compositional, idiosyncratic)
    end
    (;compositional, idiosyncratic)
end
social_costs((;S, M)) = social_costs(S, M)


social_cost = dataframe(social_costs, grid(M=1:100, S=1:100))
@rput social_cost


social_equality = map(1:100) do S
    res = optimize(1, S) do M
        a, b = social_costs(S, M)
        abs(a - b)
    end
    M = res.minimizer
    @assert abs(res.minimum) < 1e-5
    return (;S, M)
end |> DataFrame

@rput social_equality

@with social_equality :M ./ :S

# %% --------

R"""
S1 = 6
social_cost %>%
    filter(S==S1) %>%
    pivot_longer(c(compositional, idiosyncratic), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(M, value, color=name)) +
    geom_vline(xintercept=filter(social_equality, S==S1)$M, linewidth=.4) +
    geom_line() +
    cpal +
    no_legend +
    labs(y="discovery cost")

fig("social_cost")
"""


R"""
social_cost %>%
    # mutate(S = S^2) %>% filter(S < 101) %>%
    mutate(advantage = idiosyncratic - compositional) %>%
    ggplot(aes(M, S, fill=advantage)) +
    advantage_heat +
    geom_line(aes(fill=NaN), data=filter(social_equality, M <= 100), linewidth=.4, linetype="dashed")

fig("social_cost_heat")
"""

# %% ==================== dynamical systems analysis ====================


# %% --------


curve = dataframe(grid(p0=0:.001:1, S=6, M=25, p_r=0)) do (;p0, S, M, p_r)
    env = RedBlackEnv(;S, M, p_r)
    (;p0, p1=transition(env, p0))
end |> DataFrame

find_stable_points(S=6, M=25, p_r=0)
stable = DataFrame([find_stable_points(S=6, M=25, p_r=0)])
@rput curve stable

R"""



fig()

"""
R"""


    ggplot(aes(p0, p1)) +
    geom_line(
        size=.5,
        mapping=aes(group=segment),
        # data=filter(curve, p0 %in% c(0.3, 0.5, 0.7)),
        arrow = arrow(angle = 30, ends = "last", type = "open", length = unit(0.25, "inches")),
    )

fig()
"""


# %% --------

R"""

D = curve %>%
    mutate(
        delta=p1 - p0,
        segment = pmax(1, cumsum(replace_na(sign(delta) - lag(sign(delta)) != 0, FALSE)))
    ) %>%
    group_by(segment) %>%
    mutate(direction = sign(last(delta)))

ggplot(D) +
    geom_abline(linetype="solid", color=GRAY) +
    lapply(split(D, D$segment), function(df)
      geom_line(data = df, aes(p0, p1, color = factor(direction)),
                size=.8,
                arrow = arrow(
                     length = unit(.1, "in"), type = 'open',
                     ends = if (first(df$direction) < 0) 'first' else 'last',
                ))
    ) +
    no_gridlines +
    # gridlines +
    coord_fixed(expand=T) +
    # labs(x="previous compositionality rate", y="new compositionality rate") +
    labs(x="previous compositionality rate", y="new compositionality rate") +
    cpal + no_legend

fig("rate_rate", w=2.9)
"""

# %% --------

R"""
curve %>%
    mutate(delta = p1 - p0) %>%
    ggplot(aes(p0, p0+delta, color=delta, alpha=1)) +
    # geom_hline(yintercept=0) +
    geom_abline() +
    geom_line() +
    labs(x="compositionality rate", y="change in rate") +
    scale_color_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=.5, p2=.5) + no_legend

fig()
"""

# %% --------

df = dataframe(grid(p0=0:.001:1, S=10, M=30, p_r=[0, .125, .25, .5, 1])) do (;p0, S, M, p_r)
    env = RedBlackEnv(;S, M, p_r)
    (;p0, p1=transition(env, p0))
end |> DataFrame
@rput df


R"""
df %>%
    mutate(delta = p1 - p0) %>%
    ggplot(aes(p0, delta, color=delta)) +
    geom_line() +
    geom_hline(yintercept=0) +
    labs(x="compositionality rate", y="change in rate") +
    scale_color_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=.5, p2=.5) + no_legend +
    facet_wrap(~p_r, nrow=1) +
    xbreaks(3)

fig(w=6, )
"""


# %% --------

# %% --------

# df = dataframe(grid(S=3:50, M=1:50, p_r = 0:.2:1)) do (;S, M, p_r)
df = dataframe(grid(S=[10, 20], M=[40, 80], p_r = 0:.01:1)) do (;S, M, p_r)
    env = RedBlackEnv(;S, M, p_r)
    stable = find_zeros(0, 1) do x
        transition(env, x) - x
    end
    @assert stable[1] == 0.
    if length(stable) == 1
        @infiltry @assert transition(env, .5) < .5
        (;start=NaN, stop=NaN)
    elseif length(stable) == 2
        if transition(env, 1e-6) > 1e-6
            return (;start=0., stop=stable[2])
        else
            return (;start=NaN, stop=NaN)
        end
    elseif length(stable) == 3
        @assert transition(env, stable[2] - 1e-6) < stable[2] - 1e-6
        @assert transition(env, stable[2] + 1e-6) > stable[2] + 1e-6
        @assert transition(env, stable[3] + 1e-6) < stable[3] + 1e-6
        return (;start=stable[2], stop=stable[3])
    else
        @assert false
    end
end

@rput df
# %% --------

R"""
df %>%
    rename(min=start, max=stop) %>%
    pivot_longer(c(min, max), names_to="name", values_to="value") %>%
    ggplot(aes(p_r, value, color=name)) +
    geom_line() +
    facet_grid(M~S) +
    labs(x="Completion Rate", y="Compositionality", color="")

fig(h=4, w=7)
    # pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
"""

# %% --------



R"""
df %>%
    ggplot(aes(M, S, fill=start)) +
    rasterise(geom_tile(), dpi=500) +
    coord_fixed(expand=F) +
    no_gridlines +
    scale_fill_continuous_sequential(h1=350, h2=NA, c1=180, l1=20, l2=95, p1=1, p2=1.5,
            name="Compositionality Threshold", labels=scales::percent_format(), rev=F) +
    facet_wrap(~p_r)

fig(w=8, h=4)
"""

R"""
df %>%
    ggplot(aes(M, S, fill=stop)) +
    rasterise(geom_tile(), dpi=500) +
    coord_fixed(expand=F) +
    no_gridlines +
    scale_fill_continuous_sequential(h1=350, h2=NA, c1=180, l1=20, l2=95, p1=1, p2=1.5,
            name="Compositionality Asymptote", labels=scales::percent_format(), rev=T) +
    facet_wrap(~p_r)

fig(w=8, h=4)
"""


# %% --------

df = dataframe(grid(p0=0:.01:1, S=10, M=1:100)) do (;p0, S, M)
    env = RedBlackEnv(;S, M, p_r=0.)
    (;p0, p1=transition(env, p0))
end |> DataFrame
@rput df

R"""
df %>%
    mutate(
        delta = p1 - p0,
        # logdelta = if_else(delta == 0, 0, sign(delta) * log(abs(delta)))
        logdelta = sign(delta) * abs(delta) ^ (1/3)
    ) %>%
    # ggplot(aes(M, p0, fill=2*(delta > 0) - 1)) +
    ggplot(aes(p0, M, fill=logdelta)) +
    geom_tile() +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=1.5) + no_legend

fig()
"""


# %% ==================== evolution ====================

# run_sim_infinite(p_0=[1e-6], p_brr = [0.], p_r = [1])

run_sim_infinite(S=10, M=25)

R"""
df %>%
    ggplot(aes(gen, compositionality)) +
    geom_line(color=RED)

fig("basic_single")
"""

# %% ==================== limits ====================



asymptotic = dataframe(grid(M=1:1:100, S=1:100)) do (;M, S)
    env = RedBlackEnv(;M, S, p_0=.01, p_r=0.5, p_brr=0.)
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

fig("asymptotic")
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
    # filter(generation < 9) %>%
    ggplot(aes(generation, 1*compositionality, group=population)) +
    geom_line(linewidth=.5, color=RED, alpha=.3) +
    # geom_line(linewidth=.5, color="#BA1109", alpha=0.5) +
    geom_line(data=human) +
    facet_wrap(~M, labeller=label_glue("{M} Demos"), scales="free_y") +
    xbreaks() + ybreaks() +
    expand_limits(y=1.)


fig("experiment", w=7)
"""


# %% ==================== individual with memory ====================



run_sim_finite(;n_gen=30, p_0 = .01, repeats=10, N=30)

R"""
df %>%
    ggplot(aes(gen, compositional)) +
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

ggplot(d, aes(gen, compositional, color=name)) +
    geom_line(aes(group=interaction(pop, name)), filter(d, pop < 10), linewidth=.5, alpha=.5) +
    mean_line()

fig(w=6)
"""