
# %% ==================== with replacement ====================

function expected_cost(env::RedBlackEnv)
    (;S, K) = env
   (;
       compositional = 2 * expected_unique(S, S, K),
       idiosyncratic = expected_unique(S^2, S^2, K)
   )
end

indi_cost = dataframe(grid(S=10, K=1:50)) do (;S, K)
    expected_cost(RedBlackEnv(;S, K))
end
@rput indi_cost


R"""
# library(ggbreak)

indi_cost %>%
    # filter(!between(K, 50, 100)) %>%
    pivot_longer(c(idiosyncratic, compositional), names_to="name", values_to="cost") %>%
    group_by(name) %>% mutate(cost = cost - lag(cost, default=0)) %>%
    ggplot(aes(K, cost, color=name)) +
    # geom_vline(xintercept=filter(indi_equality, S==20)$K, linewidth=.4) +
    geom_line() +
    # scale_x_break(c(50, 100)) +
    # ybreaks(6) +
    cpal +
    labs(x="# Tasks", y="Total Cost") +
    no_legend

fig("indi_cost", h=2, w=2.5)
"""

# %% --------

indi_cost = dataframe(grid(S=1:50, K=1:200)) do (;S, K)
    expected_cost(RedBlackEnv(;S, K))
end
@rput indi_cost

R"""
indi_cost %>%
    mutate(
        compositional = compositional,
        advantage = (idiosyncratic - compositional) / K,
        # advantage = 2 * (advantage > 0) - 1
    ) %>%
    ggplot(aes(K, S, fill=advantage)) +
    heatmap() +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=1.5, rev=F, name="compositional advantage")
    # geom_line(aes(fill=NaN), data=filter(indi_equality, K <= 100), linewidth=.4, linetype="dashed")


fig("indi_cost_heat", w=4)
"""

# %% ==================== cost without replacement ====================

function expected_unique_nodes(;S, K)
    env = RedBlackEnv(;S, K)
    tasks = all_tasks(env)
    monte_carlo(100_000) do
        sub = sample(tasks, K; replace=false)
        length(unique(first, sub)) + length(unique(last, sub))
    end
end

df = dataframe(grid(S=10, K=1:100)) do
    (
        compositional = expected_unique_nodes(;S, K),
        idiosyncratic =
    )

    (;cost =
end

# %% --------

monte_carlo() do
    tasks = sample(all_tasks(env),env. K; replace=false)
    length(unique(first, tasks)) + length(unique(last, tasks))
end

individual_costs(;S=5, K=5)

# %% --------


R"""
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

function social_costs(S, D; costs = Costs(), revert_to_idio=false)
    b = prob_observe(1 / (S^2), D)
    # prob observe ONE compositional edges (not both)
    r = prob_observe(1 / S, D)
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
social_costs((;S, D)) = social_costs(S, D)


social_cost = dataframe(social_costs, grid(D=1:100, S=1:100))
@rput social_cost


social_equality = map(1:100) do S
    res = optimize(1, S) do D
        a, b = social_costs(S, D)
        abs(a - b)
    end
    D = res.minimizer
    @assert abs(res.minimum) < 1e-5
    return (;S, D)
end |> DataFrame

@rput social_equality

@with social_equality :D ./ :S

# %% --------

R"""
S1 = 6
social_cost %>%
    filter(S==S1) %>%
    pivot_longer(c(compositional, idiosyncratic), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(D, value, color=name)) +
    geom_vline(xintercept=filter(social_equality, S==S1)$D, linewidth=.4) +
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
    ggplot(aes(D, S, fill=advantage)) +
    advantage_heat +
    geom_line(aes(fill=NaN), data=filter(social_equality, D <= 100), linewidth=.4, linetype="dashed")

fig("social_cost_heat")
"""