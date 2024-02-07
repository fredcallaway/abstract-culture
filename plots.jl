include("black_red.jl")
include("r.jl")

# %% --------

g = grid(
    # ε = [.05, .1, .2, .4],
    k = [1, 5, 9, 13],
    m = 1:5,
    n = 1:5,
)
g = grid(
    # ε = [.05, .1, .2, .4],
    k = [1, 5, 9, 13],
    m = [1, 5, 10, 20],
    n = [1, 5, 10, 20],
)

df = dataframe(g) do prm
    env = Environment(;prm..., ε=0., p_0=.001)
    pop = initial_population(env, 100)
    sim = simulate(env, 100; state=pop)
    map(enumerate(sim)) do (gen, pop)
        (;gen, red = mean(get.(pop, :red)))
    end
end
@rput df

R"""
df %>%
    filter(gen > 1) %>%
    ggplot(aes(gen, red, color=factor(k))) +
    geom_line() +
    ylab("proportion using red") +
    teals_pal(rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(m ~ n, labeller=label_glue(
        cols='n = {n}',
        rows='m = {m}'
    ))
fig("red_black_k", w=7, h=4.5)
"""

# %% ==================== try seeding ====================
g = grid(
    # ε = [.05, .1, .2, .4],
    k = [1],
    m = 1:5,
    n = 1:5,
    init_red = 0:.2:1
)

df = dataframe(g) do (;k, m, n, init_red)
    env = Environment(;k, m, n, ε=0.)
    pop = initial_population(env, 100)
    pop = map(transition(env, pop)) do beh
        mutate(beh, red = rand() < init_red)
    end
    sim = simulate(env, 100; state=pop)
    map(enumerate(sim)) do (gen, pop)
        (;gen, red = mean(get.(pop, :red)))
    end
end
@rput df


R"""
df %>%
    filter(gen > 1) %>%
    ggplot(aes(gen, red, color=factor(init_red))) +
    geom_line() +
    ylab("proportion using red") +
    teals_pal(rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(m ~ n, labeller=label_glue(
        cols='n = {n}',
        rows='m = {m}'
    ))
fig("red_black_red", w=7, h=4.5)
"""


# %% ==================== one task ====================
g = grid(
    ε = [.05, .1, .2, .4]
)

df = dataframe(g) do prm
    env = Environment(;prm...)
    n = env.n
    sim = simulate(env, 20)
    map(enumerate(sim)) do (gen, P)
        black = sum(P[1:n, 1:n])
        red = sum(P[1:n, n+1:end]) / 2
        (;gen, black, red)
    end
end
@rput df

R"""
df %>%
    filter(gen > 1) %>%
    ggplot(aes(gen, red, color=factor(ε))) +
    geom_line() +
    ylab("proportion using red") +
    teals_pal(rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE))
fig("red_black1")
"""

# %% ==================== m and n ====================

env = Environment()
n = env.n

g = grid(
    ε = [.05, .1, .2, .4],
    m = [1, 5, 10],
    n = [5, 10, 20],
)

df = dataframe(g) do prm
    env = Environment(;prm...)
    sim = simulate(env, 20)
    map(enumerate(sim)) do (gen, P)
        black = sum(P[1:n, 1:n])
        red = sum(P[1:n, n+1:end]) / 2
        (;gen, black, red)
    end
end
@rput df

R"""
df %>%
    filter(gen > 1) %>%
    ggplot(aes(gen, red, color=factor(ε))) +
    geom_line() +
    ylab("proportion using red") +
    teals_pal(rev=T) +
    expand_limits(y=0.4) +
    guides(color = guide_legend(reverse=TRUE)) +
    facet_grid(m ~ n, labeller=label_glue(
        cols='n={n}',
        rows='m={m}'
    ))
fig("red_black_grid", w=5, h=4)
"""
