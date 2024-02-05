include("black_red.jl")
include("r.jl")

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
