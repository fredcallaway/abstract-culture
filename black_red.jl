include("utils.jl")
include("r.jl")

prob_learn(p::Float64, m::Int) = 1 - (1-p)^m

@kwdef struct Environment
    n::Int = 5  # number of starts and goals
    m::Int = 5  # number of models
    k::Int = 1  # number of tasks per agent
    ε::Float64 = .01  # lapse rate
    T::Matrix{Float64} = normalize!(ones(n, n))  # task distribution
end

¬(p::Real) = 1 - p

function prob_learn_red(b, r1, r2; ε)
    p_red =
         b *  r1 *  r2 * .5 +
         b *  r1 * ¬r2 * 0. +
         b * ¬r1 *  r2 * 0. +
         b * ¬r1 * ¬r2 * 0. +
        ¬b *  r1 *  r2 * 1. +
        ¬b *  r1 * ¬r2 * .5 +
        ¬b * ¬r1 *  r2 * .5 +
        ¬b * ¬r1 * ¬r2 * 0.
    ε * .5 + (1-ε) * p_red
end


function transition(env::Environment, P::Matrix)
    (;n, m, ε, T) = env
    P′ = zeros(n, n+2)
    for s in 1:n, g in 1:n
        b = prob_learn(P[s, g], m)
        r1 = prob_learn(P[s, n+1], m)
        r2 = prob_learn(P[g, n+2], m)

        p_task = T[s, g]

        p_red = prob_learn_red(b, r1, r2; env.ε)
        p_black = 1 - p_red

        P′[s, g] += p_task * p_black
        P′[s, n+1] += p_task * p_red
        P′[g, n+2] += p_task * p_red
    end
    P′
end

function simulate(env::Environment, n_gen; state=zeros(env.n, env.n+2))
    res = [state]
    for i in 1:n_gen
        state = transition(env, state)
        push!(res, state)
    end
    res
end

simulate(Environment(), 4)


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
