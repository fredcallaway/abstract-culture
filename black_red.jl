include("utils.jl")
include("r.jl")

prob_learn(p::Float64, m::Int) = 1 - (1-p)^m

@kwdef struct Environment
    N::Int = 5
    m::Int = 5
    ε::Float64 = .01
    T::Matrix{Float64} = normalize!(ones(N, N))
end

function initial(env::Environment)
    (;N) = env
    zeros(N,N+2)
end

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
    (;N, m, ε, T) = env
    P′ = zeros(N, N+2)
    for s in 1:N, g in 1:N
        b = prob_learn(P[s, g], m)
        r1 = prob_learn(P[s, N+1], m)
        r2 = prob_learn(P[g, N+2], m)

        p_task = T[s, g]

        p_red = prob_learn_red(b, r1, r2; env.ε)
        p_black = 1 - p_red

        P′[s, g] += p_task * p_black
        P′[s, N+1] += p_task * p_red
        P′[g, N+2] += p_task * p_red
    end
    P′
end

function simulate(env::Environment, n_gen; P=initial(env))
    res = [P]
    for i in 1:n_gen
        P = transition(env, P)
        push!(res, P)
    end
    res
end


# %% ==================== one task ====================
g = grid(
    ε = [.05, .1, .2, .4]
)

df = dataframe(g) do prm
    env = Environment(;prm...)
    sim = simulate(env, 20)
    map(enumerate(sim)) do (gen, P)
        black = sum(P[1:N, 1:N])
        red = sum(P[1:N, N+1:end]) / 2
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
N = env.N

g = grid(
    ε = [.05, .1, .2, .4],
    m = [1, 5, 10],
    N = [5, 10, 20],
)

df = dataframe(g) do prm
    env = Environment(;prm...)
    sim = simulate(env, 20)
    map(enumerate(sim)) do (gen, P)
        black = sum(P[1:N, 1:N])
        red = sum(P[1:N, N+1:end]) / 2
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
    facet_grid(m ~ N, labeller=label_glue(
        cols='n={N}',
        rows='m={m}'
    ))
fig("red_black_grid", w=5, h=4)
"""
