include("utils.jl")

prob_observe(p::Float64, m::Int) = 1 - (1-p)^m

@kwdef struct Environment
    n::Int = 5  # number of starts and goals
    m::Int = 5  # number of models
    k::Int = 1  # number of tasks per agent
    ε::Float64 = .01  # lapse rate
    T::Matrix{Float64} = normalize!(ones(n, n))  # task distribution
end

¬(p::Real) = 1 - p

# ---------- track all edges separately ---------- #

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
        b = prob_observe(P[s, g], m)
        r1 = prob_observe(P[s, n+1], m)
        r2 = prob_observe(P[g, n+2], m)

        p_task = T[s, g]

        p_red = prob_learn_red(b, r1, r2; env.ε)
        p_black = 1 - p_red

        P′[s, g] += p_task * p_black
        P′[s, n+1] += p_task * p_red
        P′[g, n+2] += p_task * p_red
    end
    P′
end

# ---------- just track red frequency ---------- #

# behavior given observation probabilities
function prob_learn_red(b, r; ε)
    p = b * r * r * .5 + ¬b * (r * r + r * ¬r)
    ε * .5 + (1-ε) * p
end

function transition(env::Environment, p_red::Float64)
    (;n, m, ε, T, k) = env
    @assert k == 1
    # @assert all(T .≈ 1 / length(T))

    # prob observe a given black edge
    b = prob_observe(¬p_red / (n^2), m)
    # prob observe a given red edge (not a pair)
    r = prob_observe(p_red / n, m)

    prob_learn_red(b, r; ε)
end

function simulate(env::Environment, n_gen; state=zeros(env.n, env.n+2))
    res = [state]
    for i in 1:n_gen
        state = transition(env, state)
        push!(res, state)
    end
    res
end
