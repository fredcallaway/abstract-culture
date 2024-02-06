using Memoize
include("utils.jl")

¬(p::Real) = 1 - p
prob_observe(p::Float64, m::Int) = ¬((¬p) ^ m)


@kwdef struct Environment
    n::Int = 5  # number of starts and goals
    m::Int = 5  # number of models
    k::Int = 1  # number of tasks per agent
    ε::Float64 = .01  # lapse rate
    T::Matrix{Float64} = normalize!(ones(n, n))  # task distribution
end

@memoize function taskdist(T::Matrix{Float64})
    SetSampler(get.(CartesianIndices(T), :I)[:], T[:])
end
taskdist(env::Environment) = taskdist(env.T)


struct Behavior
    s::Int
    g::Int
    red::Bool
end

function initial_population(env::Environment, N::Int)
    [Behavior(0, 0, false) for _ in 1:env.k, _ in 1:N]
end

function behave(tasks, observed; ε)
    @assert length(tasks) == 1
    s, g = tasks[1]
    if rand() < ε
        return [Behavior(s, g, rand((true, false)))]
    end

    black_cost = Behavior(s, g, false) in observed ? 0 : 1

    red_s = any(observed) do beh
        beh.red && beh.s == s
    end
    red_g = any(observed) do beh
        beh.red && beh.g == g
    end
    red_cost = 2 - (red_s + red_g)

    red =
        red_cost < black_cost ? true :
        red_cost > black_cost ? false :
        rand((true, false))

    [Behavior(s, g, red)]
end


function transition(env::Environment, pop::Matrix{Behavior})
    (;n, m, ε, T, k) = env
    N = length(pop)

    pop1 = similar(pop)
    tasks = rand(taskdist(env), k, N)

    for agent in 1:length(pop)
        observed = sample(pop, m)
        pop1[:, agent] = behave(tasks[:, agent], observed; ε)
    end
    pop1
end

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


function transition(env::Environment, P::Matrix{Float64})
    (;n, m, ε, T) = env
    P′ = zeros(n, n+2)
    for s in 1:n, g in 1:n
        b = prob_observe(P[s, g], m)
        r1 = prob_observe(P[s, n+1], m)
        r2 = prob_observe(P[g, n+2], m)

        p_task = T[s, g]

        p_red = prob_learn_red(b, r1, r2; ε)
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

    # prob observe the desired black edge
    b = prob_observe(¬p_red / (n^2), m)
    # prob observe one of the desired red edges (not both)
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
