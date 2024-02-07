using Memoize
include("utils.jl")
include("find_compositions.jl")

¬(p::Real) = 1 - p
prob_observe(p::Float64, m::Int) = ¬((¬p) ^ m)


@kwdef struct Environment
    n::Int = 5  # number of starts and goals
    m::Int = 5  # number of models
    k::Int = 1  # number of tasks per agent
    ε::Float64 = 0. # lapse rate
    T::Matrix{Float64} = normalize!(ones(n, n))  # task distribution
    foresight::Bool = false  # choose composition in advance?
    # red frequencies for ambiguous cases
    p_0::Float64 = 0.  # no info
    p_r::Float64 = 1.  # part red
    p_brr::Float64 = 0.  # full both

    # used to avoid memory allocation
    K::BitMatrix = falses(n, n+2)
end

@memoize function taskdist(T::Matrix{Float64})
    n = size(T, 1)
    map(CartesianIndices(T)[:]) do c
        s, g = c.I
        (s, g+n)
    end
    # SetSampler(get.(CartesianIndices(T), :I)[:], T[:])
end
taskdist(env::Environment) = taskdist(env.T)

struct Behavior
    s::Int
    g::Int
    red::Bool
end

function red_rate(pop::AbstractArray{<:Behavior})
    mean(get.(pop, :red))
end

function initial_population(env::Environment, N::Int)
    [Behavior(0, 0, false) for _ in 1:env.k, _ in 1:N]
end


function learn!(K, b::Behavior)
    (;s, g, red) = b
    n = size(K, 1)
    (s == 0 || g == 0) && return
    g = mod1(g, n)
    if red
        K[s, n+1] = true
        K[g, n+2] = true
    else
        K[s, g] = true
    end
end

function red_known(K, s)
    n = size(K, 1)
    K[mod1(s, n), n+1+div(s-1, n)]
end

red_known(K, s, g) = red_known(K, s) + red_known(K, g)

function black_known(K, s, g)
    n = size(K, 1)
    g = mod1(g, n)
    K[s, g]
end


function behave(env, tasks, observed)
    K = env.K
    fill!(K, false)
    for b in observed
        learn!(K, b)
    end
    if env.foresight
        error("unimplemented")
        # union!(known_red, find_compositions(env, tasks, known_red, known_black))
    end
    map(tasks) do (s, g)
        b = black_known(K, s, g)
        r = red_known(K, s, g)
        use_red = if b
            r == 2 ? rand() < env.p_brr : false
        else
            r == 2 ? true :
            r == 1 ? rand() < env.p_r :
            rand() < env.p_0
        end
        b = Behavior(s, g, use_red)
        learn!(K, b)
        b
    end
end

function transition(env::Environment, pop::Matrix{Behavior})
    (;n, m, T, k) = env
    N = size(pop, 2)

    pop1 = similar(pop)
    tasks = rand(taskdist(env), k, N)

    for agent in 1:N
        observed = sample(pop, m)
        pop1[:, agent] = behave(env, tasks[:, agent], observed)
    end
    pop1
end

# ---------- track all edges separately ---------- #

function prob_learn_red(b, r1, r2; ε)
    p_red =
         # b *  r1 *  r2 * .5 +
         # b *  r1 * ¬r2 * 0. +
         # b * ¬r1 *  r2 * 0. +
         # b * ¬r1 * ¬r2 * 0. +
        ¬b *  r1 *  r2 * 1. +
        ¬b *  r1 * ¬r2 * 1. +
        ¬b * ¬r1 *  r2 * 1. +
        # ¬b * ¬r1 * ¬r2 * 0. +
        0.
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
    p = ¬b * ¬(¬r * ¬r)
    # p = b * r * r * .5 + ¬b * (r * r + r * ¬r)
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
