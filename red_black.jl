using Roots
using Memoize
using StaticArrays
include("utils.jl")

¬(p::Real) = 1 - p
prob_observe(p, k) = ¬((¬p) ^ k)

mutable struct Knowledge
    black::BitMatrix
    red_a::BitVector
    red_b::BitVector
    _n_solved::Int
end

Knowledge(S::Int) = Knowledge(falses(S, S), falses(S), falses(S), 0)

abstract type AgentPolicy end

struct TabularPolicy <: AgentPolicy
    table::SMatrix{2, 4, Float64} # p(red) for bespoke (no/yes) × compositional (none, partial, full, exact)
end

@kwdef struct RedBlackEnv
    S::Int = 5  # number of starts and goals
    D::Int = 5  # number of demonstrations
    K::Int = 1  # number of tasks per agent
    N::Real = Inf # population size
    agent_policy::TabularPolicy = classic_policy()

    replace_tasks::Bool = true  # sample tasks with replacement?
    replace_demos::Bool = true  # sample demonstrations with replacement?

    # pre-allocate for efficiency
    _all_tasks::Vector{Tuple{Int, Int}} = collect(Iterators.product(1:S, 1:S))[:]
    _sampled_tasks::Vector{Tuple{Int, Int}} = fill((0, 0), K)
    _knowledge::Knowledge = Knowledge(S)
end

all_tasks(env::RedBlackEnv) = env._all_tasks
sample_tasks(env::RedBlackEnv) = sample!(env._all_tasks, env._sampled_tasks)

struct Behavior
    a::Int
    b::Int
    red::Bool
end

Population = Matrix{<:Behavior}

function red_rate(pop::Population)
    mean(get.(pop, :red))
end

function initial_population(env::RedBlackEnv, init::Nothing=nothing)
    [Behavior(0, 0, false) for _ in 1:env.K, _ in 1:env.N]
end

function initial_population(env::RedBlackEnv, init::Float64)
    (;S, N, K) = env
    compositional = falses(N * K)
    for i in sample(eachindex(compositional), Int(init * N * K); replace=false)
        compositional[i] = true
    end
    X = map(compositional) do red
        Behavior(rand(1:S), rand(1:S), red)
    end
    reshape(X, (K, N))
end

initial_population(env::RedBlackEnv, init::Population) = init

# %% ==================== learning ====================

# get it while it's hot!
function fresh_knowledge(env::RedBlackEnv)
    k = env._knowledge
    k.black .= false
    k.red_a .= false
    k.red_b .= false
    k._n_solved = 0
    k
end

function learn!(env::RedBlackEnv, knowledge::Knowledge, demo::Behavior)
    @assert env.agent_policy isa TabularPolicy  # see old_red_black.jl
    (;red_a, red_b, black) = knowledge
    (;a, b, red) = demo
    (a == 0 || b == 0) && return  # dummy observation
    if red
        red_a[a] = true
        red_b[b] = true
    else
        black[a, b] = true
    end
end

function social_learning!(env::RedBlackEnv, knowledge::Knowledge, pop)
    (;D, replace_demos) = env
    demos = D == -1 ? pop : sample(pop, D; replace=replace_demos)
    for d in demos
        learn!(env, knowledge, d)
    end
end

# %% ==================== acting ====================

classic_policy(;p_0=0.0, p_r=0.0, p_brr=0.0, ε=0.0) = TabularPolicy([
    p_0 p_r 1-ε 1-ε
    ε ε p_brr p_brr
])

function behave(env::RedBlackEnv, knowledge::Knowledge, a, b, trials_remaining)
    # ignore trials_remaining for now (see old_red_black.jl)
    use_red = rand(Bernoulli(prob_red(env, knowledge, a, b)))
    Behavior(a, b, use_red)
end

function prob_red(env::RedBlackEnv, knowledge::Knowledge, a, b)
    red_known = knowledge.red_a[a] + knowledge.red_b[b]
    black_known = knowledge.black[a,b]
    env.agent_policy.table[black_known+1, red_known+1]
end

# %% ==================== evolution ====================

function transition(env::RedBlackEnv, pop::Population)
    (;S, D, K, N, replace_tasks) = env
    @assert size(pop) == (K, N)

    pop1 = similar(pop)

    for agent in 1:N
        knowledge = fresh_knowledge(env)
        social_learning!(env, knowledge, pop)

        tasks = sample_tasks(env)

        for (k, (a, b)) in enumerate(tasks)
            beh = behave(env, knowledge, a, b, K-k)
            learn!(env, knowledge, beh)
            pop1[k, agent] = beh
        end
    end
    pop1
end

function simulate(env::RedBlackEnv, n_gen; init=nothing)
    pop = initial_population(env, init)
    x = fill(pop, n_gen+1)
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
end

using DataStructures: CircularBuffer
function simulate_asymptote(env::RedBlackEnv; init=nothing, max_gen=1000, tol=1e-4, tol2=1e-5, win_size=30)
    pop = initial_population(env, init)
    history = CircularBuffer{Float64}(win_size)
    for gen in 0:max_gen
        pop′ = transition(env, pop)
        push!(history, red_rate(pop))


        d1 = diff(history); d2 = diff(d1)
        compositionality = mean(history); g1 = mean(d1); g2 = mean(d2)

        if length(history) == win_size && g1 < tol && g2 < tol2
            return (;gen, compositionality, g1, g2, converged=true)
        end
        pop = pop′
    end
    @warn "hit max_gen"
    return (;gen, compositionality, g1, g2, converged=false)
end
