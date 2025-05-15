using Roots
using Memoize
using StaticArrays
include("utils.jl")

¬(p::Real) = 1 - p
prob_observe(p, k) = ¬((¬p) ^ k)

mutable struct Knowledge
    bespoke::BitMatrix
    compositional_a::BitVector
    compositional_b::BitVector
    _n_solved::Int
end

Knowledge(S::Int) = Knowledge(falses(S, S), falses(S), falses(S), 0)

abstract type AgentPolicy end

@kwdef struct BinaryCompositionEnv
    S::Int = 5  # number of starts and goals
    D::Int = 5  # number of demonstrations
    K::Int = 1  # number of tasks per agent
    N::Real = Inf # population size
    agent_policy::AgentPolicy = classic_policy()

    replace_tasks::Bool = true  # sample tasks with replacement?
    replace_demos::Bool = true  # sample demonstrations with replacement?

    # pre-allocate for efficiency
    _all_tasks::Vector{Tuple{Int, Int}} = collect(Iterators.product(1:S, 1:S))[:]
    _sampled_tasks::Vector{Tuple{Int, Int}} = fill((0, 0), K)
    _knowledge::Knowledge = Knowledge(S)
end

all_tasks(env::BinaryCompositionEnv) = env._all_tasks
sample_tasks(env::BinaryCompositionEnv) = sample!(env._all_tasks, env._sampled_tasks)


struct Info
    bespoke::Int  # 0: none, 1: full
    comp::Int  # 0: none, 1: partial, 2: full
end

struct Behavior
    a::Int
    b::Int
    compositional::Bool
    info::Info  # not actually behavior, but kept for later tracking
end

Population = Matrix{<:Behavior}

function compositional_rate(pop::Population)
    mean(x->x.compositional, pop)
end

function initial_population(env::BinaryCompositionEnv, ::Nothing=nothing)
    [Behavior(0, 0, false, Info(0, 0)) for _ in 1:env.K, _ in 1:env.N]
end

function initial_population(env::BinaryCompositionEnv, init::Float64)
    (;S, N, K) = env
    compositional = falses(N * K)
    for i in sample(eachindex(compositional), Int(init * N * K); replace=false)
        compositional[i] = true
    end
    X = map(compositional) do compositional
        Behavior(rand(1:S), rand(1:S), compositional, Info(0, 0))
    end
    reshape(X, (K, N))
end

initial_population(::BinaryCompositionEnv, init::Population) = init

# %% ==================== learning ====================

# get it while it's hot!
# we use "cached" memory on the env to avoid allocating
@assert Threads.nthreads() == 1  # this is not thread safe
function fresh_knowledge(env::BinaryCompositionEnv)
    k = env._knowledge
    k.bespoke .= false
    k.compositional_a .= false
    k.compositional_b .= false
    k._n_solved = 0
    k
end

function learn!(env::BinaryCompositionEnv, knowledge::Knowledge, demo::Behavior)
    (;compositional_a, compositional_b, bespoke) = knowledge
    (;a, b, compositional) = demo
    (a == 0 || b == 0) && return  # dummy observation
    if compositional
        compositional_a[a] = true
        compositional_b[b] = true
    else
        bespoke[a, b] = true
    end
end

function social_learning!(env::BinaryCompositionEnv, knowledge::Knowledge, pop)
    (;D, replace_demos) = env
    demos = D == -1 ? pop : sample(pop, D; replace=replace_demos)
    for d in demos
        learn!(env, knowledge, d)
    end
end

# %% ==================== acting ====================


struct TabularPolicy <: AgentPolicy
    table::SMatrix{2, 4, Float64} # p(compositional) for bespoke (no/yes) × compositional (none, partial, full, exact)
    TabularPolicy(table::AbstractMatrix{Float64}) = begin
        X = if size(table) == (2, 3)
            # treat full and exact the same
            hcat(table, table[:, end])
        else
            table
        end
        new(X)
    end
end

function classic_policy(;p_0=0.0, p_r=0.0, p_brr=0.0, ε=0.0)
    TabularPolicy([
        p_0 p_r 1-ε 1-ε
        ε ε p_brr p_brr
    ])
end

function prob_compositional(policy::TabularPolicy, info::Info)
    return policy.table[info.bespoke+1, info.comp+1]
end

@kwdef struct Costs
    comp_full::Float64  # cost of compositional solution when you observed both parts
    comp_partial::Float64
    comp_none::Float64
    bespoke_full::Float64
    bespoke_none::Float64
end

function cost(C::Costs, info::Info, compositional::Bool)
    if compositional
        [C.comp_none, C.comp_partial, C.comp_full][info.comp + 1]
    else
        [C.bespoke_none, C.bespoke_full][info.bespoke + 1]
    end
end

cost(C::Costs, beh::Behavior) = cost(C, beh.info, beh.compositional)

function rational_policy(C::Costs; β=100., ε=0.)
    map(product(0:1, 0:2)) do (bespoke_known, comp_known)
        info = Info(bespoke_known, comp_known)
        rel_cost = cost(C, info, true) - cost(C, info, false)
        p = logistic(β * -rel_cost)
        lapse(p, ε)
    end |> TabularPolicy
end

relevant_info(knowledge::Knowledge, a, b) = Info(
    knowledge.bespoke[a,b], 
    knowledge.compositional_a[a] + knowledge.compositional_b[b]
)

function behave(env::BinaryCompositionEnv, knowledge::Knowledge, a, b)
    # NOTE: this ignores long-run benefits of learning compositional stuff; see old_red_black.jl
    info = relevant_info(knowledge, a, b)
    p_comp = prob_compositional(env.agent_policy, info)
    use_comp = rand(Bernoulli(p_comp))
    Behavior(a, b, use_comp, info)
end

# %% ==================== evolution ====================

function transition(env::BinaryCompositionEnv, pop::Population)
    (;S, D, K, N, replace_tasks) = env
    @assert size(pop) == (K, N)

    pop1 = similar(pop)

    for agent in 1:N
        knowledge = fresh_knowledge(env)
        social_learning!(env, knowledge, pop)

        tasks = sample_tasks(env)

        for (k, (a, b)) in enumerate(tasks)

            beh = behave(env, knowledge, a, b)  # trials_remaining: K-k
            learn!(env, knowledge, beh)
            pop1[k, agent] = beh
        end
    end
    pop1
end

function simulate(env::BinaryCompositionEnv, n_gen; init=nothing)
    pop = initial_population(env, init)
    x = fill(pop, n_gen+1)
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
end

using DataStructures: CircularBuffer
function simulate_asymptote(env::BinaryCompositionEnv; init=nothing, max_gen=1000, tol=1e-4, tol2=1e-5, win_size=30)
    pop = initial_population(env, init)
    history = CircularBuffer{Float64}(win_size)
    for gen in 0:max_gen
        new_pop = transition(env, pop)
        push!(history, compositional_rate(pop))


        d1 = diff(history); d2 = diff(d1)
        compositionality = mean(history); g1 = mean(d1); g2 = mean(d2)

        if length(history) == win_size && g1 < tol && g2 < tol2
            return (;gen, compositionality, g1, g2, converged=true)
        end
        pop = new_pop
    end
    error("hit max_gen in simulate_asymptote")
    # @warn "hit max_gen"
    # return (;gen=0, compositionality=NaN, g1=NaN, g2=NaN, converged=false)
    # return (;gen, compositionality, g1, g2, converged=false)
end
