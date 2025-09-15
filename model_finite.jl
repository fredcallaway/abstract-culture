using DataStructures: CircularBuffer

include("model_shared.jl")

mutable struct Knowledge
    bespoke::BitMatrix
    comp_s::BitVector
    comp_g::BitVector
    _n_solved::Int
end

Knowledge(S::Int, G::Int) = Knowledge(falses(S, G), falses(S), falses(G), 0)


@kwdef struct FiniteModel
    S::Int = 5  # number of starts
    G::Int = 5  # number of goals
    D::Int = 5  # number of demonstrations
    K::Int = 1  # number of tasks per agent
    N::Int = 100 # population size
    agent_policy::InfoRates = agent_policy()

    replace_tasks::Bool = true  # sample tasks with replacement?
    replace_demos::Bool = false  # sample demonstrations with replacement? (false in experiment)

    # pre-allocate for efficiency
    _all_tasks::Vector{Tuple{Int, Int}} = collect(Iterators.product(1:S, 1:G))[:]
    _sampled_tasks::Vector{Tuple{Int, Int}} = fill((0, 0), K)
    _knowledge::Knowledge = Knowledge(S, G)
end

all_tasks(env::FiniteModel) = env._all_tasks
sample_tasks(env::FiniteModel) = sample!(env._all_tasks, env._sampled_tasks)


struct Behavior
    s::Int
    g::Int
    compositional::Bool
    info::Info  # not actually behavior, but kept for later tracking
end

FinitePop = Matrix{<:Behavior}

function compositional_rate(pop::FinitePop)
    mean(x->x.compositional, pop)
end

function initial_population(env::FiniteModel, ::Nothing=nothing)
    [Behavior(0, 0, false, Info(0, 0)) for _ in 1:env.K, _ in 1:env.N]
end

function initial_population(env::FiniteModel, init::Float64)
    (;S, G, N, K) = env
    compositional = falses(N * K)
    
    n_comp = init * N * K
    if abs(n_comp - round(Int, n_comp)) > 1e-3
        @warn "rounding initial comp rate from $init to $(round(Int, n_comp) / (N * K))"
    end
    for i in sample(eachindex(compositional), round(Int,n_comp); replace=false)
        compositional[i] = true
    end
    X = map(compositional) do comp
        Behavior(rand(1:S), rand(1:G), comp, Info(0, 0))
    end
    reshape(X, (K, N))
end

initial_population(::FiniteModel, init::FinitePop) = init

# %% ==================== learning ====================

# get it while it's hot!
# we use "cached" memory on the env to avoid allocating
@assert Threads.nthreads() == 1  # this is not thread safe
function fresh_knowledge(env::FiniteModel)
    k = env._knowledge
    k.bespoke .= false
    k.comp_s .= false
    k.comp_g .= false
    k._n_solved = 0
    k
end

function learn!(env::FiniteModel, knowledge::Knowledge, demo::Behavior)
    (;comp_s, comp_g, bespoke) = knowledge
    (;s, g, compositional) = demo
    (s == 0 || g == 0) && return  # dummy observation
    if compositional
        comp_s[s] = true
        comp_g[g] = true
    else
        bespoke[s, g] = true
    end
end

function social_learning!(env::FiniteModel, knowledge::Knowledge, pop)
    (;D, replace_demos) = env
    demos = D == -1 ? pop : sample(pop, D; replace=replace_demos)
    for d in demos
        learn!(env, knowledge, d)
    end
end

# %% ==================== acting ====================

cost(C::Costs, beh::Behavior) = cost(C, beh.info, beh.compositional)
cost(C::Costs, pop::FinitePop) = sum(cost(C, beh) for beh in pop) / length(pop)

relevant_info(knowledge::Knowledge, s, g) = Info(
    knowledge.bespoke[s,g], 
    knowledge.comp_s[s] + knowledge.comp_g[g]
)

function behave(env::FiniteModel, knowledge::Knowledge, s, g)
    # NOTE: this ignores long-run benefits of learning compositional stuff; see old_red_black.jl
    info = relevant_info(knowledge, s, g)
    p_comp = env.agent_policy[info]
    use_comp = rand(Bernoulli(p_comp))
    Behavior(s, g, use_comp, info)
end

# %% ==================== evolution ====================

function transition(env::FiniteModel, pop::FinitePop)
    (;S, D, K, N, replace_tasks) = env
    @assert size(pop) == (K, N)

    pop1 = similar(pop)

    for agent in 1:N
        knowledge = fresh_knowledge(env)
        social_learning!(env, knowledge, pop)

        tasks = sample_tasks(env)

        for (k, (s, g)) in enumerate(tasks)

            beh = behave(env, knowledge, s, g)  # trials_remaining: K-k
            learn!(env, knowledge, beh)
            pop1[k, agent] = beh
        end
    end
    pop1
end

function simulate(env::FiniteModel, n_gen; init=nothing)
    pop = initial_population(env, init)
    x = fill(pop, n_gen+1)
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
end

function simulate_asymptote(env::FiniteModel; init=nothing, max_gen=1000, tol=1e-4, tol2=1e-5, win_size=30)
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
