using Roots
using Memoize
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

@kwdef struct RedBlackEnv
    S::Int = 5  # number of starts and goals
    D::Int = 5  # number of demonstrations
    K::Int = 1  # number of tasks per agent
    ε::Float64 = 0. # lapse rate
    N::Real = Inf # population size

    myopic::Bool = false  # ignore future cost in K>1 case?
    replace_tasks::Bool = true  # sample tasks with replacement?
    replace_demos::Bool = true  # sample demonstrations with replacement?

    # red frequencies for ambiguous cases
    p_0::Float64 = 0.  # no info
    p_r::Float64 = 0.  # part red
    p_brr::Float64 = 0.  # full both

    # redundant with above, avoid recomputing
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
    if isinf(env.N)
        NaN
    else
        [Behavior(0, 0, false) for _ in 1:env.K, _ in 1:env.N]
    end
end

function initial_population(env::RedBlackEnv, init::Float64)
    (;S, N, K) = env
    if isinf(N)
        init
    else
        compositional = falses(N * K)
        for i in sample(eachindex(compositional), Int(init * N * K); replace=false)
            compositional[i] = true
        end
        X = map(compositional) do red
            Behavior(rand(1:S), rand(1:S), red)
        end
        reshape(X, (K, N))
    end
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
    (;red_a, red_b, black, _n_solved) = knowledge
    (;a, b, red) = demo
    (a == 0 || b == 0) && return  # dummy observation
    S = length(red_a)
    foresight = env.K > 1 && !env.myopic
    # all the if foresight lines are just updating _n_solved, for computing expected_cost
    if red
        if foresight && !red_a[a]
            # println("new a $a")
            for b2 in 1:S
                if red_b[b2] && !black[a, b2]
                    _n_solved += 1
                end
            end
        end
        red_a[a] = true
        if foresight && !red_b[b]
            # println("new b $b")
            for a2 in 1:S
                if red_a[a2] && !black[a2, b]
                    _n_solved += 1
                end
            end
        end
        red_b[b] = true
    else
        if foresight && !black[a, b] && !(red_a[a] && red_b[b])
            # println("new black $a - $b")
            _n_solved += 1
        end
        black[a, b] = true
    end
    knowledge._n_solved = _n_solved
end

function social_learning!(env::RedBlackEnv, knowledge::Knowledge, pop)
    (;D, replace_demos) = env
    demos = D == -1 ? pop : sample(pop, D; replace=replace_demos)
    for d in demos
        learn!(env, knowledge, d)
    end
end

# %% ==================== acting ====================

function behave(env::RedBlackEnv, knowledge::Knowledge, a, b, trials_remaining)
    use_red = rand(Bernoulli(prob_red(env, knowledge, a, b, trials_remaining)))
    Behavior(a, b, use_red)
end

function prob_red(env::RedBlackEnv, knowledge::Knowledge, a, b, trials_remaining)
    red_known = knowledge.red_a[a] + knowledge.red_b[b]
    black_known = knowledge.black[a,b]

    if env.myopic
        trials_remaining = 0
    end

    if black_known
        if red_known == 2  # no cost either way
            env.p_brr
        else  # take the free solution
            0.
        end
    else
        if red_known == 2  # take the free solution
            1.
        elseif red_known == 1  # must discover one edge either way
            if trials_remaining > 0  # could benefit from later reuse
                1.
            else
                env.p_r
            end
        else  # no relevant information
            # learn compositional solution if future savings outweighs immediate cost
            if expected_future_savings(env, trials_remaining, knowledge) > 1.
                1.
            else
                env.p_0
            end
        end
    end
end

function expected_future_savings(env::RedBlackEnv, K::Int, knowledge::Knowledge)
    K == 0 && return 0.
    ec = expected_cost(env, K, knowledge)
    ec.idiosyncratic - ec.compositional
end

expected_unique(n_option, n_active, n_sample) = n_active * (1 - (1 - 1/n_option)^n_sample)

function expected_cost(env::RedBlackEnv, K::Int, knowledge::Knowledge)
    S = env.S
    edges_left = S^2 - knowledge._n_solved
    # @infiltrate edges_left != sum(all_tasks(env)) do (a, b)
    #     !(knowledge.black[a, b] || knowledge.red_a[a] && knowledge.red_b[b])
    # end
    knowledge.black
    knowledge.red_a
    knowledge.red_b
    (;
        compositional = expected_unique(S, S - sum(knowledge.red_a), K) +
                         expected_unique(S, S - sum(knowledge.red_b), K),
        idiosyncratic = expected_unique(S^2, edges_left, K)
    )
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

# %% ==================== analytic for infinite-population one-task case ====================

# behavior given observation probabilities
function prob_learn_red(env, b, r)
    (;ε, p_brr, p_r, p_0) = env
    p =
        b *  r * r * p_brr +
        ¬b * (
            r * r +
            r * ¬r * 2p_r +  # two ways for this to happen
            ¬r * ¬r * p_0
        )
    ε * .5 + (1-ε) * p
end

function transition(env::RedBlackEnv, p_red::Float64)
    (;S, D, K) = env
    @assert K == 1
    # @assert all(T .≈ 1 / length(T))

    # prob observe the desired black edge
    if isnan(p_red)
        env.p_0
    else
        @assert -1e-4 < p_red < 1 + 1e-4
        p_red = clip(p_red, 0, 1)
        if env.p_r == 1. && env.p_0 == 0 && env.p_brr == 0
            p_c = p_red * (2S - 1) / S^2
            p_b = (1 - p_red) / S^2
            (1 - p_b)^D * (1 - (1 - (p_c / (1 - p_b)))^D)
        else
            expectation(Binomial(D, p_red)) do n_comp_demo
                b = prob_observe(1 / (S^2), D - n_comp_demo)
                r = prob_observe(1 / S, n_comp_demo)
                prob_learn_red(env, b, r)
            end
        end
    end
end

function find_stable_points(env::RedBlackEnv)
    stable = find_zeros(0, 1) do x
        transition(env, x) - x
    end

    i = findfirst(stable) do x
        x ≈ 1 && return false
        x += 1e-8
        transition(env, x) > x
    end

    start = if isnothing(i)
        if transition(env, 0.) > 0
            0.
        else
            NaN
        end
    else
        stable[i]
    end

    i = findlast(stable) do x
        x ≈ 0 && return false
        transition(env, x - 1e-8) > x - 1e-8 && transition(env, x + 1e-8)  < x + 1e-8
    end

    stop = if isnothing(i)
        NaN
    else
        stable[i]
    end

    (;start, stop)
end

find_stable_points(;params...) = find_stable_points(RedBlackEnv(;params...))
