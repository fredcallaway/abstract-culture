using Roots
using Memoize
include("utils.jl")

¬(p::Real) = 1 - p
prob_observe(p, k) = ¬((¬p) ^ k)

struct Knowledge
    black::BitMatrix
    red_a::BitVector
    red_b::BitVector
end

@kwdef struct RedBlackEnv
    S::Int = 5  # number of starts and goals
    D::Int = 5  # number of demonstrations
    K::Int = 1  # number of tasks per agent
    ε::Float64 = 0. # lapse rate
    N::Real = Inf # population size

    myopic::Bool = false  # ignore future cost in K>1 case?
    replace_tasks::Bool = true  # sample tasks with replacement?
    # replace_demos::Bool = true  # sample demonstrations with replacement?

    # red frequencies for ambiguous cases
    p_0::Float64 = 0.  # no info
    p_r::Float64 = 0.  # part red
    p_brr::Float64 = 0.  # full both

    # redundant with above, avoid recomputing
    _all_tasks::Vector{Tuple{Int, Int}} = collect(Iterators.product(1:S, 1:S))[:]
    _sampled_tasks::Vector{Tuple{Int, Int}} = fill((0, 0), K)
    _knowledge::Knowledge = Knowledge(falses(S, S), falses(S), falses(S))
end

all_tasks(env::RedBlackEnv) = env._all_tasks
sample_tasks(env::RedBlackEnv) = sample!(env._all_tasks, env._sampled_tasks)

function initial_population(env::RedBlackEnv, init::Nothing=nothing)
    if isinf(env.N)
        NaN
    else
        zeros(env.S, env.S, 2)
    end
end

function initial_population(env::RedBlackEnv, init::Float64)
    (;S, N, K) = env
    if isinf(N)
        init
    else
        x = zeros(S, S, 2)
        n_comp = Int(init * N * K)
        for i in 1:(N * K)
            comp_idx = 1 + (i ≤ n_comp)
            x[rand(1:S), rand(1:S), comp_idx] += 1
        end
        normalize!(x)
    end
end

# %% ==================== learning ====================

# get it while it's hot!
function fresh_knowledge(env::RedBlackEnv)
    k = env._knowledge
    k.black .= false
    k.red_a .= false
    k.red_b .= false
    k
end

function learn!(k::Knowledge, a::Int, b::Int, use_red::Bool)
    if use_red
        k.red_a[a] = k.red_b[b] = true
    else
        k.black[a, b] = true
    end
end

function social_learning!(k::Knowledge, pop, D)
    w = Weights(view(pop, :), 1.)
    @assert D ≥ 0
    for i in 1:D
        (a, b, c) = sample(CartesianIndices(pop), w).I
        if c == 2
            k.red_a[a] = k.red_b[b] = true
        else
            k.black[a, b] = true
        end
    end
end

# %% ==================== acting ====================

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
            if expected_future_savings(env.S, trials_remaining, knowledge) > 1.
                1.
            else
                env.p_0
            end
        end
    end
end

function expected_future_savings(S::Int, K::Int, knowledge::Knowledge)
    K == 0 && return 0.
    ec = expected_cost(S, K, knowledge)
    ec.idiosyncratic - ec.compositional
end

expected_unique(n_option, n_active, n_sample) = n_active * (1 - (1 - 1/n_option)^n_sample)

function expected_cost(S::Int, K::Int, knowledge::Knowledge)
    edges_left = sum(all_tasks(env)) do (a, b)
        !(knowledge.black[a, b] || knowledge.red_a[a] && knowledge.red_b[b])
    end
    (;
        compositional = expected_unique(S, S - sum(knowledge.red_a), K) +
                         expected_unique(S, S - sum(knowledge.red_b), K),
        idiosyncratic = expected_unique(S^2, edges_left, K)
    )
end

# %% ==================== evolution ====================

function transition(env::RedBlackEnv, pop::Array{Float64, 3})
    (;S, D, K, N, replace_tasks) = env

    pop1 = zeros(size(pop))

    for agent in 1:N
        knowledge = fresh_knowledge(env)
        social_learning!(knowledge, pop, D)

        tasks = sample_tasks(env)

        for (i, (a, b)) in enumerate(tasks)
            use_red = rand() < prob_red(env, knowledge, a, b, K-i)
            learn!(knowledge, a, b, use_red)
            pop1[a, b, use_red+1] += 1
        end
    end
    normalize!(pop1)
end

function simulate(env::RedBlackEnv, n_gen; init=nothing)
    pop = initial_population(env, init)
    x = fill(pop, n_gen+1)
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
end

function red_rate(pop::Array{Float64, 3})
    sum(pop[:, :, 2])
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
