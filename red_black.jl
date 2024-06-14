
using Memoize
include("utils.jl")
# include("find_compositions.jl")

¬(p::Real) = 1 - p
prob_observe(p, M) = ¬((¬p) ^ M)


@kwdef struct RedBlackEnv
    S::Int = 5  # number of starts and goals
    M::Int = 5  # number of models
    K::Int = 1  # number of tasks per agent
    ε::Float64 = 0. # lapse rate
    T::Matrix{Float64} = normalize!(ones(S, S))  # task distribution
    N::Real = Inf # population size
    foresight::Bool = false  # choose composition in advance?
    # red frequencies for ambiguous cases
    p_0::Float64 = 0.  # no info
    p_r::Float64 = 1.  # part red
    p_brr::Float64 = 0.  # full both

    # used to avoid memory allocation
    knowledege::BitMatrix = falses(S, S+2)
end

@memoize function taskdist(T::Matrix{Float64})
    S = size(T, 1)
    map(CartesianIndices(T)[:]) do c
        s, g = c.I
        (s, g+S)
    end
    # SetSampler(get.(CartesianIndices(T), :I)[:], T[:])
end
taskdist(env::RedBlackEnv) = taskdist(env.T)

struct Behavior
    s::Int
    g::Int
    red::Bool
end

function red_rate(pop::AbstractArray{<:Behavior})
    mean(get.(pop, :red))
end

function initial_population(env::RedBlackEnv)
    if isinf(env.N)
        NaN
    else
        [Behavior(0, 0, false) for _ in 1:env.K, _ in 1:env.N]
    end
end

function learn!(knowledege, b::Behavior)
    (;s, g, red) = b
    (s == 0 || g == 0) && return  # dummy observation
    S = size(knowledege, 1)
    g = mod1(g, S)
    if red
        knowledege[s, S+1] = true
        knowledege[g, S+2] = true
    else
        knowledege[s, g] = true
    end
end

function learn_red!(knowledege, s)
    S = size(knowledege, 1)
    if s > S
        knowledege[mod1(s, S), S+2] = true
    else
        knowledege[s, S+1] = true
    end
end

function red_known(knowledege, s)
    S = size(knowledege, 1)
    knowledege[mod1(s, S), S+1+div(s-1, S)]
end

red_known(knowledege, s, g) = red_known(knowledege, s) + red_known(knowledege, g)

function black_known(knowledege, s, g)
    S = size(knowledege, 1)
    g = mod1(g, S)
    knowledege[s, g]
end


function behave(env, tasks, observed)
    knowledege = env.knowledege
    fill!(knowledege, false)
    for b in observed
        learn!(knowledege, b)
    end
    if env.foresight
        # note: this modifies knowledege
        find_compositions!(env, tasks)
    end
    map(tasks) do (s, g)
        b = black_known(knowledege, s, g)
        r = red_known(knowledege, s, g)
        use_red = if b
            r == 2 ? rand() < env.p_brr : false
        else
            r == 2 ? true :
            r == 1 ? rand() < env.p_r :
            rand() < env.p_0
        end
        b = Behavior(s, g, use_red)
        learn!(knowledege, b)
        b
    end
end

function transition(env::RedBlackEnv, pop::Matrix{Behavior})
    (;S, M, T, K, N) = env
    @assert N == size(pop, 2)

    pop1 = similar(pop)
    # tasks = rand(taskdist(env), K, N)
    tasks = mapreduce(hcat, 1:N) do i
        sample(taskdist(env), K; replace=true)
    end


    for agent in 1:N
        observed = sample(pop, M)
        pop1[:, agent] = behave(env, tasks[:, agent], observed)
    end
    pop1
end

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
    (;S, M, K) = env
    @assert K == 1
    # @assert all(T .≈ 1 / length(T))

    # prob observe the desired black edge
    if isnan(p_red)
        env.p_0
    else
        b = prob_observe(¬p_red / (S^2), M)
        # prob observe the right bottom/top red edge (not both)
        # these are independent events because you observe M bottom and M top
        r = prob_observe(p_red / S, M)

        prob_learn_red(env, b, r)
    end
end


function simulate(env::RedBlackEnv, n_gen; init=initial_population(env))
    pop = init
    # note: initial pop is a dummy, containing no cultural knowledge
    repeatedly(n_gen) do
        pop = transition(env, pop)
        pop
    end
end

function get_limit(env::RedBlackEnv; init=initial_population(env), max_gen=100000)
    pop = init
    @assert env.N == Inf
    for gen in 0:max_gen
        pop′ = transition(env, pop)
        if pop′ ≈ pop
            return (pop, gen)
        elseif gen == max_gen && abs(pop′ - pop) < 1e-3
            return (pop, gen)
        end
        pop = pop′
    end
    @warn "hit max_gen"
    missing, missing
end

