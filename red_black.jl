using Roots
using Memoize
include("utils.jl")
# include("find_compositions.jl")

¬(p::Real) = 1 - p
prob_observe(p, k) = ¬((¬p) ^ k)


@kwdef struct RedBlackEnv
    S::Int = 5  # number of starts and goals
    D::Int = 5  # number of demonstrations
    K::Int = 1  # number of tasks per agent
    ε::Float64 = 0. # lapse rate
    N::Real = Inf # population size

    foresight::Bool = false  # choose composition in advance?
    replace_tasks::Bool = true  # sample tasks with replacement?
    replace_demos::Bool = true  # sample demonstrations with replacement?

    # red frequencies for ambiguous cases
    p_0::Float64 = 0.  # no info
    p_r::Float64 = 0.  # part red
    p_brr::Float64 = 0.  # full both

    # used to avoid memory allocation
    knowledege::BitMatrix = falses(S, S+2)
end

@memoize function all_tasks(S::Int)
    collect(Iterators.product(1:S, S+1:2S))[:]
end
all_tasks(env::RedBlackEnv) = all_tasks(env.S)

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
    (;S, D, K, N, replace_tasks, replace_demos) = env
    @assert N == size(pop, 2)

    pop1 = similar(pop)

    for agent in 1:N
        observed = D == -1 ? pop : sample(pop, D; replace=replace_demos)
        tasks = sample(allntasks(env), K; replace=replace_tasks)
        pop1[:, agent] = behave(env, tasks, observed)
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
    (;S, D, K) = env
    @assert K == 1
    # @assert all(T .≈ 1 / length(T))

    # prob observe the desired black edge
    if isnan(p_red)
        env.p_0
    else
        b = prob_observe(¬p_red / (S^2), D)
        # prob observe the right bottom/top red edge (not both)
        # these are independent events because you observe D bottom and D top
        r = prob_observe(p_red / S, D)

        prob_learn_red(env, b, r)
    end
end

function simulate(env::RedBlackEnv, n_gen; init=initial_population(env))
    x = zeros(n_gen+1)
    x[1] = init
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
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


function find_stable_points(;params...)
    env = RedBlackEnv(;params...)
    stable = find_zeros(0, 1) do x
        transition(env, x) - x
    end

    # if length(stable) == 1
    #     if transition(env, .5) < .5
    #         return (;start=NaN, stop=NaN)
    #     else

    #         @infiltrate transition(env, .99999) > .99999

    #         return (;start=0., stop=1.)
    #     end
    # end

    # if length(stable) == 2 && transition(env, .99999) > .99999
    #     return (start=stable[2], stop=1.)
    # end

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
    
    # if length(stable) == 1
    #     stable
    #     if transition(env, .5) < .5
    #         return (;zero=NaN, start=NaN, stop=NaN)
    #     else
    #         @assert transition(env, .9999) > .9999
    #         return (;zero=0., start=0.)
    #     end
    # elseif length(stable) == 2
    #     if transition(env, 1e-6) > 1e-6
    #         return (;zero=0, start=0, stop=stable[2])
    #     else
    #         return (;zero=0, start=NaN, stop=NaN)
    #     end
    # elseif length(stable) == 3
    #     @label stable3
    #     @infiltry @assert transition(env, stable[2] - 1e-6) < stable[2] - 1e-6
    #     @infiltry @assert transition(env, stable[2] + 1e-6) > stable[2] + 1e-6
    #     @infiltry @assert transition(env, stable[3] + 1e-6) < stable[3] + 1e-6
    #     return (;zero=0, start=stable[2], stop=stable[3])
    # elseif length(stable) == 4
    #     # ignore the case at exactly 1. for now
    #     @infiltry @assert stable[end] ≈ 1.
    #     @assert transition(env, .9999) < .9999
    #     @goto stable3
    # else
    #     @assert false
    # end
end
