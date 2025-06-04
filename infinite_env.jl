using Memoize
using StaticArrays

include("utils.jl")
include("probability.jl")

¬(p::Real) = 1 - p
prob_observe(p, k) = ¬((¬p) ^ k)

@kwdef struct InfiniteEnv
    S::Int = 5  # number of starts and goals
    D::Int = 5  # number of demonstrations
    
    # red frequencies for ambiguous cases
    p_0::Float64 = 0.  # no info
    p_r::Float64 = 0.  # part red
    
    ε::Float64 = 0. # lapse rate applied at end
end

@kwdef struct FreqPop <: FieldVector{5, Float64}
    bespoke_zilch::Float64 = 0.
    bespoke_full::Float64 = 0.
    comp_zilch::Float64 = 0.
    comp_partial::Float64 = 0.
    comp_full::Float64 = 0.
end

compositionality_rate(pop::FreqPop) = pop.comp_full + pop.comp_partial + pop.comp_zilch

function initial_population(env::InfiniteEnv, init::Float64)
    FreqPop(
        bespoke_zilch = 1 - init,
        comp_zilch = init,
    )
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

function observation_probabilities(S, D_comp, D_bespoke)
    bespoke = prob_observe(1 / S^2, D_bespoke)
    comp_a = comp_b = prob_observe(1 / S, D_comp)
    (;
        bespoke,
        full_only = ¬bespoke * comp_a * comp_b,
        partial_only = ¬bespoke * (comp_a * ¬comp_b + ¬comp_a * comp_b),
        zilch = ¬bespoke * ¬comp_a * ¬comp_b,
    )
end

function ensure_prob(x; tol=1e-8)
    if x < -tol || x > 1+tol
        error("Probability $x is out of bounds")
    end
    clip(x, 0., 1.)
end


function transition(env::InfiniteEnv, pop::FreqPop)
    (;S, D, p_0, p_r) = env
    c = ensure_prob(compositionality_rate(pop))

    pop2 = expectation(Binomial(D, c)) do D_comp
        D_bespoke = D - D_comp
        P = observation_probabilities(S, D_comp, D_bespoke)
        FreqPop(
            bespoke_zilch = P.zilch * ¬p_0 + P.partial_only * ¬p_r,
            bespoke_full = P.bespoke,
            comp_zilch = P.zilch * p_0,
            comp_partial = P.partial_only * p_r,
            comp_full = P.full_only,
        )
    end
    @assert sum(pop2) ≈ 1
    pop2
end

function simulate(env::InfiniteEnv, n_gen; init=0.)
    pop = init
    x = fill(pop, n_gen+1)
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
end

function find_stable_points(env::InfiniteEnv)
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

find_stable_points(;params...) = find_stable_points(InfiniteEnv(;params...))
