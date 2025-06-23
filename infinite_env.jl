using StaticArrays
using Roots

include("utils.jl")
include("probability.jl")
include("broadcastable.jl")


@broadcastable @kwdef struct InfiniteEnv
    S::Int = 5  # number of starts and goals
    D::Int = 5  # number of demonstrations
    
    # red frequencies for ambiguous cases
    p_0::Float64 = 0.  # no info
    p_r::Float64 = 1.  # part red
    
    ε::Float64 = 0. # lapse rate applied at end
end

¬(p::Real) = 1 - p
prob_observe(p, k) = ¬((¬p) ^ k)

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

abstract type InfinitePop end

@broadcastable struct CompPop <: InfinitePop
    comp::Float64
end

compositional_rate(pop::CompPop) = pop.comp

function transition(env::InfiniteEnv, pop::CompPop)
    (;S, D, p_0, p_r) = env
    c = ensure_prob(compositional_rate(pop))

    expectation(Binomial(D, c)) do D_comp
        D_bespoke = D - D_comp
        P = observation_probabilities(S, D_comp, D_bespoke)
        let
            P.zilch * p_0 +
            P.partial_only * p_r +
            P.full_only
        end
    end |> CompPop
end

initialize(::InfiniteEnv, init::InfinitePop) = init
initialize(::InfiniteEnv, init::Real) = CompPop(init)

function simulate(env::InfiniteEnv, n_gen; init=0.)
    gen0 = initialize(env, init)

    x = fill(gen0, n_gen+1)
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
end

function fixed_points(env::InfiniteEnv)
    find_zeros(0, 1) do c
        c2 = transition(env, c)
        c2 - c
    end
end

# %% ===== expanded form populations ===========================================

CompPop(pop::InfinitePop) = CompPop(compositional_rate(pop))
transition(env::InfiniteEnv, c::Float64) = transition(env, CompPop(c)).comp


struct FullPop{S} <: InfinitePop
    C::SMatrix{S,S,Float64}
    B::SMatrix{S,S,Float64}
end

compositional_rate(pop::FullPop) = sum(pop.C)

FullPop{S}(c::Float64) where S = FullPop{S}(
    c .* ones(S, S) ./ S^2,
    (1 - c) .* ones(S, S) ./ S^2,
)
FullPop(S::Int, c::Float64) = FullPop{S}(c)

function transition(env::InfiniteEnv, pop::FullPop)
    (;S, D, p_0, p_r) = env
    @assert p_r == 1.  # TODO: handle other cases

    (;B, C) = pop
    
    B1 = similar(pop.B); C1 = similar(pop.C)
    for i in 1:S, j in 1:S
        term1 = 1/S^2
        term2 = (1 - B[i,j])^D
        term3_inner = (sum(C[i, :]) + sum(C[:, j]) - C[i,j]) / (1 - B[i,j])
        term3 = 1 - (1 - term3_inner)^D
        C1[i,j] = term1 * term2 * term3
        B1[i, j] = term1 - C1[i,j]
    end
    FullPop{S}(C1, B1)
end

# # %% ===== FreqPop ============================================================


@broadcastable @kwdef struct FreqPop <: InfinitePop
    bespoke_zilch::Float64 = 0.
    bespoke_full::Float64 = 0.
    comp_zilch::Float64 = 0.
    comp_partial::Float64 = 0.
    comp_full::Float64 = 0.
end

FreqPop(pop::CompPop) = FreqPop(
    comp_full = pop.comp,
    bespoke_full = 1 - pop.comp
)
FreqPop(pop::FullPop) = FreqPop(CompPop(pop))

compositional_rate(pop::FreqPop) = pop.comp_full + pop.comp_partial + pop.comp_zilch

function transition(env::InfiniteEnv, pop::FreqPop)
    (;S, D, p_0, p_r) = env
    c = ensure_prob(compositional_rate(pop))

    expectation(Binomial(D, c)) do D_comp
        D_bespoke = D - D_comp
        P = observation_probabilities(S, D_comp, D_bespoke)
        FreqPop(
            P.zilch * ¬p_0 + P.partial_only * ¬p_r,
            P.bespoke,
            P.zilch * p_0,
            P.partial_only * p_r,
            P.full_only,
        )
    end
end

# %% ===== Pop3 ===============================================================


@kwdef struct Pop3 <: InfinitePop
    indiv::Float64
    bespoke::Float64
    comp::Float64
end

Pop3(pop::FreqPop) = Pop3(
    pop.bespoke_zilch + pop.comp_zilch,
    pop.bespoke_full,
    pop.comp_full + pop.comp_partial,
)

FreqPop(pop::Pop3, p_0::Float64) = FreqPop(
    bespoke_full = pop.bespoke,
    comp_full = pop.comp,
    comp_zilch = pop.indiv * p_0,
    bespoke_zilch = pop.indiv * (1 - p_0),
)

initialize(::InfiniteEnv, init::Pop3) = init
transition(env::InfiniteEnv, pop::Pop3) = transition(env, FreqPop(pop, env.p_0)) |> Pop3
compositional_rate(pop::Pop3) = pop.comp