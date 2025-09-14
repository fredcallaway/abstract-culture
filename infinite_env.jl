using StaticArrays
using Roots
using Distributions

include("utils.jl")

# used for observation and action probabilities
@kwdef struct EndowmentProbs <: FieldMatrix{2, 3, Float64}
    b0c0::Float64
    b1c0::Float64
    b0c1::Float64
    b1c1::Float64
    b0c2::Float64
    b1c2::Float64
end

struct InfiniteEnv
    S::Int  # number of starts
    G::Int  # number of goals
    D::Int  # number of demonstrations
    agent_policy::EndowmentProbs
end

InfiniteEnv(;S, D, G=S, kws...) = InfiniteEnv(S, G, D, agent_policy(;kws...))
InfiniteEnv(pol::EndowmentProbs; S, D, G=S) = InfiniteEnv(S, G, D, pol)

abstract type InfinitePop end

# %% ===== social observations ================================================

¬(p::Real) = 1 - p
prob_observe(p, k) = ¬((¬p) ^ k)

#  OLD VERSION WITHOUT G
# function observation_probabilities(S, D_comp, D_bespoke)
#     d_bespoke = Bernoulli(prob_observe(1 / S^2, D_bespoke))
#     d_comp = Binomial(2, prob_observe(1 / S, D_comp))

#     # SVector prevents memory allocation
#     p_bespoke = pdf(d_bespoke, SVector(0,1))
#     p_comp = pdf(d_comp, SVector(0,1,2))
    
#     EndowmentProbs(p_bespoke * p_comp')
# end

function observation_probabilities(S, G, D_comp, D_bespoke)
    # d_bespoke = Bernoulli(prob_observe(1 / S^2, D_bespoke))
    pB = prob_observe(1 / (S*G), D_bespoke)
    pS = prob_observe(1 / S, D_comp)
    pG = prob_observe(1 / G, D_comp)

    # SVector prevents memory allocation
    p_bespoke = SVector(¬pB, pB)
    p_comp = SVector(
        ¬pS * ¬pG,  # neither
        ¬pS * pG + pS * ¬pG,  # one
        pS * pG,  # both
    )
    @assert sum(p_comp) ≈ 1.
    
    EndowmentProbs(p_bespoke * p_comp')
end

function observation_probabilities(env::InfiniteEnv, pop::InfinitePop)
    c = ensure_prob(compositional_rate(pop))
    expectation(Binomial(env.D, c)) do D_comp
        D_bespoke = env.D - D_comp
        observation_probabilities(env.S, env.G, D_comp, D_bespoke)
    end
end


# %% ===== agent policy =======================================================

function agent_policy(;kws...)
    default = (
        b0c0 = 0.,  # no info
        b1c0 = 0.,
        b0c1 = 1.,  # no bespoke, half comp
        b1c1 = 0.,
        b0c2 = 1.,  # no bespoke, full comp
        b1c2 = 0.,
    )
    EndowmentProbs(;default..., kws...)
end
bespoke_policy() = zeros(EndowmentProbs)
comp_policy() = ones(EndowmentProbs)

@kwdef struct Costs
    bespoke_zilch::Float64 = 0.
    bespoke_full::Float64 = 0.
    comp_zilch::Float64 = 0.
    comp_partial::Float64 = 0.
    comp_full::Float64 = 0.
end

function cost(C::Costs, bespoke_known::Int, comp_known::Int, comp_solution::Bool)
    if comp_solution
        (C.comp_zilch, C.comp_partial, C.comp_full)[comp_known + 1]
    else
        (C.bespoke_zilch, C.bespoke_full)[bespoke_known + 1]
    end
end

function Base.show(io::IO, ::MIME"text/plain", costs::Costs)
    print(io, "Costs")
    fields = fieldnames(Costs)
    for f in fields
        val = getfield(costs, f)
        print(io, "\n  ", string(f), " = ", val)
    end
end

function rational_policy(C::Costs; β=1e10, ε=0.)
    map(product(0:1, 0:2)) do (bespoke_known, comp_known)
        rel_cost = cost(C, bespoke_known, comp_known, true) - cost(C, bespoke_known, comp_known, false)
        p = logistic(β * -rel_cost)
        lapse(p, ε)
    end |> EndowmentProbs
end

# %% ===== population dynamics ================================================

@kwdef struct CompPop{T<:Real} <: InfinitePop
    comp::T
end

compositional_rate(pop::CompPop) = pop.comp

function transition(env::InfiniteEnv, pop::CompPop)
    CompPop(sum(observation_probabilities(env, pop) .* env.agent_policy))
end

function simulate(env::InfiniteEnv, n_gen; init=0.)
    gen0 = init isa Real ? CompPop(init) : init

    x = fill(gen0, n_gen+1)
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
end

function fixed_points(env::InfiniteEnv; raw=false)
    fixed = find_zeros(0, 1) do c
        c2 = transition(env, c)
        c2 - c
    end
    raw && return fixed
    if isempty(fixed)  # shouldn't happen
        return fixed
    end

    # handle edge cases with numerical problems
    if fixed[end] == 0.
        c = 1 - 1e-10
        if transition(env, c) > c
            push!(fixed, 1.)
        end
    elseif fixed[end] == 1.
        c = 1 - 1e-10
        if transition(env, c) < c
            pop!(fixed)
        end
    end
    fixed
end

# %% ===== expanded form populations ===========================================

CompPop(pop::InfinitePop) = CompPop(compositional_rate(pop))
transition(env::InfiniteEnv, c::Real) = transition(env, CompPop(c)).comp


struct FullPop{S,G} <: InfinitePop
    C::SMatrix{S,G,Float64}
    B::SMatrix{S,G,Float64}
end

compositional_rate(pop::FullPop) = sum(pop.C)

FullPop{S,G}(c::Float64) where {S,G} = FullPop{S,G}(
    (c / (S*G)) .* ones(S, G),
    ((1 - c) / (S*G)) .* ones(S, G),
)
FullPop(S::Int, G::Int, c::Float64) = FullPop{S,G}(c)

function old_transition(env::InfiniteEnv, pop::FullPop)
    (;S, D) = env
    @assert env.G == S
    @assert env.agent_policy.b0c1 == 1.  # TODO: handle other cases
    @assert env.agent_policy.b0c0 == 0.

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
    FullPop{S, S}(C1, B1)
end

function transition(env::InfiniteEnv, pop::FullPop)
    (;S, G, D) = env
    @assert env.agent_policy.b0c1 == 1.  # TODO: handle other cases
    @assert env.agent_policy.b0c0 == 0.

    (;B, C) = pop
    
    B1 = similar(pop.B); C1 = similar(pop.C)
    for s in 1:S, g in 1:G
        p_task = 1/(S*G)
        p_not_bespoke = (1 - B[s,g])
        p_none_bespoke = p_not_bespoke^D

        p_s_single = sum(C[s, :])
        p_g_single = sum(C[:, g])
        p_either_single = p_s_single + p_g_single - C[s,g]
        p_either_single_conditional = p_either_single / p_not_bespoke
        p_either_any_conditional = 1 - (1 - p_either_single_conditional)^D
        
        C1[s,g] = p_task * p_none_bespoke * p_either_any_conditional
        B1[s,g] = p_task - C1[s,g]
    end
    FullPop{S,G}(C1, B1)
end


@kwdef struct FreqPop <: InfinitePop
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

cost(costs::Costs, pop::FreqPop) = sum(struct2vec(costs) .* struct2vec(pop))

function observation_probabilities(env::InfiniteEnv, pop::FreqPop)
    total = sum(struct2vec(pop))
    if total ≈ 0.
        observation_probabilities(env.S, env.G, 0, 0)
    else @assert total ≈ 1. "FreqPop must sum to 0 or 1"
        @invoke observation_probabilities(env, pop::InfinitePop)  # like python super()
    end
end

function transition(env::InfiniteEnv, pop::FreqPop)
    p_obs = observation_probabilities(env, pop)
    p_comp = p_obs .* env.agent_policy
    p_besp = p_obs .* 1 .- p_comp

    FreqPop(
        bespoke_zilch = sum(p_besp[1, :]),  # 1 -> b0
        bespoke_full = sum(p_besp[2, :]),
        comp_zilch = sum(p_comp[:, 1]),
        comp_partial = sum(p_comp[:, 2]),
        comp_full = sum(p_comp[:, 3]),
    )
end

function Base.show(io::IO, ::MIME"text/plain", pop::FreqPop)
    print(io, "FreqPop")
    fields = fieldnames(FreqPop)
    for f in fields
        val = getfield(pop, f)
        print(io, "\n  ", string(f), " = ", val)
    end
end