using Roots
using Distributions

include("model_shared.jl")

struct InfiniteModel
    S::Int  # number of starts
    G::Int  # number of goals
    D::Int  # number of demonstrations
    agent_policy::InfoRates
end

InfiniteModel(;S, D, G=S, kws...) = InfiniteModel(S, G, D, agent_policy(;kws...))
InfiniteModel(pol::InfoRates; S, D, G=S) = InfiniteModel(S, G, D, pol)

abstract type InfinitePop end

# %% ===== social observations ================================================

#  OLD VERSION WITHOUT G
# function observation_probabilities(S, D_comp, D_bespoke)
#     d_bespoke = Bernoulli(prob_observe(1 / S^2, D_bespoke))
#     d_comp = Binomial(2, prob_observe(1 / S, D_comp))

#     # SVector prevents memory allocation
#     p_bespoke = pdf(d_bespoke, SVector(0,1))
#     p_comp = pdf(d_comp, SVector(0,1,2))
    
#     InfoRates(p_bespoke * p_comp')
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
    
    InfoRates(p_bespoke * p_comp')
end

function observation_probabilities(env::InfiniteModel, pop::InfinitePop)
    c = ensure_prob(compositional_rate(pop))
    expectation(Binomial(env.D, c)) do D_comp
        D_bespoke = env.D - D_comp
        observation_probabilities(env.S, env.G, D_comp, D_bespoke)
    end
end


# %% ===== population dynamics ================================================

@kwdef struct CompPop{T<:Real} <: InfinitePop
    comp::T
end

compositional_rate(pop::CompPop) = pop.comp

function transition(env::InfiniteModel, pop::CompPop)
    CompPop(sum(observation_probabilities(env, pop) .* env.agent_policy))
end

function simulate(env::InfiniteModel, n_gen; init=0.)
    gen0 = init isa Real ? CompPop(init) : init

    x = fill(gen0, n_gen+1)
    for i in 1:n_gen
        x[i+1] = transition(env, x[i])
    end
    x
end

function fixed_points(env::InfiniteModel; raw=false)
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
transition(env::InfiniteModel, c::Real) = transition(env, CompPop(c)).comp


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

function transition(env::InfiniteModel, pop::FullPop)
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

function observation_probabilities(env::InfiniteModel, pop::FreqPop)
    total = sum(struct2vec(pop))
    if total ≈ 0.
        observation_probabilities(env.S, env.G, 0, 0)
    else @assert total ≈ 1. "FreqPop must sum to 0 or 1"
        @invoke observation_probabilities(env, pop::InfinitePop)  # like python super()
    end
end

function transition(env::InfiniteModel, pop::FreqPop)
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