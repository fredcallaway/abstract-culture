include("infinite_env.jl")
include("utils.jl")
using Accessors


# %% --------

env = InfiniteEnv(p_0 = 0.0, p_r = 1.0)

function analytic_transition(env, c)
    (;S, D) = env
    b = 1 - c
    no_bespoke = (1 - b/S^2)^D
    p = (c * (2S - 1/S^2) / S^2) / (1 - b/S^2)
    any_comp = prob_observe(p, D)
    no_bespoke * any_comp
end

c = .5
transition(env, CompPop(c))
analytic_transition(env, c)

# %% --------

struct FullPop{S}
    C::SMatrix{S,S,Float64}
    B::SMatrix{S,S,Float64}
end

# %% --------

function transition(env::InfiniteEnv, pop::FullPop)
    (;S, D, p_0, p_r) = env

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

env = InfiniteEnv(p_0 = 0.0, p_r = 1.0)

for c in 0:.1:1
    S = env.S
    pop = FullPop{S}(
        (c/S^2) * ones(S, S), 
        ((1-c)/S^2) * ones(S, S)
    )
    @assert sum(pop.C) + sum(pop.B) ≈ 1
    pop1 = transition(env, pop)
    @assert sum(pop1.C) + sum(pop1.B) ≈ 1

    @assert sum(pop1.C) ≈ transition(env, CompPop(c)).comp
end

# %% --------
env = InfiniteEnv(p_0 = 0.0, p_r = 1.0, S = 3)
S = env.S

pop = FullPop{S}(
    zeros(S, S), 
    ones(S, S) ./ S^2
)
@reset pop.C[1,1] += .02
@reset pop.B[1,1] -= .02

@assert sum(pop.C) + sum(pop.B) ≈ 1

pop1 = transition(env, pop)
pop1.C
pop2 = transition(env, pop1)
pop2.C
pop3 = transition(env, pop2)
sum(pop3.C)

# %% --------







# %% --------

function new_transition(env::InfiniteEnv, pop::CompPop)
    (;S, D, p_0, p_r) = env

    c = pop.comp
    b = 1 - c
    c_ij = c / S^2
    b_ij = b / S^2

    no_bespoke = 1 - prob_observe(b_ij, D)

    # partial = prob_observe((c / S) / (1-b_ij), D)
    # both = partial ^ 2
    # either = 2partial - both

    any_comp_one = (c * (2S - 1) / S^2) / (1 - b_ij)
    any_comp = 1 - (1 - any_comp_one)^D
    
    c1 = no_bespoke * any_comp
    CompPop(c1)
end

for c in 0:.1:1
    old = transition(env, CompPop(c)).comp
    new = new_transition(env, CompPop(c)).comp
    @assert old ≈ new
end