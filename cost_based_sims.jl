
include("binary_env.jl")
using DataFrames, CSV

# %% --------



# @kwdef struct DialCosts
#     action_cost = 1
#     search_cost::Float64 = 4
#     n_bespoke::Int = 4
#     n_compositional::Int = 6
# end

# function cost(C::DialCosts, beh::Behavior)
#     if beh.compositional
#         fraction_learned = beh.learning / 2
#         C.n_compositional * (C.action_cost + fraction_learned * C.search_cost)
#     else
#         C.n_bespoke * (C.action_cost + beh.learning * C.search_cost)
#     end
# end

# %% --------

function dial_costs(; action_cost=1, search_cost=4, n_bespoke=4, n_compositional=6)
    Costs(
        comp_full = n_compositional * action_cost,
        comp_partial = n_compositional * (action_cost + search_cost / 2),
        comp_none = n_compositional * (action_cost + search_cost),
        bespoke_full = n_bespoke * action_cost,
        bespoke_none = n_bespoke * (action_cost + search_cost),
    )
end


C = dial_costs()

map(product(0:1, 0:2)) do (bespoke_known, comp_known)
    info = Info(bespoke_known, comp_known)
    rel_cost = cost(C, info, false)
end
# %% --------

pol = rational_policy(C, 0.3, 0.05)



# %% --------

function simulate_costs(; search_cost, n_bespoke, n_compositional, D, N, β=0.3, ε=0.0, n_pop=500, n_gen=20)
    # D = [2, 3, 4, 8, 16, 24, 32]
    costs = dial_costs(;action_cost=1, search_cost, n_bespoke, n_compositional)

    df = dataframe(grid(;D, N, pop=1:n_pop)) do (;D, N, pop)
        D > N && return missing
        agent_policy = rational_policy(costs, β, ε)
        env = BinaryCompositionEnv(;S=4, D, K=1, N, agent_policy, replace_demos=false)
        sim = simulate(env, n_gen)
        imap(sim) do gen, pop
            (;gen, 
             cost=mean(beh -> cost(costs, beh), pop), 
             compositionality=compositional_rate(pop)
            )
        end
    end
end

df = simulate_costs(search_cost=4, n_bespoke=4, n_compositional=6, D=32, N=100, n_pop=100)
CSV.write("data/cost_sim.csv", df)

# %% --------
costs = dial_costs(;action_cost=1, search_cost=4, n_bespoke=4, n_compositional=6)

function estimate_final_state(env; n_gen=30, n_sim=100, init=0.0)
    total_cost = 0.; total_compositionality = 0.
    for _ in 1:n_sim
        sim = simulate(env, n_gen; init)
        pop = sim[end]
        total_cost += mean(beh -> cost(costs, beh), pop)
        total_compositionality += compositional_rate(pop)
    end
    (;cost=total_cost / n_sim, compositionality=total_compositionality / n_sim)
end

struct FixedPolicy <: AgentPolicy
    p_compositional::Float64
end
prob_compositional(pol::FixedPolicy, ::Info) = pol.p_compositional

function estimate_compositionality_advantage(env, costs; β=100., ε=0.0, kws...)
    env1 = mutate(env, agent_policy=rational_policy(costs; β, ε))
    free = estimate_final_state(env1; kws...)

    env2 = mutate(env; agent_policy=FixedPolicy(0.0))
    bespoke_only = estimate_final_state(env2; kws...)

    map(-, free, bespoke_only)
end

costs = Costs(
    comp_full = 1,
    comp_partial = 2,
    comp_none = 3,
    bespoke_full = 0,
    bespoke_none = 3,
)

rational_policy(costs).table

# %% --------

env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
estimate_compositionality_advantage(env, costs; ε=0.01)

# %% --------

function simulate_many(env; init=0., n_pop=500, n_gen=20)
    # D = [2, 3, 4, 8, 16, 24, 32]

    df = dataframe(grid(;pop=1:n_pop)) do (;pop)
        sim = simulate(env, n_gen; init)
        imap(sim) do gen, pop
            (;gen, 
             cost=mean(beh -> cost(costs, beh), pop), 
             compositionality=compositional_rate(pop)
            )
        end
    end
end

df = simulate_many(mutate(env; agent_policy=rational_policy(costs; ε=0.01)))
df.agent .= "rational"

df2 = simulate_many(mutate(env; agent_policy=FixedPolicy(0.01)))
df2.agent .= "bespoke"

vcat(df, df2) |> CSV.write("data/cost_sim.csv")

# %% --------
