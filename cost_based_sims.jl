
include("binary_env.jl")
using DataFrames, CSV

# %% ===== select good cost parameters ========================================


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

# %% --------

costs = Costs(
    comp_full = 2,
    comp_partial = 3,
    comp_none = 4,
    bespoke_full = 1,
    bespoke_none = 4,
)


env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
estimate_compositionality_advantage(env, costs; ε=0.01)

# %% ===== simulation for plotting ============================================


function simulate_many(env; init=0., n_pop=500, n_gen=50)
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

