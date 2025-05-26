include("binary_env.jl")
using DataFrames, CSV, JSON

# %% ===== select good cost parameters ========================================

function estimate_final_state(env; n_gen=10, n_sim=100, init=0.0)
    total_cost = 0.; total_compositionality = 0.
    for _ in 1:n_sim
        sim = simulate(env, n_gen; init)
        pop = sim[end]
        # HACK FIX ME costs is global
        total_cost += mean(beh -> cost(costs, beh), pop)
        total_compositionality += compositional_rate(pop)
    end
    (;cost=total_cost / n_sim, compositionality=total_compositionality / n_sim)
end

struct FixedPolicy <: AgentPolicy
    p_compositional::Float64
end
prob_compositional(pol::FixedPolicy, ::Info) = pol.p_compositional

function estimate_compositionality_advantage(env1::BinaryCompositionEnv; kws...)
    free = estimate_final_state(env1; kws...)

    env2 = mutate(env; agent_policy=FixedPolicy(0.0))
    bespoke_only = estimate_final_state(env2; kws...)

    (;
        bespoke_cost = bespoke_only.cost,
        free_cost = free.cost,
        free_comp = free.compositionality,
    )
    # map(-, free, bespoke_only)
end

function estimate_compositionality_advantage(env::BinaryCompositionEnv, costs::Costs; β=100., ε=0.0, kws...)
    env1 = mutate(env, agent_policy=rational_policy(costs; β, ε))
    estimate_compositionality_advantage(env1)
end

# %% ===== hand select ========================================================

costs = Costs(
    comp_full = 2,
    comp_partial = 3,
    comp_none = 4,
    bespoke_full = 1,
    bespoke_none = 4,
)

env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
estimate_compositionality_advantage(env, costs; ε=0.01, n_gen=10)


# %% ===== use dial model =====================================================

function dial_costs(; action_cost=1, search_cost=4, n_bespoke=4, n_compositional=6)
    Costs(
        comp_full = n_compositional * action_cost,
        comp_partial = n_compositional * (action_cost + search_cost / 2),
        comp_none = n_compositional * (action_cost + search_cost),
        bespoke_full = n_bespoke * action_cost,
        bespoke_none = n_bespoke * (action_cost + search_cost),
    )
end

dcosts = dial_costs()

env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
estimate_compositionality_advantage(env, dcosts; ε=0.01)

# %% ===== empirical costs + rational policy ====================================

function empirical_costs()
    data = JSON.parsefile("r/tmp/empirical_costs.json")
    cost_dict = Dict(d["solution_type"] => d["duration"] for d in data)
    Costs(
        comp_full = cost_dict["comp_full"],
        comp_partial = cost_dict["comp_partial"], 
        comp_none = cost_dict["comp_none"],
        bespoke_full = cost_dict["bespoke_full"],
        bespoke_none = cost_dict["bespoke_none"]
    )
end

costs = empirical_costs()
rational_policy(costs).table

env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
estimate_compositionality_advantage(env, costs; ε=0.1, n_gen=10)

# %% ===== pure empirical =====================================================

function parse_policy(file)
    df = CSV.read(file, DataFrame)
    B = ["zilch", "exact"]
    C = ["zilch", "partial", "full", "exact"]
    if "exact" ∉ df.compositional
        C[end] = "full"
    end
    map(product(B, C)) do (b, c)
        row = filter(df) do row
            row.bespoke == b && row.compositional == c
        end |> only
        row.p_compositional
    end |> TabularPolicy
end

human_policy = parse_policy("tmp/compositional-rates-code-pilot-v23.csv")
estimate_compositionality_advantage(mutate(env, agent_policy=human_policy), n_gen=10)


# %% ===== simulation for plotting ============================================

function simulate_many(env; init=0., n_pop=500, n_gen=15)
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

df = simulate_many(mutate(env; agent_policy=rational_policy(costs; ε=0.1)))
df.agent .= "rational"

df2 = simulate_many(mutate(env; agent_policy=FixedPolicy(0.1)))
df2.agent .= "bespoke"

vcat(df, df2) |> CSV.write("data/cost_sim.csv")
