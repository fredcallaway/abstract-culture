include("binary_env.jl")
include("utils.jl")
include("figures.jl")
using DataFrames, CSV, JSON

# %% ===== select good cost parameters ========================================

function estimate_final_state(env, costs; n_gen=10, n_sim=100, init=0.0)
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

function estimate_compositionality_advantage(env, costs, agent_policy;  kws...)
    env1 = mutate(env; agent_policy)
    free = estimate_final_state(env1, costs; kws...)

    env2 = mutate(env; agent_policy=FixedPolicy(0.0))
    bespoke_only = estimate_final_state(env2, costs; kws...)

    (;
        bespoke_cost = bespoke_only.cost,
        free_cost = free.cost,
        free_comp = free.compositionality,
    )
end

# %% ===== hand select ========================================================

figure() do
    costs = Costs(
        comp_full = 3,
        comp_partial = 4,
        comp_none = 8,
        bespoke_full = 1,
        bespoke_none = 5,
    )
    
    env = BinaryCompositionEnv(;S=4, D=32, N=32, replace_demos=false,
        agent_policy=rational_policy(costs; ε=.01, β=100.)
    )
    
    sim = simulate_many(env, costs, n_gen=50)
    @chain sim begin
        @groupby :gen
        @combine :cost = mean(:cost)
        @df plot!(:gen, :cost, label="rational")
    end

    @chain sim begin
        @groupby :gen
        @combine :compositionality = mean(:compositionality)
        @df plot!(:gen, :compositionality, label="compositionality")
    end

    env = BinaryCompositionEnv(;S=4, D=32, N=32, replace_demos=false,
        agent_policy=FixedPolicy(0.0)
    )
    
    @chain simulate_many(env, costs, n_gen=50) begin
        @groupby :gen
        @combine :cost = mean(:cost)
        @df plot!(:gen, :cost, label="fixed")
    end
end

# %% ===== empirical costs + rational policy ====================================

function empirical_costs()
    data = JSON.parsefile("r/tmp/empirical_costs.json")  # combines pilot-v27 and reg-v2
    cost_dict = Dict(d["full_solution_type"] => d["duration"] for d in data)
    Costs(
        comp_full = cost_dict["comp_full"],
        comp_partial = cost_dict["comp_partial"], 
        comp_none = cost_dict["comp_none"],
        bespoke_full = cost_dict["bespoke_full"],
        bespoke_none = cost_dict["bespoke_none"]
    )
end

let 
    env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
    costs = empirical_costs()
    policy = rational_policy(costs; ε=.01, β=100.)
    estimate_compositionality_advantage(env, costs, policy; n_gen=10)
end


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

let 
    env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
    costs = empirical_costs()
    # policy = parse_policy("tmp/compositional-rates-code-pilot-v23.csv")
    policy = parse_policy("r/tmp/compositional-rates-reg-v2.csv")
    estimate_compositionality_advantage(env, costs, policy; n_gen=10)
end

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

let 
    env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
    costs = dial_costs(; action_cost=1, search_cost=2, n_bespoke=3, n_compositional=5)
    policy = rational_policy(costs; ε=.01, β=100.)
    display(policy.table)
    estimate_compositionality_advantage(env, costs, policy, n_gen=50)
end

# %% --------

using Optim
function fitted_dial_costs()
    emp_costs = empirical_costs()

    res = optimize([1., 10.]) do (action_cost, search_cost)
        costs = dial_costs(; action_cost, search_cost)
        sum(abs.(costs .- emp_costs) .^ 2)
    end

    action_cost, search_cost = res.minimizer
    costs = dial_costs(; action_cost, search_cost)
    (; costs, action_cost, search_cost)
end

# %% --------


let 
    env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
    costs = fitted_dial_costs().costs
    policy = rational_policy(costs; ε=.01, β=100.)
    estimate_compositionality_advantage(env, costs, policy)
end

# %% --------

env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
(;action_cost, search_cost) = fitted_dial_costs()

costs = dial_costs(; action_cost, search_cost)
costs = costs ./ costs.bespoke_full

policy = rational_policy(costs; ε=.01, β=100.)
estimate_compositionality_advantage(env, costs, policy)

# TODO tweak costs to better match the ideal costs identified at the top

# %% ===== simulation for plotting ============================================
function simulate_many(env, costs; init=0., n_pop=100, n_gen=15)
    dataframe(grid(;pop=1:n_pop)) do (;pop)
        sim = simulate(env, n_gen; init)
        imap(sim) do gen, pop
            (;gen, 
             cost=mean(beh -> cost(costs, beh), pop), 
             compositionality=compositional_rate(pop)
            )
        end
    end
end
    

# df = simulate_many(mutate(env; agent_policy=rational_policy(costs; ε=0.1)))
# df.agent .= "rational"

# df2 = simulate_many(mutate(env; agent_policy=FixedPolicy(0.1)))
# df2.agent .= "bespoke"

# vcat(df, df2) |> CSV.write("data/cost_sim.csv")
