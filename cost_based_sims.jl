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

# %% ===== fitting dial cost ==================================================


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

# %% ===== heatmaps by action and search cost =================================

X = map(product(1:10, 1:10)) do (action_cost, search_cost)
    costs = dial_costs(; action_cost, search_cost)
    policy = rational_policy(costs; ε=.01, β=100.)
    estimate_compositionality_advantage(env, costs, policy; n_gen=50)
end

Y = map(product(1:10, 1:10)) do (action_cost, search_cost)
    costs = dial_costs(; action_cost, search_cost)
    policy = rational_policy(costs; ε=.15, β=100.)
    estimate_compositionality_advantage(env, costs, policy; n_gen=15)
end

figure() do
    plot(
        heatmap(getfield.(X, :free_comp), clim=(0,1)),
        heatmap(getfield.(Y, :free_comp), clim=(0,1)),
        size=(600, 200)
    )
end

# %% ===== change n_dials =====================================================

(;action_cost, search_cost) = fitted_dial_costs()

g = grid(;
    search_cost=1:.2:5,
    n_compositional=2:2:8,
    n_bespoke=2:7
)

params = filter(collect(g)) do x
    x.n_compositional > x.n_bespoke
end

results = @showprogress map(params) do (;search_cost, n_compositional, n_bespoke)
    env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
    costs = dial_costs(; search_cost, n_compositional, n_bespoke)

    policy = rational_policy(costs; ε=.01, β=100.)
    model = estimate_compositionality_advantage(env, costs, policy; n_gen=50)
    
    policy = rational_policy(costs; ε=.15, β=100.)
    human = estimate_compositionality_advantage(env, costs, policy; n_gen=15)

    (;search_cost, n_compositional, n_bespoke, model, human)
end

# %% --------

good = filter(results) do r
    all((
        # r.model.free_comp > 0.5,
        r.human.free_comp > 0.5,
        # r.model.free_cost / r.model.bespoke_cost > 1.1,
        r.human.free_cost / r.human.bespoke_cost > 1.1,
    ))
end

dump(good[1])


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
