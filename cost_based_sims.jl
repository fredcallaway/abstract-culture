using Distributed
nprocs() == 1 && addprocs(8)

@everywhere begin
    
    include("binary_env.jl")
    include("utils.jl")
    include("figures.jl")
    include("box.jl")
    using DataFrames, CSV, JSON    
end

RESULTS_PATH = "results/cost/"
mkpath(RESULTS_PATH)
function write_csv(name, df)
    fp = RESULTS_PATH * name
    CSV.write(fp, df)
    println("Wrote $fp")
end
write_csv(name::String) = df -> write_csv(name, df)


@everywhere begin
    function estimate_final_state(env, costs; n_gen=100, n_pop=1000, init=0.0)
        total_cost = 0.; total_compositionality = 0.
        for _ in 1:n_pop
            sim = simulate(env, n_gen; init)
            pop = sim[end]
            total_cost += mean(beh -> cost(costs, beh), pop)
            total_compositionality += compositional_rate(pop)
        end
        (;cost=total_cost / n_pop, compositionality=total_compositionality / n_pop)
    end

    struct FixedPolicy <: AgentPolicy
        p_compositional::Float64
    end
    prob_compositional(pol::FixedPolicy, ::Info) = pol.p_compositional

    function estimate_compositionality_advantage(env, costs, agent_policy, fixed_ε;  kws...)
        env1 = mutate(env; agent_policy)
        free = estimate_final_state(env1, costs; kws...)

        env2 = mutate(env; agent_policy=FixedPolicy(fixed_ε))
        bespoke_only = estimate_final_state(env2, costs; kws...)

        (;
            bespoke_cost = bespoke_only.cost,
            free_cost = free.cost,
            free_comp = free.compositionality,
        )
    end

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

    function simulate_free_vs_fixed(env, costs; ε, kws...)
        env = mutate(env; agent_policy=rational_policy(costs; ε))
        free = simulate_many(env, costs; kws...)
        free.agent .= "rational"

        env = mutate(env; agent_policy=FixedPolicy(ε))
        fixed = simulate_many(env, costs; kws...)
        fixed.agent .= "bespoke"

        vcat(free, fixed)
    end
end

# %% ===== search over possible cost functions ================================

function run_cost_search(name, params; ε, n_gen)
    env = BinaryCompositionEnv(;S=4, D=32, N=32, replace_demos=false)

    search = dataframe(params, parallel=true) do x
        costs = Costs(;x...)
        estimate_compositionality_advantage(env, costs, rational_policy(costs; ε), ε; n_gen)
    end
    write_csv("search-flexible-$(name).csv", search)
    
    sim = let            
        all_costs = map(x->Costs(;x...), params)
        objective = @with search :free_cost ./ :bespoke_cost
        best_cost = all_costs[argmax(objective)]
        simulate_free_vs_fixed(env, best_cost; ε, n_gen=100)
    end
    write_csv("sim-flexible-$(name).csv", sim)
end


box = Box(
    comp_full = (1, 5),
    comp_partial = (1, 5),
    comp_none = (5, 10),
    bespoke_full = 1,
    bespoke_none = 5,
)

params = filter(grid(10, box)[:]) do x
    x.comp_full < x.comp_partial < x.comp_none
end

run_cost_search("pure", params; ε=.01, n_gen=50)
run_cost_search("noisy", params; ε=.13, n_gen=15)

# %% ===== empirical costs ====================================================

@everywhere function empirical_costs()
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

@everywhere function empirical_policy(file="tmp/compositional-rates-code-pilot-v23.csv")
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

# %% --------

params = @chain  begin
    grid(
        S = [4],
        N = 10:2:50,
        D = 10:2:50
    )
    reduce(vcat, _)
    filter(x -> x.N ≥ x.D, _)
end

search = dataframe(params, parallel=true) do (;S, D, N)
    env = BinaryCompositionEnv(;S, D, N, replace_demos=false)
    costs = empirical_costs()
    policy = empirical_policy()
    estimate_compositionality_advantage(env, costs, policy, 0.0; n_gen=15, n_pop=5000)
end
write_csv("search-empirical.csv", search)

# %% --------

objective = @with search :free_cost ./ :bespoke_cost
best_prm = params[argmax(objective)]
env = BinaryCompositionEnv(;best_prm..., replace_demos=false)


sim = let
    agents = (
        rational = rational_policy(empirical_costs(); ε=.01),
        empirical = empirical_policy(),
        fixed = FixedPolicy(0.),
    )
    mapreduce(vcat, pairs(agents)) do (name, pol)
        x = simulate_many(mutate(env, agent_policy=pol), empirical_costs(); n_pop=500, n_gen=30)
        x.agent .= name
        x
    end
end

write_csv("sim-empirical.csv", sim)

