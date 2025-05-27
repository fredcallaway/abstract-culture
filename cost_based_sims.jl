using Distributed
nprocs() == 1 && addprocs(8)

@everywhere begin
    
    include("binary_env.jl")
    include("utils.jl")
    include("figures.jl")
    include("box.jl")
    using DataFrames, CSV, JSON    

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

    function simulate_free_vs_fixed(env, costs; ε=0.01, β=100., kws...)
        env = mutate(env; agent_policy=rational_policy(costs; ε, β))
        free = simulate_many(env, costs; kws...)
        free.agent .= "rational"

        env = mutate(env; agent_policy=FixedPolicy(ε))
        fixed = simulate_many(env, costs; kws...)
        fixed.agent .= "bespoke"

        vcat(free, fixed)
    end

end

# %% ===== search =============================================================

env = BinaryCompositionEnv(;S=4, D=32, N=32, replace_demos=false)

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

# %% --------

results = dataframe(params, pbar=true) do x
    costs = Costs(;x...)
    estimate_compositionality_advantage(env, costs, rational_policy(costs; ε=0.13, β=100.), 0.13; n_gen=15)
end
results |> CSV.write("results/costs-flexible-noisy.csv")

best = @chain CSV.read("results/costs-flexible-noisy.csv", DataFrame) begin
    @rtransform :rel_cost = :free_cost / :bespoke_cost
    @subset :rel_cost .== maximum(:rel_cost)
    select(1:5)
    first
    NamedTuple
end

x = simulate_free_vs_fixed(env, Costs(;best...); ε=0.13, β=100., n_gen=30)
x |> CSV.write("results/sim-flexible-noisy.csv")

# %% --------

results = dataframe(params, pbar=true) do x
    costs = Costs(;x...)
    estimate_compositionality_advantage(env, costs, rational_policy(costs; ε=0.01, β=100.), 0.01; n_gen=100)
end
results |> CSV.write("results/costs-flexible-pure.csv")

best = @chain CSV.read("results/costs-flexible-pure.csv", DataFrame) begin
    @rtransform :rel_cost = :free_cost / :bespoke_cost
    @subset :rel_cost .== maximum(:rel_cost)
    select(1:5)
    first
    NamedTuple
end

x = simulate_free_vs_fixed(env, Costs(;best...); ε=0.01, β=100., n_gen=100)
x |> CSV.write("results/sim-flexible-pure.csv")

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

# %% --------

results = dataframe(params, parallel=true) do (;S, D, N)
    env = BinaryCompositionEnv(;S, D, N, replace_demos=false)
    costs = empirical_costs()
    policy = empirical_policy()
    estimate_compositionality_advantage(env, costs, policy, 0.0; n_gen=15, n_sim=5000)
end

results |> CSV.write("results/costs-empirical.csv")

# %% --------

best = @chain CSV.read("results/costs-empirical.csv", DataFrame) begin
    @rsubset :S == 4 && :N == 32 && :D == 32
    @rtransform :rel_cost = :free_cost / :bespoke_cost
    @subset :rel_cost .== maximum(:rel_cost)
    select(1:3)
    first
end

env = BinaryCompositionEnv(;best..., replace_demos=false)

agents = (
    rational = rational_policy(empirical_costs()),
    empirical = empirical_policy(),
    fixed = FixedPolicy(0.),
)

mapreduce(vcat, pairs(agents)) do (name, pol)
    x = simulate_many(mutate(env, agent_policy=pol), empirical_costs(); n_pop=500, n_gen=30)
    x.agent .= name
    x
end |> CSV.write("results/sim-empirical.csv")




# # %% ===== use dial model =====================================================

# function dial_costs(; action_cost=1, search_cost=4, n_bespoke=4, n_compositional=6)
#     Costs(
#         comp_full = n_compositional * action_cost,
#         comp_partial = n_compositional * (action_cost + search_cost / 2),
#         comp_none = n_compositional * (action_cost + search_cost),
#         bespoke_full = n_bespoke * action_cost,
#         bespoke_none = n_bespoke * (action_cost + search_cost),
#     )
# end

# let 
#     env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
#     costs = dial_costs(; action_cost=1, search_cost=2, n_bespoke=3, n_compositional=5)
#     policy = rational_policy(costs; ε=.01, β=100.)
#     display(policy.table)
#     estimate_compositionality_advantage(env, costs, policy, n_gen=50)
# end

# # %% ===== fitting dial cost ==================================================


# using Optim
# function fitted_dial_costs()
#     emp_costs = empirical_costs()

#     res = optimize([1., 10.]) do (action_cost, search_cost)
#         costs = dial_costs(; action_cost, search_cost)
#         sum(abs.(costs .- emp_costs) .^ 2)
#     end

#     action_cost, search_cost = res.minimizer
#     costs = dial_costs(; action_cost, search_cost)
#     (; costs, action_cost, search_cost)
# end

# # %% --------

# let 
#     env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
#     costs = fitted_dial_costs().costs
#     policy = rational_policy(costs; ε=.01, β=100.)
#     estimate_compositionality_advantage(env, costs, policy)
# end

# # %% ===== heatmaps by action and search cost =================================

# X = map(product(1:10, 1:10)) do (action_cost, search_cost)
#     costs = dial_costs(; action_cost, search_cost)
#     policy = rational_policy(costs; ε=.01, β=100.)
#     estimate_compositionality_advantage(env, costs, policy; n_gen=50)
# end

# Y = map(product(1:10, 1:10)) do (action_cost, search_cost)
#     costs = dial_costs(; action_cost, search_cost)
#     policy = rational_policy(costs; ε=.15, β=100.)
#     estimate_compositionality_advantage(env, costs, policy; n_gen=15)
# end

# figure() do
#     plot(
#         heatmap(getfield.(X, :free_comp), clim=(0,1)),
#         heatmap(getfield.(Y, :free_comp), clim=(0,1)),
#         size=(600, 200)
#     )
# end

# # %% ===== change n_dials =====================================================

# g = grid(;
#     search_cost=1:.2:5,
#     n_compositional=2:2:8,
#     n_bespoke=2:7
# )

# params = filter(collect(g)) do x
#     x.n_compositional > x.n_bespoke
# end

# results = @showprogress map(params) do (;search_cost, n_compositional, n_bespoke)
#     env = BinaryCompositionEnv(;S=4, D=32, K=1, N=32, replace_demos=false)
#     costs = dial_costs(; search_cost, n_compositional, n_bespoke)

#     policy = rational_policy(costs; ε=.01, β=100.)
#     model = estimate_compositionality_advantage(env, costs, policy; n_gen=50)
    
#     policy = rational_policy(costs; ε=.15, β=100.)
#     human = estimate_compositionality_advantage(env, costs, policy; n_gen=15)

#     (;search_cost, n_compositional, n_bespoke, model, human)
# end

# # %% --------

# good = filter(results) do r
#     all((
#         # r.model.free_comp > 0.5,
#         r.human.free_comp > 0.5,
#         # r.model.free_cost / r.model.bespoke_cost > 1.1,
#         r.human.free_cost / r.human.bespoke_cost > 1.1,
#     ))
# end

# dump(good[1])


# # %% ===== simulation for plotting ============================================

    

