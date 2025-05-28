
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