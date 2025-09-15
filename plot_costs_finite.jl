using Distributed
nprocs() == 1 && addprocs()

@everywhere include("model_finite.jl")

DEFAULT_PARALLEL = true
RESULTS_PATH = "results/cost-finite/"

# %% --------

@everywhere function get_env_costs(prm, policy=rational_policy)
    env_prm = subset(prm, fieldnames(FiniteModel))
    cost_prm = subset(prm, fieldnames(Costs))
    C = Costs(;cost_prm...)
    env = FiniteModel(; env_prm...)
    (env, C)
end

@everywhere function compute_evolution(prm, make_policy)
    env, C = get_env_costs(prm)
    env = mutate(env, agent_policy=make_policy(C))
    sim = simulate(env, 12)
    imap(sim) do gen, pop
        (;
            gen,
            cost = cost(C, pop),
            compositionality = compositional_rate(pop),
        )
    end
end

function reparametrize(prm)
    (;act_cost, search_cost, base_cost) = prm
    (;
        prm...,
        bespoke_zilch = base_cost,
        bespoke_full = 0,
        comp_zilch = act_cost + (base_cost + search_cost),
        comp_partial = act_cost + (base_cost + search_cost)/2,
        comp_full = act_cost,
    )
end

# %% --------

@everywhere noisy_rational_policy(C::Costs) = rational_policy(C; β=0.1, ε=0.05)


prm = (;
    S = 1,
    G = 5,
    D = 8,

    base_cost = 100,
    act_cost = 45,
    search_cost = 0,
) |> reparametrize

evol = dataframe([prm]) do prm
    compute_evolution(prm, noisy_rational_policy)
end
evol |> write_csv("evolution-simple.csv")


# # %% --------

# function reparametrize(prm)
#     (;act_cost, search_cost, base_cost) = prm
#     (;
#         prm...,
#         bespoke_zilch = base_cost,
#         bespoke_full = 0,
#         comp_zilch = act_cost + (base_cost + search_cost),
#         comp_partial = act_cost + (base_cost + search_cost)/2,
#         comp_full = act_cost,
#     )
# end


# prms = reparametrize.(grid(;
#     S = 1,
#     G = 100,
#     # β = 1.0,
#     # D = 1 .* 3 .^ (1:5),
#     # D = [2, 20, 100, 200],
#     # D = [80, 100, 120],
#     D = [2, 20, 100, 200],
#     base_cost = 100,
#     act_cost = 0:2:60,
#     search_cost = 0:2:30,
# ))


# df = dataframe(compute_costs, prms)
# @rtransform! df :asymptotic_advantage = (:bespoke_cost - :asymptotic_cost) / :base_cost
# println(minimum(df.asymptotic_advantage))
# prms[argmin(df.asymptotic_advantage)]

# df |> write_csv("costs-idealized.csv")
# dataframe(compute_evolution, prms) |> write_csv("evolution-idealized.csv")

# # %% --------

# prms = reparametrize.(grid(;
#     S = 1:14,
#     G = 1:14,
#     D = 1 .* 3 .^ (1:5),
#     # D = [2, 20, 100, 200],
#     # D = [80, 100, 120],
#     # D = [2, 20, 100, 200],
#     base_cost = 100,
#     act_cost = 49,
#     search_cost = 1,
# ))

# prms = filter(prms) do prm
#     prm.S * prm.G > 2 && prm.S ≤ prm.G
# end

# df = dataframe(compute_costs, prms)
# @rtransform! df :asymptotic_advantage = (:bespoke_cost - :asymptotic_cost) / :base_cost
# # println(minimum(df.asymptotic_advantage))
# # prms[argmin(df.asymptotic_advantage)]

# dataframe(compute_costs, prms) |> write_csv("costs-idealized-SG.csv")
# @time dataframe(compute_evolution, prms) |> write_csv("evolution-idealized-SG.csv")
