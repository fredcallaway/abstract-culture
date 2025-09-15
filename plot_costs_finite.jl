using Distributed
nprocs() == 1 && addprocs()

@everywhere include("model_finite.jl")

DEFAULT_PARALLEL = true
RESULTS_PATH = "results/cost/"

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

prms = reparametrize.(grid(;
    S = 1,
    G = 6:8,
    N = 50,
    D = 1 .* (4:2:10),
    base_cost = 100,
    act_cost = 20:5:50,
    search_cost = 0:5:30,
    # act_cost = 40:2:50,
    # search_cost = 0:2:10,
    pop = 1:100
))


evol = dataframe(prms) do prm
    compute_evolution(prm, noisy_rational_policy)
end
evol |> write_csv("evolution-finite.csv")
