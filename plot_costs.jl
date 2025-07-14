using Distributed
nprocs() == 1 && addprocs()

@everywhere include("infinite_env.jl")

# %% --------

DEFAULT_PARALLEL = true
RESULTS_PATH = "results/cost/"
mkpath(RESULTS_PATH)

function write_csv(name, df)
    fp = RESULTS_PATH * name
    CSV.write(fp, df)
    println("Wrote $fp")
end
write_csv(name::String) = df -> write_csv(name, df)
read_csv(name::String) = CSV.read(RESULTS_PATH * name, DataFrame)

# %% --------

@everywhere function get_env_costs(prm)
    env_prm = subset(prm, fieldnames(InfiniteEnv))
    cost_prm = subset(prm, fieldnames(Costs))
    C = Costs(;cost_prm...)
    pol = rational_policy(C; ε=1e-10)
    env = InfiniteEnv(pol; env_prm...)
    (env, C)
end

@everywhere function compute_costs(prm)
    env, C = get_env_costs(prm)

    asymptotic_compositionality, max_compositionality = let
        fixed = fixed_points(env)
        if length(fixed) == 1
            c = only(fixed)
            (c, c)
        else
            mc = last(fixed)
            ac = try
                first(c for c in fixed if c > 1e-10)
            catch
                transition(env, .001) < .001 ? 0. :
                transition(env, .999) > .999 ? 1. :
                error("huh")
            end
            (ac, mc)
        end
    end

    (;
        asymptotic_compositionality,
        max_compositionality,
        asymptotic_cost = cost(C, simulate(env, 2, init=FreqPop(CompPop(asymptotic_compositionality)))[end]),
        max_cost = cost(C, simulate(env, 2, init=FreqPop(CompPop(max_compositionality)))[end]),
        bespoke_cost = cost(C, simulate(mutate(env, agent_policy=bespoke_policy()), 2, init=FreqPop())[end]),
        comp_cost = cost(C, simulate(mutate(env, agent_policy=comp_policy()), 2, init=FreqPop())[end]),
    )
end

# %% --------

prms = grid(;
    S = 5,
    D = 1 .* 3 .^ (1:5),

    act_cost = 0:10,
    search_cost = 0:2:24,
)

prms = map(prms) do prm
    (;act_cost, search_cost) = prm
    (;
        prm...,
        bespoke_zilch = 10,
        bespoke_full = 0,
        comp_zilch = act_cost + search_cost,
        comp_partial = act_cost + search_cost/2,
        comp_full = act_cost,
    )
end

dataframe(compute_costs, prms) |> write_csv("costs.csv")

dataframe(prms) do prm
    env, C = get_env_costs(prm)

    sim = simulate(env, 100; init=FreqPop(CompPop(1e-6)))
    imap(sim) do gen, pop
        (;
            gen,
            cost = cost(C, pop),
            compositionality = compositional_rate(pop),
        )
    end    
end |> write_csv("evolution.csv")