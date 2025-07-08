using Distributed
nprocs() == 1 && addprocs()

@everywhere include("infinite_env.jl")

DEFAULT_PARALLEL = true

# %% --------

RESULTS_PATH = "results/cost/"
mkpath(RESULTS_PATH)
function write_csv(name, df)
    fp = RESULTS_PATH * name
    CSV.write(fp, df)
    println("Wrote $fp")
end
write_csv(name::String) = df -> write_csv(name, df)
read_csv(name::String) = CSV.read(RESULTS_PATH * name, DataFrame)


@everywhere function get_env_costs(prm)
    env_prm, cost_prm = split(prm, (:S, :D))
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
        asymptotic_cost = cost(C, simulate(env, 2, init=FreqPop(CompPop(asymptotic_compositionality)))[end]),
        comp_cost = cost(C, simulate(env, 2, init=FreqPop(CompPop(max_compositionality)))[end]),
        bespoke_cost = cost(C, simulate(mutate(env, agent_policy=bespoke_policy()), 2, init=FreqPop())[end]),
        # comp_cost = cost(C, simulate(mutate(env, agent_policy=comp_policy()), 2, init=FreqPop())[end]),
    )
end

function is_valid_cost(prm)
    # comp costs make sense internally
    prm.comp_full ≤ prm.comp_partial ≤ prm.comp_zilch &&
    # bespoke costs make sense internally
    prm.bespoke_full < prm.bespoke_zilch && 
    # bespoke preferred to comp given equal info
    prm.bespoke_zilch < prm.comp_zilch && prm.bespoke_full < prm.comp_full &&
    # copy compositional preferred to learning from scratch
    prm.comp_full < prm.bespoke_zilch
end

prms = filter(is_valid_cost, grid(;
    S = [5, 10],
    D = 1 .* 3 .^ (1:5),

    bespoke_zilch = 10,
    bespoke_full = 0,
    comp_zilch = 10.001,
    comp_partial = 0:11,
    comp_full = 0:11,
)[:])


dataframe(compute_costs, prms) |> write_csv("cost-asymptote-grid.csv")

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

# %% --------


env, C = get_env_costs((;S=10, D=81, 
    bespoke_zilch = 10,
    bespoke_full = 0,
    comp_zilch = 10.001,
    comp_partial = 11,
    comp_full = 9,
))

fixed_points(env)

# %% --------


prms = filter(is_valid_cost, grid(;
    S = 10,
    D = 1 .* 3 .^ (1:5),

    bespoke_zilch = 10,
    bespoke_full = 0,
    comp_zilch = 10.001,
    comp_partial = 9,
    comp_full = 0:11,
)[:])



dataframe(compute_costs, prms) |> write_csv("cost-asymptote-D-full.csv")

# %% --------

prms = filter(is_valid_cost, grid(;
    S = 2:20, 
    D = 2 .^ (1:9),
    bespoke_zilch = 10,
    bespoke_full = 0,
    comp_zilch = 100,
    comp_partial = [9,10,11],
    comp_full = 1:9,
)[:])

df = dataframe(compute_costs, prms) 

@show maximum(df.comp_cost)

df |> write_csv("cost-asymptote-SD.csv")


# %% --------
