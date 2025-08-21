using Distributed
nprocs() == 1 && addprocs()

@everywhere include("infinite_env.jl")

# %% --------

DEFAULT_PARALLEL = true
RESULTS_PATH = "results/cost-SG/"
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
    pol_prm = subset(prm, (:ε, :β))
    C = Costs(;cost_prm...)
    pol = rational_policy(C; pol_prm...)
    env = InfiniteEnv(pol; env_prm...)
    (env, C)
end

@everywhere function compute_costs(prm)
    env, C = get_env_costs(prm)

    asymptotic_compositionality, max_compositionality = let
        fixed = fixed_points(env)
        if isempty(fixed)
            return missing
        elseif length(fixed) == 1
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

@everywhere function compute_evolution(prm)
    env, C = get_env_costs(prm)
    sim = simulate(env, 100; init=FreqPop(CompPop(1e-6)))
    imap(sim) do gen, pop
        (;
            gen,
            cost = cost(C, pop),
            compositionality = compositional_rate(pop),
        )
    end
end

# %% --------

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


prms = reparametrize.(grid(;
    S = 1,
    G = 100,
    β = 1.0,
    # D = 1 .* 3 .^ (1:5),
    # D = [2, 20, 100, 200],
    # D = [80, 100, 120],
    D = [2, 20, 100, 200],
    base_cost = 100,
    act_cost = 0:2:60,
    search_cost = 0:2:30,
))


df = dataframe(compute_costs, prms)
@rtransform! df :asymptotic_advantage = (:bespoke_cost - :asymptotic_cost) / :base_cost
println(minimum(df.asymptotic_advantage))
prms[argmin(df.asymptotic_advantage)]

# %% --------

dataframe(compute_costs, prms) |> write_csv("costs-idealized.csv")
@time dataframe(compute_evolution, prms) |> write_csv("evolution-idealized.csv")

# %% --------

prms = reparametrize.(grid(;
    S = 1:14,
    G = 1:14,
    # D = 1 .* 3 .^ (1:5),
    # D = [2, 20, 100, 200],
    # D = [80, 100, 120],
    D = [2, 20, 100, 200],
    base_cost = 100,
    act_cost = 49,
    search_cost = 1,
))

prms = filter(prms) do prm
    prm.S * prm.G > 2
end


df = dataframe(compute_costs, prms)
@rtransform! df :asymptotic_advantage = (:bespoke_cost - :asymptotic_cost) / :base_cost
println(minimum(df.asymptotic_advantage))
prms[argmin(df.asymptotic_advantage)]

# %% --------
dataframe(compute_costs, prms) |> write_csv("costs-idealized-SG.csv")
@time dataframe(compute_evolution, prms) |> write_csv("evolution-idealized-SG.csv")

# %% --------

prms = reparametrize.(grid(;
    S = 1:14,
    G = 1:14,
    # β = 1.,
    # D = 1 .* 3 .^ (1:5),
    # D = [2, 20, 100, 200],
    # D = [80, 100, 120],
    D = 20,
    base_cost = 100,
    act_cost = [40, 45, 50],
    search_cost = [0, 1, 2],
))

prms = filter(prms) do prm
    prm.S * prm.G > 2
end


df = dataframe(compute_costs, prms)
@rtransform! df :asymptotic_advantage = (:bespoke_cost - :asymptotic_cost) / :base_cost
println(minimum(df.asymptotic_advantage))
prms[argmin(df.asymptotic_advantage)]

dataframe(compute_costs, prms) |> write_csv("costs-idealized-SG-big.csv")
@time dataframe(compute_evolution, prms) |> write_csv("evolution-idealized-SG-big.csv")

# %% --------

# prms = reparametrize.(grid(;
#     S = 4,
#     D = 2 .^ (1:6),
#     ε = 0.05,
#     β = 2.0,

#     act_cost = 0:10,
#     search_cost = 0:2:24,
# ))

# dataframe(compute_costs, prms) |> write_csv("costs-predicted.csv")
# @time dataframe(compute_evolution, prms) |> write_csv("evolution-predicted.csv")


# # %% --------

# prm = reparametrize((;act_cost=0, search_cost=0))
# Costs(; subset(prm, fieldnames(Costs))...)