using Distributed
nprocs() == 1 && addprocs()

@everywhere include("model_finite.jl")

DEFAULT_PARALLEL = true
@everywhere RESULTS_PATH = "tmp/evolution-finite"

# %% --------

@everywhere function get_env_costs(prm)
    env_prm = subset(prm, fieldnames(FiniteModel))
    cost_prm = subset(prm, fieldnames(Costs))
    pol_prm = subset(prm, (:ε, :β))
    C = Costs(;cost_prm...)
    agent_policy = rational_policy(C; pol_prm...)
    env = FiniteModel(; env_prm..., agent_policy)
    (env, C)
end

@everywhere function write_evolution_csv(prm; repeats=1000)
    env, C = get_env_costs(prm)
    
    filename = "$RESULTS_PATH/$(hash(prm)).csv"
    flatmap(1:repeats) do rep
        sim = simulate(env, 12)
        imap(sim) do gen, pop
            (;
                pop=rep,
                gen,
                cost = cost(C, pop),
                compositionality = compositional_rate(pop),
            )
        end
    end |> DataFrame |> write_csv(filename; quiet=true)
    "$(hash(prm)).csv"
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

prms = reparametrize.(grid(;
    S = 1,
    G = 6:8,
    N = 50,
    # D = 1 .* (4:2:10),
    D = 4:9,

    base_cost = 100,
    act_cost = 20:5:50,
    search_cost = 0:5:30,

    β = 0.1,
    ε = 0.05
))

index = dataframe(prms) do prm
    filename = write_evolution_csv(prm; repeats=1000)
    (;prm..., filename)
end
index |> write_csv("$RESULTS_PATH/index.csv")

# %% --------

prm = (;
    S = 1,
    G = 7,
    D = 6,
    act_cost = 40,
    search_cost = 0,
    base_cost = 100,
    N=50,
    β = 0.1,
    ε = 0.05
)
env, C = get_env_costs(reparametrize(prm))

version = "cost-pilot-v1"
shape_sets = "7" .* join.(product("ABC", "123"))[:]

solutions = map(product(0:1, shape_sets)) do (rep, shape_set)
    gen = [1,6][rep+1]
    sim = simulate(env, 10)
    id = "$version-$rep-$shape_set"
    solutions = map(sim[gen][:]) do beh
        (
            task = join((beh.s, beh.g)),
            kind = beh.compositional ? "compositional" : "bespoke"
        )    
    end
    id => solutions
end |> Dict

using JSON
write("/Users/fred/projects/culture/machine-task/stimuli/solutions-g0.json", json(solutions))

