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

    base_cost = 100,  # print time plus search time
    act_cost = 20:5:50,  # print time
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

@show rational_policy(C; β=.1, ε=0.1)

# %% --------

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

# %% ===== empirical predicted ================================================


function parse_policy_cost(version="v3")
    data = read_csv("tmp/duration-policy-cost-pilot-$version.csv")
    
    function parse_info(row)
        bespoke = row.bespoke == "zilch" ? 0 : 1
        comp = row.compositional == "zilch" ? 0 : row.compositional == "partial" ? 1 : 2
        (bespoke, comp)
    end
    
    pol_vals = Dict()
    cost_vals = Dict()
    
    for row in eachrow(data)
        b, c = parse_info(row)
        pol_vals[(b, c)] = row.policy
        cost_vals[(b, c)] = row.duration
    end
    
    pol = InfoRates(
        b0c0 = pol_vals[(0, 0)],
        b1c0 = pol_vals[(1, 0)],
        b0c1 = pol_vals[(0, 1)],
        b1c1 = pol_vals[(1, 1)],
        b0c2 = pol_vals[(0, 2)],
        b1c2 = pol_vals[(1, 2)],
    )
    
    costs = Costs(
        bespoke_zilch = cost_vals[(0, 0)],
        bespoke_full = cost_vals[(1, 0)],
        comp_zilch = cost_vals[(0, 0)],
        comp_partial = cost_vals[(0, 1)],
        comp_full = cost_vals[(0, 2)],
    )
    
    (pol, costs)
end

# %% --------


pol, C = parse_policy_cost()
env = FiniteModel(S=1, G=7, D=6, N=50, agent_policy=pol)

simulate(env, 2) .|> compositional_rate
initial_population(env) |> compositional_rate

filename = "tmp/empirical_predictions.csv"
flatmap(1:100) do rep
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

