include("infinite_env.jl")

@broadcastable @kwdef struct Costs
    bespoke_zilch::Float64
    bespoke_full::Float64
    comp_zilch::Float64
    comp_partial::Float64
    comp_full::Float64
end

function cost(costs::Costs, pop::FreqPop)
    costs.bespoke_zilch * pop.bespoke_zilch +
    costs.bespoke_full * pop.bespoke_full +
    costs.comp_zilch * pop.comp_zilch +
    costs.comp_partial * pop.comp_partial +
    costs.comp_full * pop.comp_full
end

function Base.show(io::IO, ::MIME"text/plain", costs::Costs)
    print(io, "Costs")
    fields = fieldnames(Costs)
    for f in fields
        val = getfield(costs, f)
        print(io, "\n  ", string(f), " = ", val)
    end
end


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


prms = grid(;
    S = [5, 10],
    D = 1 .* 3 .^ (1:5),

    bespoke_zilch = 10,
    bespoke_full = 0,
    comp_zilch = 100,
    comp_partial = 0:11,
    comp_full = 0:11,
)[:]
filter!(prms) do prm
    # comp costs make sense internally
    prm.comp_full ≤ prm.comp_partial < prm.comp_zilch &&
    # bespoke costs make sense internally
    prm.bespoke_full < prm.bespoke_zilch && 
    # bespoke preferred to comp given equal info
    prm.bespoke_zilch < prm.comp_zilch && prm.bespoke_full < prm.comp_full
    # copy compositional preferred to learning from scratch
    prm.comp_full < prm.bespoke_zilch
end

comp_cost(env, costs, c::Float64) = cost(costs, transition(env, FreqPop(CompPop(c))))
comp_cost(env, costs, c::Missing) = missing


df = dataframe(prms) do prm
    env_prm, cost_prm = split(prm, (:S, :D))
    costs = Costs(;cost_prm...)
    p_r = logistic(10 * sign(costs.bespoke_zilch - costs.comp_partial))
    env = InfiniteEnv(;env_prm..., p_0=1e-10, p_r)
    
    asymptotic_compositionality = let
        fixed = fixed_points(env)
        if length(fixed) == 1
            only(fixed)
        else
            try
                first(c for c in fixed if c > 1e-10)
            catch
                transition(env, .001) < .001 ? 0. :
                transition(env, .999) > .999 ? 1. :
                error("huh")
            end
        end
    end
    
    (;
        asymptotic_compositionality,
        asymptotic_cost = comp_cost(env, costs, asymptotic_compositionality),
        bespoke_cost = comp_cost(env, costs, 0.),
        comp_cost = comp_cost(env, costs, 1.),
    )
end

df |> write_csv("cost-asymptote-grid.csv")

# %% --------

df = dataframe(prms) do prm
    env_prm, cost_prm = split(prm, (:S, :D))
    costs = Costs(;cost_prm...)
    p_r = logistic(costs.bespoke_zilch - costs.comp_partial)
    env = InfiniteEnv(;env_prm..., p_0=1e-10, p_r)

    sim = simulate(env, 100; init=FreqPop(bespoke_zilch=1.))
    imap(sim) do gen, pop
        (;
            gen,
            cost = cost(costs, pop),
            compositionality = compositional_rate(pop),
        )
    end    
end

df |> write_csv("evolution.csv")
