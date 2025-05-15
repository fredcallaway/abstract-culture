
include("binary_env.jl")
using DataFrames, CSV

# %% --------

logistic(x) = 1 / (1 + exp(-x))
lapse(p, ε) = (1 - ε) * p + ε / 2

@kwdef struct Costs
    action_cost::Float64 = 1
    search_cost::Float64 = 4
    n_bespoke::Int = 4
    n_compositional::Int = 6
end

function cost(C::Costs, beh::Behavior)
    if beh.compositional
        fraction_learned = beh.learning / 2
        C.n_compositional * (C.action_cost + fraction_learned * C.search_cost)
    else
        C.n_bespoke * (C.action_cost + beh.learning * C.search_cost)
    end
end

function cost_table(costs::Costs=Costs())
    map(product(0:1, 0:2, false:true)) do (bespoke_known, compositional_known, do_comp)
        beh = if do_comp
            Behavior(-1, -1, true, 2 - compositional_known)
        else
            Behavior(-1, -1, false, 1 - bespoke_known)
        end
        cost(costs, beh)
    end
end

# %% --------

function get_rational_policy(costs, β, ε)
    map(diff(cost_table(costs), dims=3)[:, :]) do bespoke_advantage
        p = logistic(β * -bespoke_advantage)
        lapse(p, ε)
        # bespoke_cost - compositional_cost
        # bespoke_cost, compositional_cost
    end |> TabularPolicy
end

costs = Costs()
softmax_policy = get_rational_policy(costs, 0.3, 0.05)

softmax_policy.table


# %% --------

function simulate_costs(; search_cost, n_bespoke, n_compositional, D, N, β=0.3, ε=0.0, n_pop=500, n_gen=20)
    # D = [2, 3, 4, 8, 16, 24, 32]
    costs = Costs(;action_cost=1, search_cost, n_bespoke, n_compositional)

    df = dataframe(grid(;D, N, pop=1:n_pop)) do (;D, N, pop)
        D > N && return missing
        agent_policy = get_rational_policy(costs, β, ε)
        env = BinaryCompositionEnv(;S=4, D, K=1, N, agent_policy, replace_demos=false)
        sim = simulate(env, n_gen)
        imap(sim) do gen, pop
            (;gen, 
             cost=mean(beh -> cost(costs, beh), pop), 
             compositionality=compositional_rate(pop)
            )
        end
    end
end
ls
simulate_costs(search_cost=4, n_bespoke=4, n_compositional=6, D=32, N=100, n_pop=10)
