include("utils.jl")

using StructArrays

const REUSABLE = 0

struct Agent
    problem::Int
    strategy::Int
end

@kwdef struct Env
    n_agent::Int = 100
    n_state::Int = 5
    p_reusable::Float64 = .5
    p_accept_reusable::Float64 = .5
    reusable_reward::Float64 = .5
end

function payoff(env, agent)
    if agent.strategy == REUSABLE
        env.reusable_reward
    else
        1. * (agent.problem == agent.strategy)
    end
end

function update!(pop, env, i::Int)
    pop.problem[i] = rand(1:env.n_state)
    pop.strategy[i] = rand(pop.strategy)  # social learning
    pay = payoff(env, pop[i])
    keep_strategy = pay == 1 || (pay > 0 && rand() < env.p_accept_reusable)
    if !keep_strategy
        pop.strategy[i] = rand() < env.p_reusable ? REUSABLE : pop.problem[i]
    end
end

function initial_population(env)
    (;n_agent, n_state) = env
    StructArray{Agent}((rand(0:n_state, n_agent), rand(0:n_state, n_agent)))
end

function update!(pop, env)
    for i in eachindex(pop)
        update!(pop, env, i)
    end
end

# %% --------

function get_curve(ps, p_accept_reusable)
    y = map(ps) do p
        env = Env(;p_reusable=p, n_state=10, p_accept_reusable)
        pop = initial_population(env)

        y = repeatedly(2000) do
            update!(pop, env)
            mean(pop.strategy .== 0)
        end
        mean(y[1001:end])
    end
end

figure(size=(350,300), xlabel="p(discover reusable)", ylabel="p(use reusable)") do
    ps = 0:.01:1
    lines!(ps, get_curve(ps, 0.), label="0%")
    lines!(ps, get_curve(ps, 0.5), label="50%")
    lines!(ps, get_curve(ps, 0.9), label="90%")
    lines!(ps, get_curve(ps, 1.), label="100%")
    axislegend(position=:rb)
end


# %% --------
y = repeatedly(1000) do
    update!(pop, env)
    mean(pop.strategy .== 0)
end

ys = repeatedly(20) do
    repeatedly(1000) do
        update!(pop, env)
        mean(pop.strategy .== 0)
    end
end

include("figure.jl")

figure() do
    lines!(y)
end

