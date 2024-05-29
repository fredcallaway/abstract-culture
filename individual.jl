@everywhere begin
    include("red_black.jl")

    @kwdef struct Costs
        black_travel = 0.
        red_travel = 0.1
        black_discovery = 1.
        red_discovery = 1.
    end

    numunique(x) = length(unique(x))
    numunique(f, x) = length(unique(f, x))

    function red_cost(costs, tasks)
        (numunique(first, tasks) + numunique(last, tasks)) * costs.red_discovery +
        length(tasks) * costs.red_travel
    end

    function black_cost(costs, tasks)
        numunique(tasks) * costs.black_discovery +
        length(tasks) * costs.black_travel
    end

    function red_advantage(env::RedBlackEnv, costs::Costs; reps=10000)
        monte_carlo(reps) do
            tasks = sample(taskdist(env), env.K; replace=true)
            black_cost(costs, tasks) - red_cost(costs, tasks)
        end
    end

end

function simulate(;S, K, red_travel, red_discovery)
    @showprogress pmap(grid(;S, K, red_travel, red_discovery)) do prm
        costs = Costs(;prm.red_travel, prm.red_discovery)
        (;prm..., advantage = red_advantage(RedBlackEnv(;prm.S, prm.K), costs))
    end
end

mkpath("tmp")
result = simulate(K=2:1:100, S=2:1:100, red_travel=[0., .05, .1, .2], red_discovery=[.5, .75, .95, 1])
serialize("tmp/individual", result)

# %% --------


# result = simulate(K=2:1:100, S=2:1:100)




