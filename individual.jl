@everywhere begin
    include("red_black.jl")

    @kwdef struct Costs
        black_travel::Float64 = 0.
        red_travel::Float64 = 0.1
        black_discovery::Float64 = 1.
        red_discovery::Float64 = 1.
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


# %% --------
mkpath("tmp")
# previous:  K=2:1:100, S=2:1:100, red_travel=[0., .05, .1, .2], red_discovery=[.5, .75, .95, 1]
g = collect(grid(K=1:1:100, S=1:1:100, red_travel=[0., .05], red_discovery=[1., .9]))
result = dataframe(g; parallel=true) do prm
    costs = Costs(;prm.red_travel, prm.red_discovery)
    (;advantage = red_advantage(RedBlackEnv(;prm.S, prm.K), costs))
end
serialize("tmp/individual-jun14", result)
