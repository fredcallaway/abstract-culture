include("black_red.jl")

using BenchmarkTools
using ProfileView

# %% --------

env = Environment(;k=10, n=5, m=10, ε=0.)
@btime sim = simulate(env, 100; state=initial_population(env, 100));

@time simulate(env, 100; state=initial_population(env, 200));

@profview simulate(env, 100; state=initial_population(env, 200));

# %% --------

function bench_comp(jobs; mode=:both)
    map(jobs) do (tasks, observed)
        find_compositions(env, tasks, observed)
    end
end

env = Environment(k=10, m=10, n=20; ε=0.)
Random.seed!(1)
jobs = repeatedly(50000) do
    observed = map(rand(taskdist(env), env.m)) do (s, g)
        Behavior(s, g, rand(Bernoulli(0.4)))
    end
    tasks = rand(taskdist(env), env.k)
    (tasks, observed)
end

bench_comp(jobs[1:2])
GC.gc()
@time res = bench_comp(jobs);  #  1.490843 seconds (21.61 M allocations: 2.055 GiB, 11.90% gc time)

# %% --------


GC.gc()
@profview bench_comp(jobs);

