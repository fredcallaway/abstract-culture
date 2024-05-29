include("red_black.jl")

using BenchmarkTools
using ProfileView

# %% --------

env = Environment(;k=10, n=10, m=10, p_0=.001)
# @btime sim = simulate(env, 100; state=initial_population(env, 100));

GC.gc()
@time sim = simulate(env, 1000; state=initial_population(env, 1000));
# 2.035307 seconds (14.00 M allocations: 2.728 GiB, 12.63% gc time)

# %% --------

@profview simulate(env, 1000; state=initial_population(env, 1000));

# %% --------

@memoize get_X(n) = zeros(n, n)

function foo(n, alloc; N=100000)
    if alloc
        for i in 1:N
            zeros(n, n)
        end
    else
        X = get_X(n)
        for i in 1:N
            X = get_X(n)
            fill!(X, 0.)
        end
    end
end

foo(1, true)
foo(1, false)
@time foo(10, true)
@time foo(10, false)

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

