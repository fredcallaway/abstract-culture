using Distributed
nprocs() == 1 && addprocs()

@everywhere include("infinite_env.jl")

# %% --------
grd = grid(;S=1:14, G=1:14, D=2 .^ (0:6))

fixed = dataframe(grd, parallel=true) do prm
    env = InfiniteEnv(;prm...)
    stable = fixed_points(env)
    start = stable[1]
    stop = get(stable, 2, missing)
    (;start, stop, asymptote=stable[end])
end

CSV.write("results/SG_fixed_points.csv", fixed)

# %% --------


fixed_points(InfiniteEnv(;S=10, G=10, D=5))
fixed_points(InfiniteEnv(;S=5, G=20, D=5))

# %% --------

evol = dataframe(grd, parallel=true) do prm
    env = InfiniteEnv(;prm...)
    map(enumerate(simulate(env, 100; init=1e-9))) do (gen, pop)
        (;gen, compositionality=compositional_rate(pop))
    end
end

CSV.write("results/SG_evolution.csv", evol)

# %% --------
# include("r.jl")
# run_rscript("plot_model.r")
# cp("r/figs/model/model_combined.pdf", "/Users/fred/papers/cultural-abstractions/figures/model_combined.pdf"; force=true)