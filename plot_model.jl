using Distributed
nprocs() == 1 && addprocs()

@everywhere include("infinite_model.jl")
include("r.jl")

# %% --------
grd = grid(;S=2:20, D=2 .^ (1:9))

phase = dataframe(grd, parallel=true) do prm
    env = InfiniteModel(;prm...)
    map(0:.001:1) do c
        (;c, dc=transition(env, c) - c)
    end
end

CSV.write("results/phase.csv", phase)

# %% --------

df = dataframe(grd, parallel=true) do prm
    env = InfiniteModel(;prm...)
    stable = fixed_points(env)
    start = stable[1]
    stop = get(stable, 2, missing)
    (;start, stop, asymptote=stable[end])
end

CSV.write("results/fixed_points.csv", df)

# %% --------

df = dataframe(grd, parallel=true) do prm
    env = InfiniteModel(;prm...)
    map(enumerate(simulate(env, 100; init=1e-9))) do (gen, pop)
        (;gen, compositionality=compositional_rate(pop))
    end
end

CSV.write("results/evolution.csv", df)

# %% --------

run_rscript("plot_model.r")
# cp("r/figs/model/model_combined.pdf", "/Users/fred/papers/cultural-abstractions/figures/model_combined.pdf"; force=true)