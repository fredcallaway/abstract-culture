using Distributed
nprocs() == 1 && addprocs()

@everywhere include("infinite_env.jl")
include("r.jl")

# %% --------

df = dataframe(grid(;S=2:20, D=2 .^ (1:8)), parallel=true) do prm
    env = InfiniteEnv(;prm...)
    stable = fixed_points(env)
    start = stable[1]
    stop = get(stable, 2, missing)
    (;start, stop, asymptote=stable[end])
end

CSV.write("results/asymptote.csv", df)
run_rscript("plot_asymptote.r")
