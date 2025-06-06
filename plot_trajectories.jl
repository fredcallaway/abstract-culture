include("infinite_env.jl")
include("utils.jl")

# %% --------


g = grid(
    S = [5, 10],
    D = 1 .* 3 .^ (1:4),
    p_r = 0.5,
    p_0 = 1e-5
)


# %% --------

dataframe(g) do prm
    env = InfiniteEnv(;prm...)
    map(enumerate(simulate(env, 20))) do (gen, pop)
        
        (;gen=gen-1, named_tuple(pop)...)
        # (;gen=gen-1, named_tuple.(sim)...)
    end
end |> CSV.write("results/trajectories.csv")

using RCall
cd("r") do
    R"""source("trajectories.r")"""
end;

