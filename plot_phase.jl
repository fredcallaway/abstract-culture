using JSON

include("infinite_env.jl")


env = InfiniteEnv(S=10, D=10)

curve = dataframe(grid(;S=10, D=10)) do prm
    env = InfiniteEnv(;prm...)
    map(0:.001:1) do c
        (;c, dc=transition(env, c) - c)
    end
end

CSV.write("results/phase.csv", curve)

stable = fixed_points(env)
@assert length(stable) == 2

write("results/stable.json", JSON.json((;start=stable[1], stop=stable[2])))

cd("r") do
    run(`Rscript plot_phase.R`)
end

