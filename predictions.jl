include("red_black.jl")
using DataFrames, CSV
using JSON
using Optim

# %% --------

version = "v23"

function parse_policy(file)
    df = CSV.read(file, DataFrame)
    B = ["zilch", "exact"]
    C = ["zilch", "partial", "full", "exact"]
    if "exact" ∉ df.compositional
        C[end] = "full"
    end
    map(product(B, C)) do (b, c)
        row = filter(df) do row
            row.bespoke == b && row.compositional == c
        end |> only
        row.p_compositional
    end |> TabularPolicy
end

human_policy = parse_policy("tmp/compositional-rates-code-pilot-$version.csv")

N = [32, 50, 100]
D = [2, 8, 32]
n_gen = 15

function make_predictions(agent_policy)
    # D = [2, 3, 4, 8, 16, 24, 32]
    df = dataframe(grid(;D, N, pop=1:500)) do (;D, N, pop)
        D > N && return missing
        env = RedBlackEnv(;S=4, D, K=1, N, agent_policy, replace_demos=false)
        imap(red_rate.(simulate(env, n_gen))[2:end]) do gen, compositionality
            (;gen, compositionality)
        end
    end
end

make_predictions(human_policy) |> CSV.write("tmp/predictions-$version.csv")

# %% --------

function epsilon_policy(ε=0.5)
    X = [
        0 1 1 1;
        0 0 0 0
    ]
    TabularPolicy(@. X * (1 - ε) + (1 - X) * ε)
end

using Optim
res = optimize(0, 1) do ε
    abs.(epsilon_policy(ε).table .- human_policy.table) |> sum
end
res.minimizer

make_predictions(epsilon_policy(res.minimizer)) |> CSV.write("tmp/predictions-epsilon-$version.csv")

# %% --------

flatmap([2, 8, 32]) do D
    agent_policy = TabularPolicy(ones(2,4)/2)
    env = RedBlackEnv(;S=4, D, K=1, N=50, agent_policy, replace_demos=false)
    pop = simulate(env, 5)[end]
    println(red_rate(pop))
    map(pop[:]) do x
        (;D, task = string(x.a, x.b), kind = x.red ? "bespoke" : "compositional")
    end
end |> json |> write("../machine-task/stimuli/simulated_solutions.json")
