include("binary_env.jl")
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

# %% --------

N = [32, 50, 100]
D = [2, 8, 32]
n_gen = 15

function make_predictions(agent_policy)
    # D = [2, 3, 4, 8, 16, 24, 32]
    df = dataframe(grid(;D, N, pop=1:500)) do (;D, N, pop)
        D > N && return missing
        env = BinaryCompositionEnv(;S=4, D, K=1, N, agent_policy, replace_demos=false)
        imap(compositional_rate.(simulate(env, n_gen))[2:end]) do gen, compositionality
            (;gen, compositionality)
        end
    end
end

make_predictions(human_policy) |> CSV.write("results/predictions-empirical-$version.csv")

# %% --------

function epsilon_policy(ε=0.5; partial_none=1)
    X = [
        0 partial_none 1 1;
        0 0 0 0
    ]
    TabularPolicy(@. X * (1 - ε) + (1 - X) * ε)
end

function fit_epsilon_policy(;kws...)
    res = optimize(0, 1) do ε
        abs.(epsilon_policy(ε; kws...).table .- human_policy.table) |> sum
    end
    ε = res.minimizer
    println("found ε = $ε with loss $(res.minimum)")
    epsilon_policy(ε; kws...)
end

# %% --------

make_predictions(fit_epsilon_policy()) |> CSV.write("results/predictions-epsilon-$version.csv")
make_predictions(fit_epsilon_policy(partial_none=0.5)) |> CSV.write("results/predictions-epsilon-partial0.5-$version.csv")
make_predictions(fit_epsilon_policy(partial_none=0)) |> CSV.write("results/predictions-epsilon-partial0-$version.csv")

