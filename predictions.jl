include("red_black.jl")
using DataFrames, CSV
using JSON
# include("r.jl")

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

agent_policy = og_policy = parse_policy("tmp/compositional-rates-code-pilot-$version.csv")
# agent_policy = classic_policy(;p_r=0.5, p_brr=0., p_0=0.1)
baseline = agent_policy.table[1, 1]

agent_policy.table


# %% --------
# D = [2, 3, 4, 8, 16, 24, 32]
N = [32, 50, 100]
D = [2, 8, 32]
n_gen = 15
df = dataframe(grid(;D, N, pop=1:500)) do (;D, N, pop)
    D > N && return missing
    env = RedBlackEnv(;S=4, D, K=1, N, agent_policy, replace_demos=false)
    imap(red_rate.(simulate(env, n_gen))[2:end]) do gen, compositionality
        (;gen, compositionality)
    end
end

df |> CSV.write("tmp/predictions-$version.csv")
# %% --------

R"""

$df |> 
    ggplot(aes(gen, compositionality, group=pop)) +
    geom_line(linewidth=.1, color=RED) +
    geom_hline(yintercept=$baseline, linetype="dashed", linewidth=.3) +
    facet_grid(N~D) + ylim(0, 1) +
    scale_x_continuous(breaks=c(1, 4, 7, 10))

fig(w=7, h=4)
"""

# %% --------

R"""
$df |> 
    group_by(gen, N, D) |> 
    summarise(compositionality=mean(compositionality)) |> 
    ggplot(aes(gen, compositionality)) +
    geom_line(color=RED) +
    geom_hline(yintercept=$baseline, linetype="dashed", linewidth=.3) +
    facet_grid(N~D) + ylim(0, 1) +
    scale_x_continuous(breaks=c(1, 4, 7, 10))

fig(w=7, h=4)
"""

# %% --------
logistic(x) = 1 / (1 + exp(-x))

function rational_policy(β; action_cost=1, search_cost=3, n_bespoke=6, n_part=2)
    map(product(0:1, 0:3)) do (b, c)
        bespoke_cost = n_bespoke * (action_cost + search_cost * (1-b))
        c = min(c, 2)
        compositional_cost = n_part * (3action_cost + search_cost * (2-c))
        logistic(β * (bespoke_cost - compositional_cost))
        # bespoke_cost, compositional_cost
    end |> TabularPolicy
end

rational_policy(0.3).table

using Optim
target = og_policy.table[:, 1:3]
res = optimize(0, 1) do β
    abs.(rational_policy(β).table[:, 1:3] .- target) |> sum
end
res.minimizer

agent_policy = rational_policy(res.minimizer)

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


# %% --------

imap(red_rate.(simulate(env, 10))[2:end]) do gen, compositionality
    (;gen, compositionality)
end