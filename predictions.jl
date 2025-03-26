include("red_black.jl")
using DataFrames, CSV
include("r.jl")

# %% --------

function parse_policy(file)
    df = CSV.read(file, DataFrame)
    B = ["unavailable", "available"]
    C = ["none", "partial", "full", "exact"]
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

# %% --------


agent_policy = og_policy = parse_policy("tmp/compositional-rates-code-pilot-v17.csv")
agent_policy.table
# agent_policy = classic_policy(;p_r=0.5, p_brr=0., p_0=0.1)
env = RedBlackEnv(;S=4, D=8, K=1, N=30, agent_policy)

# %% --------
M = Matrix(og_policy.table)
# M[1, 4] = 0.9
# M[1, 1] = .1
# M[2, 3:4] .= .1

agent_policy = TabularPolicy(M)
agent_policy.table

baseline = agent_policy.table[1, 1]
# agent_policy = classic_policy(;p_r=0.5, p_brr=0., p_0=0.05)

df = dataframe(grid(D=[2, 8, 16, 32, 64], N=[32, 64, 128], pop=1:20)) do (;D, N, pop)
    D > N && return missing
    env = RedBlackEnv(;S=4, D, K=1, N, agent_policy, replace_demos=false)
    imap(red_rate.(simulate(env, 10))[2:end]) do gen, compositionality
        (;gen, compositionality)
    end
end


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
logistic(x) = 1 / (1 + exp(-x))

function rational_policy(β; action_cost=1, search_cost=3, n_bespoke=4, n_part=2)
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
