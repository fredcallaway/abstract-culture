include("utils.jl")
include("infinite_model.jl")
include("binary_env.jl")

using RCall
R_DIR = joinpath(@__DIR__, "r")
R"""
setwd($R_DIR)
source("base.r")
FIGS_PATH <- "figs/model/compare_finite/"
"""


# %% --------

g = grid(
    S = [5, 10],
    D = 1 .* 3 .^ (1:5),
    p_r = 0.5,
    p_0 = 0.01,
    N = Real[100000, Inf]
)

function get_env(prm)
    if prm.N == Inf
        InfiniteModel(;delete(prm, :N)...)
    else
        agent_policy = classic_policy(;prm.p_r, prm.p_0)
        BinaryCompositionEnv(;delete(prm, :p_r, :p_0)..., agent_policy)
    end
end


df = dataframe(g) do prm
    env = get_env(prm)
    sim = simulate(env, 30)
    map(enumerate(sim)) do (gen, pop)
        (;gen=gen-1, compositionality=compositional_rate(pop))
    end
end

@rput df


R"""
figure("evolution_DS", df %>%
    ggplot(aes(gen, compositionality, color=factor(N))) +
    geom_line() + 
    expand_limits(y=c(0, 1)) +
    no_legend +
    # geom_point(data=filter(df, gen == 0), color=RED) +
    facet_grid(S~D) +
    gridlines +
    theme()
)
"""
