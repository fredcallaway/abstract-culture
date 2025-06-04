using RCall
include("utils.jl")
include("infinite_env.jl")

R_DIR = joinpath(@__DIR__, "r")

R"""
setwd($R_DIR)
source("base.r")
FIGS_PATH <- "figs/model/"
"""

# %% --------

g = grid(
    S = [5, 10],
    D = 1 .* 3 .^ (1:5),
    p_r = 1,
    p_0 = 0.01
)

df = dataframe(g) do prm
    env = InfiniteEnv(;prm...)
    sim = simulate(env, 30)
    map(enumerate(sim)) do (gen, pop)
        (;gen=gen-1, compositionality=compositionality_rate(pop))
    end
end

@rput df

# %% --------

R"""
figure("evolution_DS", df %>%
    ggplot(aes(gen, compositionality)) +
    geom_line(color=RED) + expand_limits(y=c(0, 1)) +
    no_legend +
    # geom_point(data=filter(df, gen == 0), color=RED) +
    facet_grid(S~D) +
    gridlines +
    theme()
)
"""
