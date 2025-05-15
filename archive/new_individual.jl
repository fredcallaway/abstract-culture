@everywhere include("red_black.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim

# %% ==================== setup ====================

function run_sim_finite(;n_gen=30, init=NaN,
                 p_0 = 1e-6,
                 p_brr = 0.,
                 p_r = 1.,
                 S = 10,
                 M = 25,
                 K = 1,
                 N = 10,
                 repeats = 5
                 )

    g = grid(; p_0, p_brr, p_r, S, M, K, N, pop=1:repeats)
    df = dataframe(g; parallel=true) do prm
        env = RedBlackEnv(;delete(prm, :pop)...)
        sim = simulate(env, n_gen)
        map(enumerate(sim)) do (gen, pop)
            (;gen, compositionality = red_rate(pop))
        end
    end
    @rput df
    df
end

R"""
FIGS_PATH = "~/papers/cultural-abstractions/figs/"
MAKE_PDF = TRUE
"""

# %% ==================== individual ====================

df = dataframe(grid(S=[10, 50, 40, 80], p_0=[1e-6, 1e-4, 1e-2], p_r=[1, .5, 0])) do (;S, p_0, p_r)
    res = simulate(RedBlackEnv(;N=1000, K=10000, S, M=0, p_0, p_r), 1)[1]
    map(enumerate(mean(getfield.(res, :red); dims=2)[:])) do (trial, compositionality)
        (;trial, compositionality)
    end
end
@rput df


R"""
df %>%
    mutate(p_r = 100 * p_r) %>%
    ggplot(aes(trial, compositionality, color=factor(S))) +
    geom_line() +
    ylim(0, 1) +
    teals_pal() +
    facet_grid(p_0~p_r, labeller=label_glue(cols="{p_r}% partial", rows="{p_0} neither")) +
    theme()


fig("individual_greedy", w=8, h=6)

"""

# %% --------



res = simulate(RedBlackEnv(N=1000, K=10000, S=100, M=0, p_0=.01, p_r=0), 1)[1]
df = DataFrame(compositionality=mean(getfield.(res, :red); dims=2)[:])
@rput df

R"""
df %>%
    mutate(problem = row_number())  %>%
    ggplot(aes(problem, compositionality)) +
    geom_line() +
    ylim(0, 1)

fig()
"""






# %% ==================== individual with memory ====================



run_sim_finite(;n_gen=30, p_0 = .01, repeats=10, N=30)

R"""
df %>%
    ggplot(aes(gen, compositional)) +
    geom_line(aes(group=pop), linewidth=.5, alpha=.5) +
    mean_line(color=RED)

fig()
"""

# %% --------
M = 80

social = run_sim_finite(;n_gen=30, p_r=0.5, p_0 = .005, repeats=100, N=M, K=1, M=M)
indiv = run_sim_finite(;n_gen=30, p_r=0.5, p_0 = .005, repeats=1000, N=1, K=M, M=M)
@rput social indiv

R"""
d = bind_named(social, indiv)

ggplot(d, aes(gen, compositional, color=name)) +
    geom_line(aes(group=interaction(pop, name)), filter(d, pop < 10), linewidth=.5, alpha=.5) +
    mean_line()

fig(w=6)
"""