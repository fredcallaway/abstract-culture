include("remote.jl")
# connect_repl("hb120")

# %% --------

# RemoteREPL._repl_client_connection = nothing
# @remote gethostname()
# @remote nprocs()
@both @everywhere include("red_black.jl")

@both function run_sim_finite(;n_gen=30, init=NaN,
                 p_0 = 0.,
                 p_brr = 0.,
                 p_r = 0.,
                 S = 10,
                 D = 25,
                 K = 1,
                 N = 10,
                 myopic = false,
                 repeats = 5,
                 )

    g = grid(; p_0, p_brr, p_r, S, D, K, N, myopic, init, pop=1:repeats)
    df = dataframe(g; parallel=true) do prm
        env = RedBlackEnv(;delete(prm, :pop, :init)...)
        sim = simulate(env, n_gen; prm.init)
        map(enumerate(sim)) do (gen, pop)
            (;gen, compositionality = red_rate(pop))
        end
    end
    df
end

R"""
# FIGS_PATH = "~/papers/cultural-abstractions/figs/"
# MAKE_PDF = TRUE
FIGS_PATH = "figs/"
MAKE_PDF = FALSE

plot_evolution = function(df, ...) {
    grp_data = df %>%
        filter(pop < 11) %>%
        mutate(population = interaction(!!!syms(head(names(df), -2))))
    df %>%
        ggplot(aes(gen, compositionality, ...)) +
        geom_line(mapping=aes(group = population), data=grp_data, linewidth=.2, alpha=1) +
        agg_line(mean, linewidth=.7) +
        ylim(0, 1)
}

"""

# %% ==================== select range ====================

@both function get_limits(; p_0 = 0., p_brr = 0., p_r = 0., S = 10, D = 25)
    g = grid(; p_0, p_brr, p_r, S, D)
    dataframe(prm->find_stable_points(;prm...), g; parallel=true)
end

@both begin
    S = 5:5:50
    D = [0; 5 * 2 .^ (0:8)]
end

D
# %% --------

stable = @remote get_limits(;S, D, p_0=.05)
@rput stable
R"""
stable %>% 
    ggplot(aes(D, factor(S), fill=stop)) +
    heatmap +
    scale_x_continuous(trans='log10')

fig()
"""

# %% --------

vary_k_big = @asyncremote run_sim_finite(;N=1000, K=[1, 10, 50], S, D, p_0=.05, init=.05,
    repeats=30, n_gen=50)

vary_k_big = fetch(vary_k_big)
@rput vary_k_big

vary_k01 = @remote run_sim_finite(;N=1000, K=[1, 10, 50], S, D, p_0=.01, init=.01, repeats=30, n_gen=100)
@rput vary_k01


R"""
# vary_k_big %>%
vary_k01 %>%
    # mutate(D = if_else(D == 0, D)) %>%
    mutate(D = factor(D)) %>%
    filter(gen == max(gen)) %>%
    group_by(S,D,K) %>%
    summarise(compositionality = max(compositionality)) %>%
    ggplot(aes(D, S, fill=compositionality)) +
    heatmap() +
    # scale_x_discrete(breaks=$(D[1:3:end])) +
    scale_x_discrete(breaks=c(0, 5, 20, 80, 320, 1280)) +
    geom_vline(xintercept=1.5, color="white", linetype="solid") +
    # scale_x_continuous(trans='log10') +
    facet_grid(~K)
fig(w=8)

"""

# %% --------

vary_k_partial = run_sim_finite(;N=100, K=[1, 10, 50], S, D = D ./ 5, p_r=1., p_0=.001, init=.01, repeats=3, n_gen=20)
@rput vary_k_partial
R"""
vary_k_partial %>%
    # mutate(D = if_else(D == 0, D)) %>%
    mutate(D = factor(D)) %>%
    filter(gen == max(gen)) %>%
    group_by(S,D,K) %>%
    summarise(compositionality = max(compositionality)) %>%
    ggplot(aes(D, S, fill=compositionality)) +
    heatmap() +
    # scale_x_discrete(breaks=$(D[1:3:end])) +
    scale_x_discrete(breaks=c(0, 5, 20, 80, 320, 1280)/5) +
    geom_vline(xintercept=1.5, color="white", linetype="solid") +
    # scale_x_continuous(trans='log10') +
    facet_grid(~K)
fig(w=8)
"""

# %% --------

R"""
vary_k_big %>% plot_evolution(color=factor(K)) + facet_grid(S~D)
fig(w=20, h=20)
"""





# %% ==================== convergence ====================

# does it converge from above?

from9 = @asyncremote run_sim_finite(N=500, K=10, S=10, D=[7, 8, 9, 10], p_0=0., p_r=0., init=1., myopic=[true], repeats=50, n_gen=50)
from9 = fetch(from9)
@rput from9


R"""
from9 %>%
    ggplot(aes(gen, compositionality, color=factor(D))) +
    geom_line(mapping=aes(group = interaction(D, pop)), data=filter(from9, pop < 11), linewidth=.2, alpha=1) +
    mean_line(linewidth=.7) +
    facet_grid(~K)
    theme()
    # labs(x="Generation", y="Compositionality Rate", color="Tasks (K)") +
    # discrete_sequential("Purple-Orange", rev=T) + rev_legend +
    # ylim(0, 1)
fig(w=8)
"""
# %% --------

df = @remote run_sim_finite(N=[100, 500], K=10, S=10, D=[7, 8, 9, 10], p_0=0., p_r=0., init=1., myopic=[true], repeats=50, n_gen=100)
@rput df
R"""
# df = bind_rows(from1, from9)
# df = from1


df %>% plot_evolution(color=factor(D)) + facet_grid(~N)
fig(w=8, h=3)
"""

# %% --------


R"""
vary_k %>%
    filter(gen == max(gen)) %>%
    ggplot(aes(D, S, fill=compositionality)) +
    heatmap
    facet_grid(~K)
    theme()
    # labs(x="Generation", y="Compositionality Rate", color="Tasks (K)") +
    # discrete_sequential("Purple-Orange", rev=T) + rev_legend +
    # ylim(0, 1)
fig("vary_k", w=8)
"""