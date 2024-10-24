include("remote.jl")
connect_repl("hb120")

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
        env = RedBlackEnv(;delete(prm, :pop, :init)..., replace_demos=false)
        sim = simulate(env, n_gen; prm.init)
        map(enumerate(sim)) do (gen, pop)
            (;gen, compositionality = red_rate(pop))
        end
    end
    df
end

@both function run_sim_asymptote(;init=NaN,
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
        env = RedBlackEnv(;delete(prm, :pop, :init)..., replace_demos=false)
        simulate_asymptote(env; prm.init)
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
    S = 4:4:50
    D = [0; 2 .^ (0:12)]
end

# %% --------

stable = @remote get_limits(;S, D, p_0=.05)
@rput stable
R"""
stable %>% 
    ggplot(aes(D, factor(S), fill=stop)) +
    heatmap() +
    scale_x_continuous(trans='log10')

fig()
"""

# %% ==================== plot asymptote ====================

task = @remote run_sim_asymptote(;N=100000, K=[1, 20, 100], S, D = D, p_r=1., init=0., p_0=.01, repeats=10)


# %% --------

# task = @asyncremote run_sim_finite(;N=100000, K=[1, 20, 100], S, D = D, p_r=1., init=0., p_0=.01, repeats=10, n_gen=300)

@async begin
    global vary_k_partial = fetch(task)
    vary_k_partial = fetch(vary_k_partial)

    @rput vary_k_partial

R"""
vary_k_partial %>%
    mutate(D = factor(D)) %>%
    group_by(S,D,K) %>%
    summarise(compositionality = max(compositionality)) %>%
    ggplot(aes(D, S, fill=compositionality)) +
    heatmap() +
    scale_x_discrete(breaks=c(0,1,2,4,8,16,64, 512, 4*1024)) +
    geom_vline(xintercept=1.5, color="white", linetype="solid") +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=85, p1=1, p2=2, mid=0.5, rev=F) +
    # scale_fill_continuous_sequential(h1=350, h2=NA, c1=180, l1=20, l2=80, p1=1, p2=1.5,
    #         name="Asymptotic Compositionality", labels=scales::percent_format(), limits=c(0, 1)) +
    # scale_x_continuous(trans='log10') +
    facet_grid(~K)
fig(w=8)
"""
end

serialize("tmp/oct10-vary_k_partial", vary_k_partial)

# %% ==================== convergence ====================
@rput vary_k_partial

R"""
data = vary_k_partial %>%
    filter(S %in% c(10, 20, 40), D %in% c(8, 32, 128))

data %>%
    ggplot(aes(gen, compositionality, color=factor(K))) +
    geom_line(mapping=aes(group = interaction(S, K, D, pop)), data=filter(data, pop < 11), linewidth=.2, alpha=1) +
    mean_line(linewidth=.7) +
    facet_grid(S~D)
    theme()
    # labs(x="Generation", y="Compositionality Rate", color="Tasks (K)") +
    # discrete_sequential("Purple-Orange", rev=T) + rev_legend +
    # ylim(0, 1)
fig(w=6, h=3)
"""

R"""
df2 %>%
    filter(D == 1024, S==20)
    ggplot(aes(gen, compositionality, color=factor(K))) +
    geom_line(mapping=aes(group = interaction(S, K, D, pop)), linewidth=.2, alpha=1) +
    mean_line(linewidth=.7) +
    theme()

fig()
"""
