include("graph.jl")
include("r.jl")

# %% --------

R"""
plot_compostionality = function(data, grp) {
    if (missing(grp)) {
        data %>%
            ggplot(aes(generation, compositionality)) +
            geom_line(data=filter(data, population < 21), linewidth=.1, mapping=aes(group=population)) +
            mean_line()
    } else {
        data %>%
        ggplot(aes(generation, compositionality, color=factor({{grp}}))) +
        geom_line(data=filter(data, population < 21), linewidth=.1, mapping=aes(group=interaction(population, {{grp}}))) +
        mean_line()
    }
}

"""

# %% --------

df = run_sims(300, 10, S=4, N=12, K=7, M=[5,20,50], ε=.2)
@rput df

R"""
df %>% plot_compostionality() +
    stat_summary(fun.data="mean_sdl", geom="ribbon", alpha=0.1, mult=2) +
    scale_x_continuous(breaks=scales::pretty_breaks()) +
    facet_wrap(~M)

fig(w=7)
"""


# %% --------

df = run_sims(10, 10, S=4, N=[10], K=[5], M=[10, 20, 30, 50], ε=[.14])
@rput df

R"""
df %>% plot_compostionality() +
    facet_wrap(~M, labeller=purrr::partial(label_both, sep = " = "), nrow=1) +
    teals_pal()

fig(w=7)
"""

# %% --------



df = run_sims(500, 10, S=4, N=[10, 12, 14, 16, 18, 20], K=[5], M=[5, 20, 50], ε=[.14])
@rput df

R"""
df %>% plot_compostionality() +
    facet_grid(M~N, labeller=purrr::partial(label_both, sep = " = ")) +
    scale_x_continuous(breaks=scales::pretty_breaks())

fig(w=10, h=7)
"""

# %% --------

df = run_sims(500, 10, S=4, N=[12], K=[5,7,10], M=[5, 20, 50], ε=[.14])
@rput df

R"""
df %>% plot_compostionality() +
    stat_summary(fun.data="mean_sdl", geom="ribbon", alpha=0.1, mult=2) +
    facet_grid(K~M, labeller=purrr::partial(label_both, sep = " = ")) +
    scale_x_continuous(breaks=scales::pretty_breaks())

fig(w=7, h=5)
"""

# %% ==================== power analysis ====================

df = run_sims(500, 10, S=4, N=[12,15], K=[5,7,10], M=[5, 20, 50], ε=[.14])
@rput df


R"""
sample_p = function(groups, n_chain, run_model) {
    data = sample(groups, n_chain, replace=T) %>% bind_rows
    tryCatch(run_model(data), error=function(c) NaN)
}

power_analysis = function(data, n_chain, n_sim, run_model) {
    groups = data %>%
        nest_by(population, .keep=TRUE) %>%
        with(data)
    # results = map(n_chain, ~ replicate(n_sim, sample_p(groups, .x, run_model))) %>% unlist
    expand.grid(
        sim_i = 1:n_sim,
        n_chain = n_chain
    ) %>%
    rowwise() %>%
    mutate(
        p = sample_p(groups, n_chain, run_model)
    ) %>% ungroup()
}


run_model = . %>%
    regress(compositionality ~ M, print_table=FALSE) %>%
    tidy %>%
    filter(term != "(Intercept)") %>%
    with(max(p.value))

pwr = df %>%
    filter(generation > 1, generation < 8) %>%
    select(-c(S, ε, generation)) %>%
    group_by(population, N, M, K) %>%
    summarise(compositionality=mean(compositionality)) %>%
    ungroup() %>% 
    fctrize(M, levels=c(20, 5, 50)) %>%
    nest_by(N, K) %>%
    reframe(power_analysis(data, c(1,2,3), 300, run_model)) %>%
    group_by(N, K, n_chain) %>%
    summarise(power = mean(p < .05))

"""

R"""
pwr %>%
    ggplot(aes(n_chain, power, color=factor(K))) +
    geom_line(linewidth=.5) + teals_pal() +
    facet_wrap(~N)

fig()
"""