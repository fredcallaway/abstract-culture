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


R"""
groups = fixations %>%
    filter(presentation != n_pres) %>%
    mutate(
        fixated = case_when(
            mod(presentation, 2) == 1 ~ pretest_accuracy_first,
            mod(presentation, 2) == 0 ~ pretest_accuracy_second,
        ),
        nonfixated = case_when(
            mod(presentation, 2) == 1 ~ pretest_accuracy_second,
            mod(presentation, 2) == 0 ~ pretest_accuracy_first,
        )
    ) %>%
    mutate(relative = fixated - nonfixated) %>%
    select(wid, duration, fixated) %>%
    nest_by(wid, .keep=TRUE) %>%
    with(data)

sample_p = function(N, run_model) {
    data = sample(groups, N, replace=T) %>%
        map(~ mutate(.x, wid=round(1e10 * runif(1)))) %>%
        bind_rows
    tryCatch(run_model(data), error=function(c) NaN)
}

power_analysis = function(N, n_sim, run_model) {
    results = map(N, ~ replicate(n_sim, sample_p(.x, run_model))) %>% unlist
    expand.grid(
        sim_i = 1:n_sim,
        N = N
    ) %>%
    rowwise() %>%
    mutate(
        p = sample_p(N, run_model)
    ) %>% ungroup()
}
"""