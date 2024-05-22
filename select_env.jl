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
df %>%
    fctrize(M, levels=c(20, 5, 50)) %>%
    regress(compositionality ~ M, print_table=FALSE) %>%
    tidy %>%
    filter(term != "(Intercept)") %>%
    with(max(p.value))

"""


R"""


sample_p = function(groups, n_chain, run_model) {
    data = sample(groups, n_chain, replace=T) %>% bind_rows
    tryCatch(run_model(data), error=function(c) NaN)
}

power_analysis = function(N, n_chain, n_sim, run_model) {
    groups = df %>%
        filter(N == {{N}}) %>%
        fctrize(M, levels=c(20, 5, 50)) %>%
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

n_sim = 300
n_chain = c(1, 2, 4, 8)

pwr = map(c(10, 12,14, 16, 18, 20), ~
    power_analysis(.x, n_chain, n_sim, . %>%
        regress(compositionality ~ M, print_table=FALSE) %>%
        tidy %>%
        filter(term != "(Intercept)") %>%
        with(max(p.value))
    ) %>%
    mutate(N = .x) %>%
    group_by(N, n_chain) %>%
    summarise(power = mean(p < .05))
) %>% bind_rows

"""

R"""
pwr %>%
    ggplot(aes(n_chain, power, color=factor(N))) +
    geom_line(linewidth=.5) + teals_pal()

fig()
"""