include("graph.jl")
include("r.jl")

# %% --------

R"""
plot_compostionality = function(data, grp) {
    if (missing(grp)) {
        data %>%
            ggplot(aes(generation, compositionality)) +
            geom_line(linewidth=.1, mapping=aes(group=population)) +
            mean_line()
    } else {
        data %>%
        ggplot(aes(generation, compositionality, color=factor({{grp}}))) +
        geom_line(linewidth=.1, mapping=aes(group=interaction(population, {{grp}}))) +
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



df = run_sims(30, 10, S=4, N=[10, 20], K=[5], M=[5, 20, 50], ε=[.14])
@rput df

R"""
df %>% plot_compostionality() +
    facet_grid(N~M, labeller=purrr::partial(label_both, sep = " = ")) +
    scale_x_continuous(breaks=scales::pretty_breaks())

fig(w=7, h=5)
"""

# %% --------

df = run_sims(30, 10, S=4, N=[30], K=[2], M=[5, 20, 50], ε=[.14])
@rput df

R"""
df %>% plot_compostionality() +
    facet_grid(N~M, labeller=purrr::partial(label_both, sep = " = ")) +
    scale_x_continuous(breaks=scales::pretty_breaks())

fig(w=7, h=2.5)
"""