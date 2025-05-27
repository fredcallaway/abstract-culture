# %% --------
source("base.r")

FIGS_PATH <- "figs/cost_sims/"

search <- read_csvs("../results/costs-flexible-{agent}.csv") %>% 
    mutate(
        rel_cost = free_cost / bespoke_cost,
    ) %>% 
    select(-data_file) %>% 
    group_by(agent)

controlled <- c(
    "comp_full",
    "comp_partial",
    "comp_none"
)
# %% --------

search %>% 
    filter(comp_none > bespoke_none) %>% 
    slice_max(rel_cost) %>% 
    select(all_of(controlled), rel_cost)

# %% --------

figure("comp_cost", search %>% 
    ggplot(aes(rel_cost, free_comp, color=comp_partial)) +
    geom_point() + 
    facet_wrap(~agent) +
    geom_vline(xintercept=1)
)

# %% --------

marginal_max <- map_dfr(controlled, function(var) {
    search %>%
        group_by(!!sym(var), .add=TRUE) %>%
        summarise(
            max_rel_cost = max(rel_cost, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(variable = var) %>%
        rename(value = !!sym(var))
})

figure("marginal_max", marginal_max %>% 
    ggplot(aes(value, max_rel_cost, color=agent)) +
    geom_line() +
    facet_wrap(~variable, scales="free_x")
)

# CONCLUDE: comp none doesn't matter much

# %% --------

figure("cost_full_partial", w=2.5,h=1,
    search %>% 
        group_by(agent, comp_full, comp_partial) %>% 
        slice_max(rel_cost) %>% 
        group_by(agent) %>% 
        group_map(function(data, grp) {
            ggplot(data, aes(comp_full, comp_partial, color=rel_cost)) +
            geom_point() +
            scale_color_continuous_diverging(mid=1) +
            ggtitle(glue("{grp$agent}"))
        }) %>% 
        reduce(`+`)
)

# %% --------

sim <- read_csvs("../results/sim-flexible-{mode}.csv") %>% 
    mutate(agent = glue("{agent}-{mode}"))

data_means <- sim %>% 
    # group_by(across(-c(cost,compositionality))) %>% 
    group_by(agent, gen) %>% 
    summarise(across(c(cost,compositionality), mean))

sim %>% distinct(agent)

figure("sim_best", w=2.5,
    data_means %>% 
        ggplot(aes(gen, cost, color=agent)) +
        geom_line(linewidth=1) +
    data_means %>% 
        ggplot(aes(gen, compositionality, color=agent)) +
        geom_line(linewidth=1)  +
    plot_layout(guides = "collect") &
    scale_color_manual(
        values = c(
            "rational-pure" = "red",
            "rational-noisy" = "pink",
            "bespoke-pure" = "blue",
            "bespoke-noisy" = "lightblue"
        )
    )
       
)

# %% ===== empirical ==========================================================

empirical_search <- read_csv("../results/costs-empirical.csv") %>% 
    mutate(rel_cost = free_cost / bespoke_cost)

figure("empirical_SDN", w=2.5, h=.9,
    empirical_search %>% 
        ggplot(aes(N, D, fill=rel_cost)) +
        geom_tile() +
        scale_fill_continuous() +
        geom_point(x=32, y=32, color="red", size=3, shape="O") +
    empirical_search %>% 
        ggplot(aes(N, D, fill=free_comp)) +
        geom_tile() + 
        scale_fill_continuous() +
        geom_point(x=32, y=32, color="red", size=3, shape="O") +
        labs(fill="predicted p(comp)")
)

# %% --------

figure("empirical_cost_comp", 
    empirical_search %>% 
        ggplot(aes(rel_cost, free_comp, color=D)) +
        geom_point() +
        scale_color_viridis_c() +
        geom_point(data=filter(empirical_search, N==32, D==32), color="red", size=3, shape="O") +
        labs(y="predicted p(comp)")
)

# %% --------

sim_empirical <- read_csv("../results/sim-empirical.csv")

data_means <- sim_empirical %>% 
    # group_by(across(-c(cost,compositionality))) %>% 
    group_by(agent, gen) %>% 
    summarise(across(c(cost,compositionality), mean))


figure("sim_empirical", w=2.5,
    data_means %>% 
        ggplot(aes(gen, cost, color=agent)) +
        geom_line(linewidth=1) +
        expand_limits(y=0) +
    data_means %>% 
        ggplot(aes(gen, compositionality, color=agent)) +
        geom_line(linewidth=1) +
        expand_limits(y=c(0, 1)) +
    plot_layout(guides = "collect")       
)
