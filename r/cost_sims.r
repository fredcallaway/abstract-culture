# %% --------
source("base.r")

FIGS_PATH <- "figs/cost_sims/"

search <- read_csvs("../results/costs-flexible-{agent}.csv") %>% 
    mutate(rel_cost = free_cost / bespoke_cost) %>% 
    select(-data_file)

# %% --------

search %>% 
    group_by(agent) %>% 
    filter(comp_none > bespoke_none) %>% 
    slice_max(rel_cost)

# %% --------

figure("tmp", search %>% 
    ggplot(aes(rel_cost, free_comp, color=comp_partial)) +
    geom_point() + 
    facet_wrap(~agent) +
    geom_vline(xintercept=1)
)

# %% --------

controlled <- c(
    "comp_full",
    "comp_partial",
    "comp_none"
)

marginal_max <- map_dfr(controlled, function(var) {
    search %>%
        group_by(agent, !!sym(var)) %>%
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

figure("cost_full_partial", w=2.5,
    search %>% 
        group_by(agent, comp_full, comp_partial) %>% 
        slice_max(rel_cost) %>% 
        group_by(agent) %>% 
        group_map(function(data, grp) {
            ggplot(data, aes(comp_full, comp_partial, color=rel_cost)) +
            geom_point() +
            scale_color_continuous_diverging(mid=1) +
            ggtitle(grp$agent)
        }) %>% 
        reduce(`+`)
)
