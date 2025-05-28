# %% --------

source("base.r")

FIGS_PATH <- "figs/cost_empirical/"


main_trials <- read_csvs("tmp/main_trials-{version}.csv") %>% 
    select(-data_file)

avg_duration <- main_trials %>% 
    group_by(bespoke, compositional, solution_type) %>% 
    summarise(duration = mean(duration/1000))

main_trials %>% distinct(version)

# %% --------

empirical_costs <- main_trials %>% 
    mutate(
        full_solution_type = if_else(solution_type == "compositional", 
            glue("comp_{compositional}"), 
            glue("bespoke_{bespoke}")
        ) %>% 
            str_replace(., "exact", "full") %>% 
            str_replace(., "zilch", "none")
    ) %>% 
    group_by(full_solution_type) %>% 
    # filter(duration < quantile(duration, .95)) %>% 
    summarise(duration = mean(duration/1000))

print(empirical_costs)
write_json(empirical_costs, "tmp/empirical_costs.json")

# %% --------

for some data file, compute actual completion time


# %% --------

# how does solution time depend on the other available option?

figure("duration_context", w=2, main_trials %>% 
    mutate(
        full_solution_type = if_else(solution_type == "compositional", 
            glue("comp_{compositional}"), 
            glue("bespoke_{bespoke}")
        ) %>% 
            str_replace(., "zilch", "none"),
    ) %>% 
    mutate(trial_type = glue("{substr(bespoke, 1, 1)}-{substr(compositional, 1, 1)}")) %>% 
    group_by(trial_type, full_solution_type) %>% 
    filter(duration < quantile(duration, .95)) %>% 
    summarise(duration = mean(duration / 1000)) %>% 
    ggplot(aes(full_solution_type, duration)) +
    geom_text_repel(aes(label = trial_type), size = 2, seed=2) +
    geom_point() +
    coord_flip()
)

# %% --------

main_trials %>% 
    mutate(
        full_solution_type = if_else(solution_type == "compositional", 
            glue("comp_{compositional}"), 
            glue("bespoke_{bespoke}")
        ) %>% 
            str_replace(., "zilch", "none"),
    ) %>% 
    mutate(duration = duration / 1000) %>% 
    regress(duration ~ full_solution_type + (1|uid), mixed=T, add_random=F)


# %% ===== scatter ============================================================




library(ggrepel)

p_comp <- main_trials %>% 
    group_by(bespoke, compositional) %>% 
    summarise(p_compositional = mean(choose_compositional))

rel_cost <- main_trials %>% 
    group_by(bespoke, compositional, solution_type) %>% 
    filter(duration < quantile(duration, .95)) %>% 
    summarise(duration = mean(duration / 1000)) %>% 
    pivot_wider(names_from=solution_type, values_from=duration, names_prefix="duration_") %>% 
    transmute(relative_cost = duration_bespoke - duration_compositional)

figure("cost-compositionality", inner_join(p_comp, rel_cost) %>% 
    ungroup() %>% 
    mutate(trial_type = glue("{substr(bespoke, 1, 1)}-{substr(compositional, 1, 1)}")) %>% 
    ggplot(aes(relative_cost, p_compositional)) +
    geom_point(color=GRAY) +
    geom_text_repel(aes(label = trial_type), direction="y", size = 3, seed=2) +
    ylim(0, 1) +
    geom_vline(xintercept=0)
)
    
# %% ===== OLD ================================================================

actual_durations <- main_trials %>% 
    group_by(bespoke, compositional, solution_type) %>% 
    filter(duration < quantile(duration, .95)) %>% 
    summarise(duration = mean(duration/1000))

# %% --------

both_durations <- actual_durations %>% 
    mutate(
        full_solution_type = if_else(solution_type == "compositional", 
            glue("comp_{compositional}"), 
            glue("bespoke_{bespoke}")
        ) %>% 
            str_replace(., "exact", "full") %>% 
            str_replace(., "zilch", "none")
    ) %>% left_join(rename(empirical_costs, prediction=duration))
    
    
# %% --------
figure("duration-prediction", 
    both_durations %>% 
        ggplot(aes(prediction, duration, color=solution_type)) +
        geom_point() +
        cpal +
        geom_abline()
)

# %% --------

main_trials %>% 
    mutate(
        solution_type = if_else(solution_type == "compositional", 
            glue("comp_{compositional}"), 
            glue("bespoke_{bespoke}")
        )
    ) %>% 
    group_by(solution_type, version) %>% 
    summarise(duration = mean(duration/1000)) %>% 
    group_by(version) %>% 
    mutate(duration = duration / min(duration)) %>% 
    pivot_wider(names_from=version, values_from=duration)

# %% --------

data <- read_csv("../data/cost_sim.csv")

data_means <- data %>% 
    # group_by(across(-c(cost,compositionality))) %>% 
    group_by(agent, gen) %>% 
    summarise(across(c(cost,compositionality), mean))


ROBUST_MIN_N = 0

data_means %>% 
    ggplot(aes(gen, cost, color=agent)) +
    geom_line(linewidth=1) +
data_means %>% 
    ggplot(aes(gen, compositionality, color=agent)) +
    geom_line(linewidth=1) +
plot_layout(guides = "collect")


fig(w=2*WIDTH)
