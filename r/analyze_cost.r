# %% --------

source("base.r")

main_trials <- read_csvs("tmp/main_trials-{version}.csv") %>% 
    select(-data_file)

avg_duration <- main_trials %>% 
    group_by(bespoke, compositional, solution_type) %>% 
    summarise(duration = mean(duration/1000))

main_trials %>% distinct(version)

# %% --------

# chosen costs
bespoke_full = 1.0
bespoke_none = 4.0
comp_full = 2.0
comp_partial = 3.0
comp_none = 4.0

bespoke_full = 1.0
bespoke_none = 5.0
comp_full = 1.5
comp_partial = 4.5
comp_none = 7.5

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
    filter(duration < quantile(duration, .95)) %>% 
    summarise(duration = mean(duration/1000))

print(empirical_costs)
write_json(empirical_costs, "tmp/empirical_costs.json")

# cost_based_sims.jl 
    
# %% --------
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

both_durations %>% 
    ggplot(aes(prediction, duration, color=solution_type)) +
    geom_point() +
    cpal +
    geom_abline()


fig()

# %% --------

both_durations


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
