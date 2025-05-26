# %% --------

source("base.r")

df <- read_csvs("tmp/main_trials-{version}.csv") %>% 
    select(-data_file)


avg_duration <- df |> 
    group_by(bespoke, compositional, solution_type) %>% 
    summarise(duration = mean(duration/1000))

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

df %>% 
    mutate(
        solution_type = if_else(solution_type == "compositional", 
            glue("comp_{compositional}"), 
            glue("bespoke_{bespoke}")
        )
    ) %>% 
    mutate(solution_type = str_replace(solution_type, "exact", "full")) %>% 
    mutate(solution_type = str_replace(solution_type, "zilch", "none")) %>% 
    group_by(solution_type) %>% 
    summarise(duration = mean(duration/1000)) %>% 
    mutate(duration = duration / min(duration)) %>% 
    write_json("tmp/empirical_costs.json")
    
# %% --------

df %>% 
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

data_means |> 
    ggplot(aes(gen, cost, color=agent)) +
    geom_line(linewidth=1) +
data_means |> 
    ggplot(aes(gen, compositionality, color=agent)) +
    geom_line(linewidth=1) +
plot_layout(guides = "collect")


fig(w=2*WIDTH)
