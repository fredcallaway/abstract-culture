# %% --------

source("base.r")

# %% --------

data <- read_csv("../data/cost_sim.csv")

data_means <- data %>% 
    # group_by(across(-c(cost,compositionality))) %>% 
    group_by(agent, gen) %>% 
    summarise(across(c(cost,compositionality), mean))

data_means

# %% --------

ROBUST_MIN_N = 0

data_means |> 
    ggplot(aes(gen, cost, color=agent)) +
    geom_line(linewidth=1) +
data_means |> 
    ggplot(aes(gen, compositionality, color=agent)) +
    geom_line(linewidth=1) +
plot_layout(guides = "collect")


fig(w=2*WIDTH)
