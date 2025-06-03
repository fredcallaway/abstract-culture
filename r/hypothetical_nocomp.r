# %% --------

source("base.r")

FIGS_PATH <- "figs/hypothetical_nocomp/"

WINSOR_THRESH <- 3

main_trials <- read_csvs("tmp/main_trials-reg-v2.csv") %>% 
    select(-data_file)

data <- main_trials %>% 
    filter(generation > 1) %>% 
    mutate(trial_type = glue("{bespoke}-{compositional}")) %>% 
    group_by(trial_type) %>%
    mutate(duration = zclip(duration, WINSOR_THRESH)) %>% 
    ungroup() %>% 
    mutate(duration = duration / 1000) %>% 
    select(D, trial_type, duration)
    
p_observe_bespoke <- 0.8732112136229997  # prob_observe(1 / (env.S^2), env.D)

duration_diff <- function(data) {
    actual <- data %>% 
        filter(D == 32) %>% 
        with(mean(duration))
    
    hypo <- data %>% 
        group_by(trial_type) %>% 
        summarise(duration = mean(duration)) %>% 
        pivot_wider(names_from=trial_type, values_from=duration) %>% 
        with(
            p_observe_bespoke * `exact-zilch` +
            (1 - p_observe_bespoke) * `zilch-zilch`
        )
    actual - hypo
}

duration_diff(data)

# %% --------

X <- main_trials %>% 
    mutate(trial_type = glue("{bespoke}-{compositional}")) %>% 
    group_by(trial_type) %>%
    mutate(outlier = duration > mean(duration) + WINSOR_THRESH*sd(duration))

figure("rt-hists", w=2, h=1.3, X %>% 
    ggplot(aes(trial_type, y=duration/1000)) +
    # geom_boxplot() +
    geom_violin(data=filter(X, !outlier)) +
    geom_point(data=filter(X, outlier), size=.3, color=RED) +
    # stat_mean_and_quantiles(color=RED, rng=.95) +
    coord_flip()
)

X %>% agg(outlier, trial_type)


# %% --------

# Bootstrap analysis
set.seed(123)  # for reproducibility
n_bootstrap <- 1000

bootstrap_results <- modelr::bootstrap(data, n_bootstrap)$strap %>% 
    map_dbl(~ duration_diff(as_tibble(.x)))


# Summary statistics
bootstrap_summary <- tibble(
    mean = mean(bootstrap_results),
    sd = sd(bootstrap_results),
    q025 = quantile(bootstrap_results, 0.025),
    q975 = quantile(bootstrap_results, 0.975),
    median = median(bootstrap_results)
)

bootstrap_summary %>% 
    with(write_tex("{mean:.2}, 95% CI [{q025:.2}, {q975:.2}]", "hypothetical_nocomp"))


