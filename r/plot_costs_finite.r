# %% --------

source("base.r")
RESULTS_PATH <- "../tmp/cost/"
FIGS_PATH <- "figs/cost/"

read_indexed_results <- function(path) {
    read_csv(glue("{path}/index.csv"), show_col_types=FALSE) %>% 
    mutate(data = map(filename, ~ 
        read_csv(glue("{path}/{.x}"), show_col_types = FALSE, progress = FALSE))
    ) %>% 
    unnest(data)
}

evolution <- read_indexed_results("../tmp/evolution-finite") %>% 
    filter(gen > 1)


FIGS_PATH <- "figs/cost/"

# %% --------

coefs <- evolution %>% 
    group_by(S,G,D,act_cost,search_cost) %>% 
    summarise(slope = cov(cost, gen) / var(gen))

figure("finite-cost-slope", coefs %>% 
    ggplot(aes(act_cost, search_cost, fill=slope)) +
    geom_raster() +
    labs(fill="cost ~ gen") +
    facet_grid(G ~ D) +
    bluered_pal() +
    theme()
)


# %% --------

pop_coefs <- evolution %>% 
    group_by(S,G,D,act_cost,search_cost, pop) %>% 
    summarise(
        slope = cov(cost, gen) / var(gen),
    )

probs <- pop_coefs %>% summarise(p_increase=mean(slope>0)) 

figure("finite-cost-slope-increases", probs  %>% 
    ggplot(aes(act_cost, search_cost, fill=p_increase)) +
    geom_raster() +
    labs(fill="P(cost increases)") +
    facet_grid(G ~ D) +
    bluered_pal(mid=0.5) +
    theme()
)

left_join(coefs, probs) %>% 
    arrange(-slope)

# %% --------

evolution %>% 
    filter(
        abs(search_cost - 0) < 1,
        abs(act_cost - 40) < 1,
        gen < 11,
    ) %>% 
    filter(G==7, D==6) %>% 
    filter(pop < 10) %>% 
    filter(gen == 2) %>% 
    summarise(sd(cost))
    

# %% --------


figure("finite-evolution-single", evolution %>% 
    mutate(score = 1 - relative(cost, lo=0, hi=base_cost)) %>% 
    filter(
        abs(search_cost - 0) < 1,
        abs(act_cost - 40) < 1,
        gen < 11,
    ) %>% 
    filter(G==7, D==6) %>% 
    filter(pop < 10) %>% 
    filter(gen > 1) %>%
    ggplot(aes(gen)) +
    # geom_hline(yintercept=10, color=GREEN, linetype="dashed") +
    geom_line(aes(group=pop, y = compositionality), color = C_COMP, linewidth=.4, alpha=0.2) +
    geom_line(aes(group=pop, y = score), color=GREEN, linewidth=.4, alpha=0.2) +
    mean_line(mapping=aes(y = compositionality), color = C_COMP, linewidth=1) +
    mean_line(mapping=aes(y = score), color=GREEN, linewidth=1) +
    double_y_axis("compositionality", "relative reward", C_COMP, GREEN) +
    expand_limits(y = c(0, 1)) +
    labs(x = "generation")
)

# %% --------

figure("finite-evolution", evolution %>% 
    mutate(score = 1 - relative(cost, lo=0, hi=base_cost)) %>% 
    filter(
        abs(search_cost - 0) <= 20,
        abs(act_cost - 40) <= 15,
        gen < 11,
    ) %>% 
    filter(G==7, D==6) %>% 
    filter(pop < 10) %>% 
    filter(gen > 1) %>%
    ggplot(aes(gen)) +
    # geom_hline(yintercept=10, color=GREEN, linetype="dashed") +
    geom_line(aes(group=pop, y = compositionality), color = C_COMP, linewidth=.4, alpha=0.2) +
    geom_line(aes(group=pop, y = score), color=GREEN, linewidth=.4, alpha=0.2) +
    mean_line(mapping=aes(y = compositionality), color = C_COMP, linewidth=1) +
    mean_line(mapping=aes(y = score), color=GREEN, linewidth=1) +
    double_y_axis("compositionality", "relative reward", C_COMP, GREEN) +
    expand_limits(y = c(0, 1)) +
    labs(x = "generation") +
    facet_grid(search_cost~act_cost)
)
