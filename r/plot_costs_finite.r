# %% --------

source("base.r")
RESULTS_PATH <- "../tmp/cost/"
FIGS_PATH <- "figs/cost/"

# %% ===== empirical ==========================================================

df <- read_csv("../tmp/empirical_predictions.csv")

figure("predicted-comp", df %>% 
    ggplot(aes(gen, compositionality, group=pop)) +
    geom_line(aes(group=pop, y = compositionality), color = C_COMP, linewidth=.4, alpha=0.2) +
    # geom_line(aes(group=pop, y = duration), color=GREEN, linewidth=.4, alpha=0.2) +
    theme()
)






# %% ===== theoretical =========================================================

read_indexed_results <- function(path) {
    read_csv(glue("{path}/index.csv"), show_col_types=FALSE) %>% 
    mutate(data = map(filename, ~ 
        read_csv(glue("{path}/{.x}"), show_col_types = FALSE, progress = FALSE))
    ) %>% 
    unnest(data)
}

evolution <- read_indexed_results("../tmp/evolution-finite") %>% 
    filter(gen > 1)

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

comp_coefs <- evolution %>% 
    group_by(S,G,D,act_cost,search_cost) %>% 
    summarise(slope = cov(compositionality, gen) / var(gen))

figure("finite-comp-slope", comp_coefs %>% 
    ggplot(aes(act_cost, search_cost, fill=slope)) +
    geom_raster() +
    labs(fill="comp ~ gen") +
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

# %% --------

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

probs2 <- pop_coefs %>% 
    group_by(S,G,D,act_cost,search_cost) %>% 
    summarise(p_increase=mean(slope > 0.5))


figure("finite-cost-slope-solid", probs2  %>% 
    ggplot(aes(act_cost, search_cost, fill=p_increase)) +
    geom_raster() +
    labs(fill="P(cost increases)") +
    facet_grid(G ~ D) +
    bluered_pal(mid=0.5) +
    theme()
)

# %% --------

data <- evolution %>% 
    filter(
        pop <= 18,
    )

normed <- data %>% 
    filter(gen == 2) %>%
    group_by(S,G,D,act_cost,search_cost) %>% 
    summarise(
        mean=mean(cost),
        sd=sd(cost),
        .groups="keep"
    ) %>% 
    left_join(data) %>% 
    mutate(cost = (cost - mean) / sd)

# %% --------  

axscale <- 4
figure("finite-evolution-single", normed %>% 
    filter(
        G == 7, D == 6,
        search_cost == 0,
        act_cost == 40,
        gen < 11,
    ) %>% 
    mutate(score = -cost) %>% 
    mutate(score = 0.5 + score / axscale) %>% 
    ggplot(aes(gen)) +
    # geom_hline(yintercept=10, color=GREEN, linetype="dashed") +
    geom_line(aes(group=pop, y = compositionality), color = C_COMP, linewidth=.4, alpha=0.2) +
    geom_line(aes(group=pop, y = score), color=GREEN, linewidth=.4, alpha=0.2) +
    mean_line(mapping=aes(y = compositionality), color = C_COMP, linewidth=1) +
    mean_line(mapping=aes(y = score), color=GREEN, linewidth=1) +
    double_y_axis("compositionality", "relative reward", C_COMP, GREEN, scale=axscale, shift=-0.5) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = "generation")
)

# %% --------
axscale <- 4

figure("finite-evolution", normed %>% 
    filter(
        G == 7, D == 6,
        search_cost <= 15,
        between(act_cost, 30, 50),
        gen < 11,
    ) %>% 
    mutate(score = -cost) %>% 
    mutate(score = 0.5 + score / axscale) %>% 
    ggplot(aes(gen)) +
    # geom_hline(yintercept=10, color=GREEN, linetype="dashed") +
    # geom_line(aes(group=pop, y = compositionality), color = C_COMP, linewidth=.2, alpha=0.2) +
    # geom_line(aes(group=pop, y = score), color=GREEN, linewidth=.2, alpha=0.2) +
    mean_line(mapping=aes(y = compositionality), color = C_COMP, linewidth=1) +
    mean_line(mapping=aes(y = score), color=GREEN, linewidth=1) +
    double_y_axis("compositionality", "relative reward", C_COMP, GREEN, scale=axscale, shift=-0.5) +
    # expand_limits(y = c(0, 1)) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = "generation") +
    facet_grid(search_cost~act_cost)
)

# %% --------

figure("finite-points", normed %>% 
    filter(
        G == 7, D == 6,
        search_cost == 0,
        act_cost %in% c(35, 40, 45),
        gen < 11,
        pop <= 6,
    ) %>% 
    ggplot(aes(compositionality, cost, color=gen)) +
    geom_path(aes(group=interaction(pop, act_cost)), linewidth=0.3) +
    geom_point() +
    # facet_grid(G~D) +
    theme()
)
