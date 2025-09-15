# %% --------

source("base.r")
RESULTS_PATH <- "../results/cost-SG"
FIGS_PATH <- "figs/cost/idealized-"

plot_evolution <- function(data) {
    data %>% ggplot(aes(gen)) +
        # geom_hline(yintercept=10, color=GREEN, linetype="dashed") +
        geom_line(aes(y = score), color=GREEN, linewidth=1) +
        geom_line(aes(y = compositionality), color = C_COMP, linewidth=1) +
        scale_y_continuous(
            name = "compositionality",
            sec.axis = sec_axis(~. * 1, name = "relative reward")
        ) +
        theme(
            axis.title.y.left = element_text(color = C_COMP),
            axis.title.y.right = element_text(color = GREEN),
            axis.text.y.left = element_text(color = C_COMP),
            axis.text.y.right = element_text(color = GREEN)
        ) +
        expand_limits(y = c(0, 1)) +
        labs(x = "generation")
}

plot_advantage <- function(data, ..., midpoint=0) {
    data %>% ggplot(aes(...)) +
        ggrastr::rasterize(geom_raster(), dev = "ragg", dpi=320) +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=midpoint) +
        no_gridlines
}

plot_advantage_binary <- function(data, ..., fill, midpoint=0) {
    data %>% ggplot(aes(...)) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=midpoint) +
        no_gridlines
}

load_costs <- function(version) {
    read_csv(glue("{RESULTS_PATH}/costs-{version}.csv"), show_col_types=FALSE) %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / base_cost,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / base_cost
    )
}

load_evolution <- function(version) {
    read_csv(glue("{RESULTS_PATH}/evolution-{version}.csv"), show_col_types=FALSE) %>% 
        mutate(score = 1 - relative(cost, lo=0, hi=base_cost))
}


# %% ===== idealized ==========================================================

costs <- load_costs('idealized')
evolution <- load_evolution('idealized')

best_prm <- costs %>% slice_min(asymptotic_advantage)
best_prm %>% pivot_longer(everything())

# %% --------

# this selects the region that should be colored GREEN
costs %>% 
    right_join(select(best_prm, S, D)) %>% 
    filter(comp_advantage > 0)

# %% --------

figure("cost-simple", w=1.5,
    costs %>% 
    filter(search_cost <= 20) %>% 
    # right_join(select(best_prm, S, D)) %>% 
    plot_advantage(search_cost, act_cost, fill=asymptotic_compositionality, midpoint=0) +
    # geom_raster(color=GREEN) +
    # geom_rect(aes(xmin=1, xmax=21, ymin=0.5, ymax=3.5), fill=NA, color=GREEN, linewidth=1) +
    geom_point(data=best_prm) +
    expand_limits(fill=c(0, 1)) +
    # coord_fixed(ratio=2.5) +
    facet_grid(S~D),
)

# %% --------

figure("cost-simple-alt", w=1.5, costs %>% 
    filter(search_cost <= 20) %>% 
    mutate(class = case_when(
        asymptotic_compositionality > 0.9 & comp_advantage > 0 ~ "both",
        asymptotic_compositionality > 0.9 ~ "evolves",
        comp_advantage > 0 ~ "good",
        TRUE ~ "neither"
    )) %>% 
    ggplot(aes(search_cost, act_cost, fill=class)) +
    geom_tile() +
    scale_colour_manual(values=c(
        both = YELLOW,
        good = GREEN,
        evolves = C_COMP,
        neither = GRAY
    ), aesthetics=c("fill", "colour")) +
    no_gridlines +
    facet_grid(S~D) +
    # geom_point(data=best_prm)
    # coord_fixed(ratio=2.5) +
    # facet_wrap(~D, nrow=1),
    theme()
)


# %% --------

figure("cost-evolution", evolution %>% 
    right_join(best_prm) %>% 
    filter(gen > 1) %>% 
    filter(gen < 50) %>% 
    plot_evolution
)

# %% --------

figure_wrap("costs-full", nrow=3,

    plot_advantage(costs, search_cost, act_cost, fill=comp_advantage, midpoint=0) +
        facet_wrap(~D, nrow=1),
    
    plot_advantage(costs, search_cost, act_cost, fill=asymptotic_compositionality, midpoint=0.5) +
        facet_wrap(~D, nrow=1),

    plot_advantage(costs, search_cost, act_cost, fill=asymptotic_advantage, midpoint=0) +
        facet_wrap(~D, nrow=1)
)

# %% ===== SG =================================================================

SG_costs <- load_costs('idealized-SG')  

best_prm <- SG_costs %>% 
    slice_min(asymptotic_advantage)

best_prm %>% pivot_longer(everything()) %>% print(n=100)

figure("tmp", SG_costs %>% 
    # mutate(tasks = factor(S * G), skew=factor(S)) %>% 
    plot_advantage(S, G, fill=asymptotic_advantage, midpoint=0) +
    facet_grid(~D)
)

# %% --------

SG_evolution <- load_evolution('idealized-SG')

figure("tmp", SG_evolution %>% 
    right_join(best_prm) %>% 
    filter(gen > 1) %>% 
    # filter(gen < 50) %>% 
    plot_evolution
)

# %% ===== predicted ==========================================================


costs <- load_costs('predicted')
evolution <- load_evolution('predicted')
FIGS_PATH <- "figs/cost/predicted-"


best_prm <- costs %>% 
    slice_min(asymptotic_advantage)

best_prm %>% pivot_longer(everything()) %>% print(n=100)


# S G D act_cost search_cost

figure("tmp", costs %>% 
    filter(S == 1) %>% 
    filter(asymptotic_advantage < -.1) %>%
    # group_by(S, G, act_cost, search_cost) %>%
    # slice_min(asymptotic_advantage) %>%
    ungroup() %>%
    plot_advantage(act_cost, search_cost, fill=asymptotic_advantage, midpoint=0) +
    facet_grid(G ~ D)
)


# %% --------

figure("evolution", evolution %>% 
    right_join(
        costs %>% 
        filter(G==8) %>% 
        slice_min(asymptotic_advantage)
    ) %>% 
    # right_join(best_prm) %>% 
    filter(gen > 1, gen < 11) %>% 
    plot_evolution
)

# # %% --------

# figure_wrap("costs-full", nrow=3,

#     plot_advantage(costs, search_cost, act_cost, fill=comp_advantage, midpoint=0) +
#         facet_wrap(~D, nrow=1),
    
#     plot_advantage(costs, search_cost, act_cost, fill=asymptotic_compositionality, midpoint=0.5) +
#         facet_wrap(~D, nrow=1),

#     plot_advantage(costs, search_cost, act_cost, fill=asymptotic_advantage, midpoint=0) +
#         facet_wrap(~D, nrow=1)
# )

# # %% --------

# figure("D-act_cost", 
#     costs %>% 
#     filter(search_cost == 2) %>% 
#     plot_advantage(factor(D), act_cost, fill=comp_advantage, midpoint=0)
# )
