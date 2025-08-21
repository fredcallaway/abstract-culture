# %% --------

source("base.r")

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
        rasterize(geom_raster()) +
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
    read_csv(glue("../results/cost/costs-{version}.csv")) %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    )
}

load_evolution <- function(version) {
    read_csv(glue("../results/cost/evolution-{version}.csv")) %>% 
        mutate(score = 1 - relative(cost, lo=0, hi=10))
}


# %% ===== idealized ==========================================================

costs <- load_costs('idealized') %>% 
    filter(bespoke_zilch < comp_zilch) %>% 
    # filter(comp_partial < bespoke_zilch)
    identity
evolution <- load_evolution('idealized')
FIGS_PATH <- "figs/cost/idealized-"

last_gen <- evolution %>% filter(gen == 100) %>% select(1:4, comp100 = compositionality)
best_prm <- costs %>% 
    left_join(last_gen) %>% filter(comp100 > .01) %>% 
    # filter(asymptotic_compositionality > 0.8) %>% 
    slice_min(asymptotic_advantage) %>% 
    head(1)

best_prm %>% pivot_longer(everything())


# %% --------

FIGS_PATH <- "/Users/fred/papers/cultural-abstractions/figures/src/"

plot_advantage <- function(data, ..., midpoint=0) {
    data %>% ggplot(aes(...)) +
        ggrastr::rasterize(geom_raster(), dev = "ragg", dpi=320) +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=midpoint) +
        no_gridlines
}

figure("cost-simple", w=1.5,
    costs %>% 
    filter(search_cost <= 20) %>% 
    right_join(select(best_prm, S, D)) %>% 
    plot_advantage(search_cost, act_cost, fill=asymptotic_compositionality, midpoint=0) +
    geom_rect(aes(xmin=1, xmax=21, ymin=0.5, ymax=3.5), fill=NA, color=GREEN, linewidth=1) +
    geom_point(data=best_prm) +
    expand_limits(fill=c(0, 1))
    # coord_fixed(ratio=2.5) +
    # scale_fill_gradient(low=RED, high=GREEN,) +
    # facet_wrap(~D, nrow=1),
)

# %% --------

figure("tmp", 
    costs %>% 
    filter(D == 27) %>% 
    # right_join(select(best_prm, S, D)) %>% 
    plot_advantage(search_cost, act_cost, fill=comp_advantage, midpoint=0) +
    labs(fill="cost savings")
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
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", lim=c(-.5, .5)) +
        facet_wrap(~D, nrow=1),
    
    plot_advantage(costs, search_cost, act_cost, fill=asymptotic_compositionality, midpoint=0.5) +
        facet_wrap(~D, nrow=1),

    plot_advantage(costs, search_cost, act_cost, fill=asymptotic_advantage, midpoint=0) +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", lim=c(-.5, .5)) +
        facet_wrap(~D, nrow=1)
)

# %% ===== SD =================================================================

SD_costs <- load_costs("idealized-SD")

figure("tmp", SD_costs %>% 
    fctrize(S) %>% 
    fctrize(D) %>% 
    ungroup() %>% 
    # right_join(select(best_prm, S, D)) %>% 
    plot_advantage(D, S, fill=asymptotic_advantage, midpoint=0) +
    labs(fill="cost savings")
)



# %% ===== predicted ==========================================================

stop("don't run this")

costs <- load_costs('predicted') %>% filter(search_cost >= 10)
evolution <- load_evolution('predicted') %>% filter(search_cost >= 10)
FIGS_PATH <- "figs/cost/predicted-"

# %% --------

last_gen <- evolution %>% filter(gen == 100) %>% select(1:6, comp100 = compositionality) %>% print

best_prm <- costs %>% 
    left_join(last_gen) %>% 
    filter(comp100 > .3) %>% 
    slice_min(asymptotic_advantage) %>% 
    # select(D, search_cost, act_cost) %>% 
    print

best_prm %>% select(comp_advantage, asymptotic_advantage, asymptotic_compositionality, comp100)

# %% --------

figure("tmp", costs %>% 
    mutate(asymptotic_advantage = pmin(0, asymptotic_advantage)) %>% 
    plot_advantage(search_cost, act_cost, fill=asymptotic_advantage, midpoint=0) +
    facet_wrap(~D, nrow=1)
)

# %% --------

# best_prm <- costs %>% 
#     filter(search_cost > 0) %>% 
#     slice_min(asymptotic_advantage) %>% 
#     select(D, search_cost, act_cost)

figure("evolution", evolution %>% 
    right_join(best_prm) %>% 
    filter(gen < 11) %>% 
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

# %% --------

figure("D-act_cost", 
    costs %>% 
    filter(search_cost == 2) %>% 
    plot_advantage(factor(D), act_cost, fill=comp_advantage, midpoint=0)
)

# %% --------


# %% --------

