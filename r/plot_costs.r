# %% --------

source("base.r")
FIGS_PATH <- "figs/cost/"

VERSION <- "predicted"
# VERSION <- "idealized"

# %% --------

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
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=midpoint) +
        no_gridlines
}

# %% ===== action search ======================================================

load_costs <- function(version) {
    read_csv(glue("../results/cost/costs-{version}.csv")) %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    )
}

load_evolution <- function(version) {
    read_csv(glue("../results/cost/evolution-{version}.csv")) %>% 
        mutate(score = 1 - relative(cost, lo=0, hi=10)) %>% 
        filter(gen > 1)
}


# %% ===== idealized ==========================================================

costs <- load_costs('idealized')
evolution <- load_evolution('idealized')
FIGS_PATH <- "figs/cost/idealized-"

# %% --------

figure("D-act_cost", 
    costs %>% 
    filter(search_cost == 2) %>% 
    plot_advantage(factor(D), act_cost, fill=comp_advantage, midpoint=0)
)

# %% --------

figure("evolution", evolution %>% 
    filter(search_cost == 2, act_cost==9, D==27) %>% 
    plot_evolution
)

# %% --------

figure("tmp", costs %>% 
    mutate(asymptotic_advantage = pmin(0, asymptotic_advantage)) %>% 
    plot_advantage(search_cost, act_cost, fill=asymptotic_advantage, midpoint=0) +
    facet_wrap(~D, nrow=1)
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


# %% ===== predicted ==========================================================

costs <- load_costs('predicted')
evolution <- load_evolution('predicted')
FIGS_PATH <- "figs/cost/predicted-"

# %% --------

figure("tmp", costs %>% 
    mutate(asymptotic_advantage = pmin(0, asymptotic_advantage)) %>% 
    plot_advantage(search_cost, act_cost, fill=asymptotic_advantage, midpoint=0) +
    facet_wrap(~D, nrow=1)
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

figure("evolution", evolution %>% 
    filter(search_cost == 2, act_cost==9, D==32) %>% 
    plot_evolution
)

# %% --------


# %% --------



# %% --------
