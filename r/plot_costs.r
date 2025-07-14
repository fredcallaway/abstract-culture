# %% --------

source("base.r")
FIGS_PATH <- "figs/cost/"

# %% ===== action search ======================================================

costs <- read_csv("../results/cost/costs.csv") %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    ) %>% 
    identity

# %% --------

Ds <- with(costs, unique(D))
data <- costs %>% filter(search_cost == 2)

figure("D-act_cost", data %>% 
    # filter(comp_partial == 9) %>% 
    # filter(comp_full == 6) %>% 
    ggplot(aes(D, act_cost, fill=comp_advantage)) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
        no_gridlines +
        scale_x_continuous(trans="log2", breaks=Ds)
)

# %% --------

evolution <- read_csv("../results/cost/evolution.csv")

data <- evolution %>% 
    filter(search_cost == 2, act_cost==9, D==27) %>% 
    filter(gen > 1, gen < 60) %>% 
    mutate(score = 1 - relative(cost, lo=0, hi=10))

figure("evolution", data %>% 
    ggplot(aes(gen)) +
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
)

# %% --------

figure_wrap("costs-full", nrow=3,

    ggplot(costs, aes(search_cost, act_cost, fill=comp_advantage)) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
        no_gridlines +
        facet_wrap(~D, nrow=1),
    
    ggplot(costs, aes(search_cost, act_cost, fill=asymptotic_compositionality)) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0.5) +
        no_gridlines +
        facet_wrap(~D, nrow=1),

    ggplot(costs, aes(search_cost, act_cost, fill=asymptotic_advantage)) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
        no_gridlines +
        facet_wrap(~D, nrow=1)
)
