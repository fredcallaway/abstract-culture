# %% --------

source("base.r")
df <- read_csv("../results/cost/cost-asymptote-grid.csv") %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    ) %>% 
    drop_na(asymptotic_compositionality)

df %>% summarise(
    min(comp_cost),
    min(asymptotic_advantage)
)

# %% --------

figure("asympotic-full", h=2,
    df %>% ggplot(aes(comp_partial, comp_full, fill=asymptotic_advantage)) +
        geom_tile() +
        facet_grid(S~D) +
        # scale_fill_continuous_diverging()
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
    # expand_limits(fill=c(-1, 1))
    
    df %>% ggplot(aes(comp_partial, comp_full, fill=asymptotic_compositionality)) +
        geom_tile() +
        facet_grid(S~D) +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0.5) +
        expand_limits(fill=c(0,1)) +
    
    plot_layout(ncol=1)
)

# %% --------

figure("asympotic-full-binary", h=2,
    df %>% ggplot(aes(comp_partial, comp_full, fill=asymptotic_advantage > 0)) +
        geom_tile() +
        facet_grid(S~D) +
        scale_fill_manual(values=c(`FALSE`=C_BESPOKE, `TRUE`=C_COMP), name="comp better") +
    
    df %>% ggplot(aes(comp_partial, comp_full, fill=asymptotic_compositionality > 0.5)) +
        geom_tile() +
        facet_grid(S~D) +
        scale_fill_manual(values=c(`FALSE`=C_BESPOKE, `TRUE`=C_COMP), name="comp used") +
    
    plot_layout(ncol=1)
)

# %% --------

sim <- read_csv("../results/cost/evolution.csv")
sim %>% 
    summarise( min(cost), max(cost) )

figure("cost-evolution.csv", sim %>% 
    filter(S==10, D==81, comp_partial==10, comp_full==9) %>% 
    filter(gen > 1) %>% 
    mutate(score = 1 - relative(cost, lo=1, hi=10)) %>% 
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
