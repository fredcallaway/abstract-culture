# %% --------

source("base.r")
df <- read_csv("../results/cost/cost-asymptote-grid.csv") %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    ) %>% 
    drop_na(asymptotic_compositionality)

df %>% summarise(
    min(comp_advantage),
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

figure("tmp", df %>% 
    ggplot(aes(comp_partial, comp_full, fill=asymptotic_compositionality)) +
    geom_tile() +
    facet_grid(S~D) +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0.5) +
    expand_limits(fill=c(0,1))
)

# %% --------

figure("tmp", df %>% 
    ggplot(aes(comp_partial, comp_full, fill=asymptotic_advantage)) +
    geom_tile() +
    facet_grid(S~D) +
    # scale_fill_continuous_diverging()
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
    # expand_limits(fill=c(-1, 1))
    theme()
)

# %% --------

figure("tmp", df %>% 
    ggplot(aes(comp_partial, comp_full, fill=asymptotic_compositionality)) +
    geom_tile() +
    facet_grid(S~D) +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0.5) +
    expand_limits(fill=c(0,1))
)

# %% --------

figure("tmp", df %>% 
    ggplot(aes(comp_partial, comp_full, fill=bespoke_cost > comp_cost)) +
    geom_tile() +
    facet_grid(S~D) +
    scale_fill_manual(values=c(`FALSE`=C_BESPOKE, `TRUE`=C_COMP), name="comp better")
)

# %% --------

figure("tmp", df %>% 
    ggplot(aes(comp_partial, comp_full, fill=asymptotic_compositionality > 0.5)) +
    geom_tile() +
    facet_grid(S~D) +
    scale_fill_manual(values=c(`FALSE`=C_BESPOKE, `TRUE`=C_COMP), name="comp used")
)

# %% --------

figure("tmp", df %>% 
    ggplot(aes(comp_partial, comp_full, fill=bespoke_cost - comp_cost)) +
    geom_tile() +
    facet_grid(S~D) +
    scale_fill_continuous_diverging()
)

# %% --------

figure("tmp", df %>% 
    ggplot(aes(comp_partial, comp_full, fill=asymptotic_compositionality)) +
    geom_tile() +
    facet_grid(S~D)
)
