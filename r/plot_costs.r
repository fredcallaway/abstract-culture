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

df %>% distinct(comp_partial)

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

    plot_layout(ncol=1) &
    coord_equal(xlim=c(0,12), ylim=c(0,12), expand=F) &
    scale_x_continuous(breaks = c(0, 4, 8, 12)) &
    scale_y_continuous(breaks = c(0, 4, 8, 12)) &
    no_gridlines
)

# %% --------

figure("asympotic-full-binary", h=2,
    df %>% ggplot(aes(comp_partial, comp_full, fill=asymptotic_advantage > 0)) +
        geom_tile() +
        facet_grid(S~D) +
        scale_fill_manual(values=c(`FALSE`=C_BESPOKE, `TRUE`=C_COMP), name="comp better") +
        scale_x_continuous(breaks = c(0, 4, 8, 12)) +
        scale_y_continuous(breaks = c(0, 4, 8, 12)) +
    
    df %>% ggplot(aes(comp_partial, comp_full, fill=asymptotic_compositionality > 0.5)) +
        geom_tile() +
        facet_grid(S~D) +
        scale_fill_manual(values=c(`FALSE`=C_BESPOKE, `TRUE`=C_COMP), name="comp used") +
        scale_x_continuous(breaks = c(0, 4, 8, 12)) +
        scale_y_continuous(breaks = c(0, 4, 8, 12)) +
    
    plot_layout(ncol=1)
)

# %% --------

sim <- read_csv("../results/cost/evolution.csv")
sim %>% 
    summarise( min(cost), max(cost) )

figure("cost-evolution.csv", sim %>% 
    filter(S==10, D==81, comp_partial==9, comp_full==8) %>% 
    filter(gen > 1) %>% 
    mutate(score = 1 - relative(cost, lo=0, hi=10)) %>% 
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

# %% ===== SD =================================================================

Ds <- 2 ^ (-1 + 2 * 1:5)

df <- read_csv("../results/cost/cost-asymptote-SD.csv") %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    ) %>% 
    drop_na(asymptotic_compositionality)

figure("tmp", df %>% 
    filter(comp_partial == 9) %>% 
    ggplot(aes(D, S^2, fill=comp_advantage)) +
    geom_raster() +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)") +
    facet_wrap(~comp_full)
)
# %% --------

figure("tmp", df %>% 
    filter(comp_partial == 9) %>% 
    ggplot(aes(D, S^2, fill=asymptotic_compositionality)) +
    geom_raster() +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0.5) +
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)") +
    facet_wrap(~comp_full)
)

# %% --------

figure("tmp", w=3, df %>% 
    filter(comp_partial == 9) %>% 
    filter(comp_full == 8) %>% 
    ggplot(aes(D, S^2, fill=comp_advantage)) +
    geom_raster() +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
    
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)") +

    df %>% filter(comp_partial == 9) %>% 
        filter(comp_full == 8) %>% 
        ggplot(aes(D, S^2, fill=asymptotic_compositionality)) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0.5) +
        
        scale_x_continuous(trans="log2", breaks=Ds) +
        scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
        no_gridlines +
        labs(x="observations (D)", y="possible tasks (S^2)")
)

# %% --------

data <- df %>% 
    filter(comp_partial == 9) %>% 
    filter(comp_full == 8)

figure("tmp", data %>% 
    ggplot(aes(D, S^2, fill=comp_advantage)) +
    geom_raster() +
    geom_raster(data=filter(data, asymptotic_compositionality > 0.5), fill="black") +
    geom_raster(alpha=0.8) +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)")
)

# %% --------

data <- df %>% 
    filter(comp_partial == 9) %>% 
    filter(comp_full == 6)

figure("tmp", data %>% 
    ggplot(aes(D, S^2, fill=comp_advantage)) +
    geom_raster() +
    geom_raster(data=filter(data, asymptotic_compositionality > 0.5), fill=C_COMP, alpha=0.5) +
    geom_raster(alpha=0.2) +
    
    scale_fill_gradient2(low="black", high=GREEN, midpoint=0) +
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)")
)

# %% --------

figure("tmp", data %>% 
    mutate(
        better = comp_advantage > 0,
        used = asymptotic_compositionality > 0.5,
    ) %>% 
    mutate(region = interaction(used, better)) %>% 
    ggplot(aes(D, S^2, fill=region)) +
    geom_tile() +
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)") + 
    scale_fill_manual(values=c(
        "TRUE.TRUE" = C_COMP,
        "TRUE.FALSE" = darken(C_COMP, 0.2),
        "FALSE.TRUE" = darken(C_BESPOKE, 0.2),
        "FALSE.FALSE" = C_BESPOKE
    ))
)
