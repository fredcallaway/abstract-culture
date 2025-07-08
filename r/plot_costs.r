# %% --------

source("base.r")
df <- read_csv("../results/cost/cost-asymptote-grid.csv") %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    ) %>% 
    drop_na(asymptotic_compositionality)

df %>% summarise(
    max(comp_cost),
    max(asymptotic_advantage)
)

df %>% filter(comp_cost != asymptotic_cost)

FIGS_PATH <- "figs/cost/"

figure("asympotic-full", h=2,
    df %>% ggplot(aes(comp_partial, comp_full, fill=comp_advantage)) +
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
    df %>% ggplot(aes(comp_partial, comp_full, fill=comp_advantage > 0)) +
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
    summarise( min(cost), max(cost), max(compositionality) )


figure("cost-evolution.csv", sim %>% 
    filter(S==10, D==81, comp_partial==9, comp_full==8) %>% 
    filter(gen > 1, gen < 30) %>% 
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
MAKE_PDF <- F

df <- read_csv("../results/cost/cost-asymptote-SD.csv") %>% 
    mutate(
        asymptotic_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    ) %>% 
    drop_na(asymptotic_compositionality)


df %>% with(max(comp_cost))

figure("SD-advantage", df %>% 
    filter(comp_partial == 9) %>% 
    ggplot(aes(D, S^2, fill=asymptotic_advantage)) +
    geom_raster() +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)") +
    facet_wrap(~comp_full)
)
# %% --------

figure("SD-compositionality", df %>% 
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

figure("SD-simple", w=3, df %>% 
    filter(comp_partial == 9) %>% 
    filter(comp_full == 6) %>% 
    ggplot(aes(D, S^2, fill=asymptotic_advantage)) +
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
    ggplot(aes(D, S^2, fill=asymptotic_advantage)) +
    geom_raster() +
    # geom_raster(data=filter(data, asymptotic_compositionality > 0.5), fill="black") +
    # geom_raster(alpha=0.8) +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)")
)

# %% --------

data <- df %>% 
    filter(comp_partial == 9) %>% 
    filter(comp_full == 7) %>% 
    mutate(
        better = asymptotic_advantage > 0.01,
        used = asymptotic_compositionality > 0.1,
        region = interaction(used, better)
    )

figure("tmp", w=3,
    ggplot(data, aes(D, S^2)) +
        geom_raster(data=filter(data, used), fill=C_COMP) +
        geom_raster(data=filter(data, !used), fill=C_BESPOKE) +
        geom_raster(data=filter(data, better), fill=BLACK, alpha=0.5) +
        # geom_raster(data=filter(data, !better), fill=WHITE, alpha=0.2) +
        scale_x_continuous(trans="log2", breaks=Ds) +
        scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
        no_gridlines +
        labs(x="observations (D)", y="possible tasks (S^2)") +

    
    ggplot(data, aes(D, S^2, fill=region)) +
        geom_tile() +
        scale_x_continuous(trans="log2", breaks=Ds) +
        scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
        no_gridlines +
        labs(x="observations (D)", y="possible tasks (S^2)") + 
        scale_fill_manual(values=c(
            "TRUE.TRUE" = "#a05e3d",
            "TRUE.FALSE" = C_COMP,
            "FALSE.TRUE" = "#565656",
            "FALSE.FALSE" = "white"
        ))
)

# %% --------

figure("tmp", data %>% 
    ggplot(aes(D, S^2)) +
        geom_raster(aes(fill = used)) +
        geom_tile(data = filter(data, better), color = "black", linewidth = 1, 
                  fill = NA, alpha = 0) +
        scale_fill_manual(values = c(`FALSE` = C_BESPOKE, `TRUE` = C_COMP), name = "comp used") +
        scale_x_continuous(trans="log2", breaks=Ds) +
        scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
        no_gridlines +
        labs(x="observations (D)", y="possible tasks (S^2)")
)

# %% ===== D-full =============================================================


df <- read_csv("../results/cost/cost-asymptote-D-full.csv") %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    ) %>% 
    identity
    # drop_na(asymptotic_compositionality)



figure("D-full", w=3, df %>% 
    # filter(comp_partial == 9) %>% 
    # filter(comp_full == 6) %>% 
    ggplot(aes(D, comp_full, fill=comp_advantage)) +
    geom_raster() +
    scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
    scale_x_continuous(trans="log2", breaks=Ds) +
    no_gridlines +

    df %>% 
        ggplot(aes(D, comp_full, fill=1 * (asymptotic_compositionality > 0.5))) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0.5) +
        
        scale_x_continuous(trans="log2", breaks=Ds) +
        no_gridlines
)


# %% ===== action search ======================================================

df <- read_csv("../results/cost/cost-asymptote-action-search.csv") %>% 
    mutate(
        comp_advantage = (bespoke_cost - comp_cost) / 10,
        asymptotic_advantage = (bespoke_cost - asymptotic_cost) / 10
    ) %>% 
    identity


figure("action-search", w=3, df %>% 
    # filter(comp_partial == 9) %>% 
    # filter(comp_full == 6) %>% 
    ggplot(aes(act_cost, search_cost, fill=asymptotic_advantage)) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0) +
        no_gridlines +

    df %>% 
        ggplot(aes(act_cost, search_cost, fill=asymptotic_compositionality)) +
        geom_raster() +
        scale_fill_gradient2(low=C_BESPOKE, high=C_COMP, mid="gray", midpoint=0.5) +
        no_gridlines
)
