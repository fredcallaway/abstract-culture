source("base.r")
FIGS_PATH <- "figs/model/"

# %% --------

fixed_points <- read_csv("../results/fixed_points.csv")

Ds <- 2 ^ (-1 + 2 * 1:5)
ev_points <- tibble(S=10, D = Ds)

# %% --------

plot_line <- function(fun) {
    stat_function(fun = fun, color = "black", linetype = "dashed", inherit.aes = FALSE)
}

asymptote_plot <- fixed_points %>% 
    filter(D > 0) %>% 
    # mutate(S = factor(S^2)) %>% 
    # mutate(D = factor(D)) %>% 
    ggplot(aes(D, S^2, fill=asymptote)) +
    geom_raster() +
    scale_fill_gradient(low=C_BESPOKE, high=C_COMP) +
    scale_x_continuous(trans="log2", breaks=Ds) +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    geom_point(data=ev_points, mapping=aes(fill=NULL), color=BLACK, size=2) +
    # plot_line(function(x) x) +
    # plot_line(function(x) x^2) +
    # coord_cartesian(ylim=extrema(fixed_points$S)^2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)", fill="asymptotic compositionality")

fig("asymptote", w=1.7)

# %% --------

evolution <- read_csv("../results/evolution.csv")

evolution_plot <- evolution %>% 
    right_join(ev_points) %>% 
    ggplot(aes(gen, compositionality)) +
    geom_line(color=C_COMP) + expand_limits(y=c(0, 1)) +
    no_legend +
    # geom_point(data=filter(df, gen == 0), color=RED) +
    facet_wrap(~D, nrow=1) +
    labs(x="generation") +
    no_gridlines +
    theme()

fig("evolution", w=3)

# %% --------

phase <- read_csv("../results/phase.csv")

D <- phase %>%
    filter(S==10, D==8) %>% 
    mutate(
        segment = pmax(1, cumsum(replace_na(sign(dc) - lag(sign(dc)) != 0, FALSE))),
        attractor = factor(segment, labels=c("compositional", "compositional"))
    ) %>%
    group_by(segment) %>%
    mutate(direction = sign(last(dc)))

fp <- fixed_points %>% 
    filter(S==10, D==8)

phase_plot <- ggplot(D) +
    geom_hline(yintercept=0, linetype="solid", color=GRAY) +
    geom_line(mapping=aes(c, dc), linewidth=.5) +
    # annotate("rect", xmin = 0, xmax = stable$start, ymin = -Inf, ymax = Inf, alpha = .3, fill=TEAL) +
    # annotate("rect", xmin = stable$start, xmax = 1, ymin = -Inf, ymax = Inf, alpha = .25, fill=RED) +
    lapply(split(D, D$segment), function(df)
      geom_line(data = df, aes(c, 0, color = attractor),
                size=.8,
                arrow = arrow(
                     length = unit(.1, "in"), type = 'open',
                     ends = if (first(df$direction) < 0) 'first' else 'last',
                ))
    ) +
    geom_point(x=0, y=0, shape=21, size=2, fill=C_BESPOKE) +
    geom_point(data=fp, aes(x=start, y=0), shape=21, size=2, fill="white") +
    geom_point(data=fp, aes(x=stop, y=0), shape=21, size=2, fill=C_COMP) +
    no_gridlines +
    # coord_fixed(expand=T) +
    # labs(x="previous compositionality rate", y="new compositionality rate") +
    labs(x="compositionality", y="change in compositionality") +
    cpal + no_legend

fig("phase")

# %% --------

(phase_plot + asymptote_plot) / evolution_plot + plot_layout(heights=c(1.8, 1)) + plot_annotation(tag_levels="A")

fig("model_combined", w=3.5, h=2)
