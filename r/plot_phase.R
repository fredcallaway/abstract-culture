source("base.r")
FIGS_PATH <- "figs/model/"

df = read_csv("../results/phase.csv")
stable = read_json("../results/stable.json")

D = df %>%
    mutate(
        segment = pmax(1, cumsum(replace_na(sign(dc) - lag(sign(dc)) != 0, FALSE))),
        attractor = factor(segment, labels=c("compositional", "compositional"))
    ) %>%
    group_by(segment) %>%
    mutate(direction = sign(last(dc)))

# %% --------

ggplot(D) +
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
    geom_point(x=stable$start, y=0, shape=21, size=2, fill="white") +
    geom_point(x=stable$stop, y=0, shape=21, size=2, fill=C_COMP) +
    no_gridlines +
    # coord_fixed(expand=T) +
    # labs(x="previous compositionality rate", y="new compositionality rate") +
    labs(x="compositionality", y="change in compositionality") +
    cpal + no_legend

fig("phase")
