source("base.r")
FIGS_PATH <- "figs/model/"

df = read_csv("../results/asymptote.csv")

df %>% 
    filter(D > 0) %>% 
    # mutate(S = factor(S^2)) %>% 
    # mutate(D = factor(D)) %>% 
    ggplot(aes(D, S^2, fill=asymptote)) +
    geom_tile() +
    scale_fill_gradient(low=C_BESPOKE, high=C_COMP) +
    scale_x_continuous(trans="log2") +
    scale_y_continuous(trans="sqrt", breaks=seq(2, 20, by=6) ^ 2) +
    no_gridlines +
    labs(x="observations (D)", y="possible tasks (S^2)", fill="asymptotic compositionality")

fig("asymptote", w=1.7)
