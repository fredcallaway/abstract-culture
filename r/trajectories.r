source("base.r")

df <- read_csv("../results/trajectories.csv")

figure("trajectories", df %>% 
       ggplot(aes(comp_full, comp_partial)) +
       geom_path(aes(color=gen)) +
       geom_point(aes(color=gen)) +
       lims(x=c(0, 1), y=c(0, 1)) +
       facet_grid(S~D) +
       theme()
)