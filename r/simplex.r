source("base.r")
library(ggtern)

df <- read_csv("../results/chains.csv")

# %% --------

df %>% 
    ggtern(aes(bespoke_zilch,bespoke_full,comp)) + 
    geom_line(aes(color=gen)) +
    limit_tern(T = 1.05, L = 1.05, R = 1.05)

fig(w=3, h=2)


# %% --------

df <- read_csv("../results/simplex.csv")
nrow(df)
df %>% 
    ggtern(aes(bespoke_zilch,bespoke_full,comp)) + 
    geom_segment(aes(xend=bespoke_zilch_end, yend=bespoke_full_end, zend=comp_end),
                 arrow=arrow(length=unit(0.02, "npc")), color=RED) +
    limit_tern(T = 1.05, L = 1.05, R = 1.05)

fig(w=3, h=2)

