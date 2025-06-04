source("base.r")
library(ggtern)

# %% --------

df <- read_csv("../results/simplex.csv")

figure("simplex_field", df %>% 
    rename(Z = bespoke_zilch, F = bespoke_full, C = comp) %>%
    ggtern(aes(Z,F,C)) + 
    geom_segment(aes(xend=bespoke_zilch_end, yend=bespoke_full_end, zend=comp_end),
                 arrow=arrow(length=unit(0.02, "npc")), color=RED) +
    limit_tern(T = 1.05, L = 1.05, R = 1.05) +
    facet_grid(S~D) +
    theme(
      tern.axis.text.T = element_blank(),  # Top axis
      tern.axis.text.L = element_blank(),  # Left axis
      tern.axis.text.R = element_blank()   # Right axis
    )
)

