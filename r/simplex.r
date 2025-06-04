suppressPackageStartupMessages(library(ggtern))
source("base.r")
FIGS_PATH <- "figs/model/"

# %% --------

df <- read_csvs("../results/simplex.csv")

# Calculate direction vectors and normalize them
df_normalized <- df %>% 
    rename(Z = bespoke_zilch, F = bespoke_full, C = comp) %>%
    mutate(
        # Calculate direction vectors
        dZ = bespoke_zilch_end - Z,
        dF = bespoke_full_end - F,
        dC = comp_end - C,
        # Calculate magnitude
        magnitude = sqrt(dZ^2 + dF^2 + dC^2),
        # Normalize to unit length (but keep some reasonable arrow size)
        arrow_scale = 0.08,  # Adjust this to change arrow size
        # Calculate normalized endpoints
        Z_end = Z + ifelse(magnitude > 0, arrow_scale * dZ / magnitude, 0),
        F_end = F + ifelse(magnitude > 0, arrow_scale * dF / magnitude, 0),
        C_end = C + ifelse(magnitude > 0, arrow_scale * dC / magnitude, 0)
    )

# %% --------




figure("simplex_field_single", df_normalized %>% 
    filter(S == 5, D == 3) %>% 
    ggtern(aes(Z,F,C)) + 
    geom_segment(aes(xend=Z_end, yend=F_end, zend=C_end, color=magnitude),
                 arrow=arrow(length=unit(0.02, "npc"))) +
    limit_tern(T = 1.05, L = 1.05, R = 1.05) +
    theme(
      tern.axis.text.T = element_blank(),  # Top axis
      tern.axis.text.L = element_blank(),  # Left axis
      tern.axis.text.R = element_blank()   # Right axis
    ) + 
    theme_nogrid() +
    theme_noticks() +
    no_legend +
    scale_color_gradient(low = "gray80", high = "black")
)

# %% --------

figure("simplex_field", df_normalized %>% 
    ggtern(aes(Z,F,C)) + 
    geom_segment(aes(xend=Z_end, yend=F_end, zend=C_end, color=magnitude),
                 arrow=arrow(length=unit(0.02, "npc"))) +
    limit_tern(T = 1.05, L = 1.05, R = 1.05) +
    facet_grid(S~D) + 
    theme(
      tern.axis.text.T = element_blank(),  # Top axis
      tern.axis.text.L = element_blank(),  # Left axis
      tern.axis.text.R = element_blank()   # Right axis
    ) + 
    theme_nogrid() +
    no_legend
)
