suppressPackageStartupMessages(library(ggtern))
source("base.r")
FIGS_PATH <- "figs/model/"

# %% --------

df <- read_csvs("../results/simplex.csv")

# Calculate direction vectors and normalize them
df_normalized <- df %>% 
    mutate(
        # Calculate direction vectors
        d_indiv = indiv_end - indiv,
        d_bespoke = bespoke_end - bespoke,
        d_comp = comp_end - comp,
        # Calculate magnitude
        magnitude = sqrt(d_indiv^2 + d_bespoke^2 + d_comp^2),
        # Normalize to unit length (but keep some reasonable arrow size)
        arrow_scale = 0.08,  # Adjust this to change arrow size
        # Calculate normalized endpoints
        indiv_end = indiv + ifelse(magnitude > 0, arrow_scale * d_indiv / magnitude, 0),
        bespoke_end = bespoke + ifelse(magnitude > 0, arrow_scale * d_bespoke / magnitude, 0),
        comp_end = comp + ifelse(magnitude > 0, arrow_scale * d_comp / magnitude, 0)
    )

# %% --------

figure("simplex_field_single", df_normalized %>% 
    filter(S == 5, D == 3) %>% 
    ggtern(aes(indiv,bespoke,comp)) + 
    geom_segment(aes(xend=indiv_end, yend=bespoke_end, zend=comp_end, color=magnitude),
                 arrow=arrow(length=unit(0.02, "npc"))) +
    limit_tern(T = 1.05, L = 1.05, R = 1.05) +
    theme(
      tern.axis.text.T = element_blank(),  # Top axis
      tern.axis.text.L = element_blank(),  # Left axis
      tern.axis.text.R = element_blank(),   # Right axis
      # tern.axis.title.T = element_text(hjust = 0.5, vjust = -1),
      tern.axis.title.L = element_text(hjust = 0, vjust = 1.2),
      tern.axis.title.R = element_text(hjust = 1, vjust = 1.2)
    ) + 
    theme_nogrid() +
    theme_noticks() +
    no_legend +
    scale_color_gradient(low = "gray80", high = "black")
)

# %% --------

# figure("simplex_field", df_normalized %>% 
#     ggtern(aes(Z,F,C)) + 
#     geom_segment(aes(xend=Z_end, yend=F_end, zend=C_end, color=magnitude),
#                  arrow=arrow(length=unit(0.02, "npc"))) +
#     limit_tern(T = 1.05, L = 1.05, R = 1.05) +
#     facet_grid(S~D) + 
#     theme(
#       tern.axis.text.T = element_blank(),  # Top axis
#       tern.axis.text.L = element_blank(),  # Left axis
#       tern.axis.text.R = element_blank()   # Right axis
#     ) + 
#     theme_nogrid() +
#     no_legend
# )
