include("red_black.jl")
include("r.jl")
using Optim

# %% ==================== new learning ====================

df = dataframe(grid(S=[5, 10, 20])) do (;S)
    map([1:10S; 10S:S:3_000]) do D
        p_red = .5
        p_c = p_red * (2S - 1) / S^2
        p_b = (1 - p_red) / S^2

        both = 1 - ((1 - p_c)^D + (1 - p_b)^D - (1 - p_c - p_b)^D)
        compositional = (1 - p_b)^D * (
            1 - (1 - (p_c / (1 - p_b))) ^ D
        )
        neither = (1 - p_c - p_b) ^ D
        idiosyncratic = (1 - both - compositional - neither)

        (;D, both, compositional, neither, idiosyncratic)
    end
end
@rput df


R"""
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

df %>%
    # filter(D < 1000) %>%
    mutate(facet = fct_case_when(
        S == 5 ~ "Small Env (S=5)",
        S == 10 ~ "Medium Env (S=10)",
        S == 20 ~ "Large Env (S=20)",
        TRUE ~ glue("S = {S})"
    ))) %>%
    pivot_longer(c(both, compositional, neither, idiosyncratic), names_to="name", values_to="value", names_prefix="") %>%
    mutate(name = factor(name, ordered=T, levels=c("neither", "idiosyncratic", "compositional", "both"))) %>%
    # mutate(name = factor(name, ordered=T, levels=c("both", "compositional", "idiosyncratic", "neither"))) %>%
    ggplot(aes(D, value, fill=name)) +
    # no_legend +
    geom_area(position = "stack") +
    scale_colour_manual(aesthetics=c("fill", "colour"), name="", values=c(
        both="#318895", compositional=RED, idiosyncratic=TEAL, neither="#66D6E6"
    )) +
    # geom_line() +
    # labs(x="S (number of starts/goals)", y="p(observe my solution)") +
    scale_x_log10(labels=scales::comma) +
    coord_cartesian(expand=F) +
    # scale_y_log10(labels=scales::comma) +
    facet_wrap(~facet, nrow=1, labeller=identity) +
    gridlines +
    labs(x="Number of demonstrations", y="observation probability")
    theme()

fig("p_observe_alt", w=8)
"""
