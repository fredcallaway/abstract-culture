# %% --------
suppressPackageStartupMessages(source("base.r"))

version <- "code-pilot-v45"
FIGS_PATH <- glue("figs/codes/code-pilot-{version}/")

df <- read_csv(glue("data/{version}/trials.csv")) |>
    mutate(
        # full_solution_type = paste0(solution_type, if_else(used_manual, "-M", "-?")),
        full_solution_type = paste0(solution_type, if_else(used_manual, "-M", "-?")),
        compositional = factor(compositional, levels = c("none", "partial", "full", "exact")),
        version = substr(version, 12, 15)
    ) |>
    labelize(bespoke, "available", "unavailable") |> 
    mutate(pid = paste0(version, "-", as.character(dense_rank(uid))), .by=version)

# %% --------

DPI <- 300
RED <- "#E86623"
TEAL <- "#07A9C0"

cpal <- scale_colour_manual(values = c(
    bespoke = TEAL,
    compositional = RED,
    `-1` = TEAL,
    `1` = RED
), aesthetics = c("fill", "colour"), name = "")

spal <- scale_fill_manual(values = c(
    "bespoke-M" = TEAL,
    "bespoke-?" = lighten(TEAL, .5),
    "compositional-M" = RED,
    "compositional-?" = lighten(RED, .5)

))

# %% --------

df |>
    ggplot(aes(compositional, fill = full_solution_type)) +
    geom_bar(position = "fill") +
    coord_equal(ratio = 3) +
    # ggplot2::facet_grid(bespoke~version) +
    facet_grid(~bespoke) +
    labs(x="compositional manual info", y="proportion",
         fill="solution type",
         caption="-M means by manual, -? means by guessing") +
    spal
fig("solution-rates", w=8, h=4)

# %% --------
df |>
    ggplot(aes(compositional, 1 * (solution_type == "compositional"))) +
    bars(fill=RED) +
    facet_grid(~bespoke) +
    labs(x="Compositional in Manual?", y="Compositional Rate")

fig("compositional-rate", w=5)

# %% --------

df |>
    ggplot(aes(compositional, 1*used_manual)) +
    bars() +
    facet_grid(~bespoke) +
    labs(x="Compositional in Manual?", y="Manual Usage")

fig("manual-rate", w=5)

# %% --------



df |> 
    filter(duration < quantile(duration, .95)) |> 
    ggplot(aes(compositional, duration, color=solution_type)) +
    geom_quasirandom(size=.2) + cpal +
    points() +
    facet_wrap(~bespoke)

fig("solution-time", w=6, h=2.5)

# %% --------

df |>
    mutate(uid = substr(uid, 12, 100)) |>
    group_by(uid, version) |>
    summarise(
        compositional = mean(solution_type == "compositional"),
        duration = mean(duration),
        n_try = mean(n_try),
        n_green = mean(n_green),
        used_locks = mean(used_locks > 0),
        used_manual = mean(used_manual)
    )

# %% --------

df |>
    # drop duplicate trials
    slice_head(n=1, by=c(pid, bespoke, compositional)) |> 
    mutate(bespoke = substr(bespoke, 1, 2)) |>
    ggplot(aes(bespoke, compositional, fill = full_solution_type)) +
    geom_tile() +
    ggplot2::facet_wrap(~pid, nrow = 2) +
    coord_equal() + spal + no_gridlines

fig("individual-solutions", w = 10, h = 5)

# %% --------

df |> 
    filter(bespoke == "unavailable", compositional == "none", solution_type == "compositional") |> 
    select(uid, trial_number, n_button_compositional)


# %% --------

df |>
    filter(compositional == "none") |>
    ggplot(aes(bespoke, fill = full_solution_type)) +
    geom_bar(position = "fill") +
    facet_wrap(~uid, ncol = 14) +
    coord_equal()
fig("compositional-bespoke-none", w = 10, h = 5)

# %% --------

df |> 
    filter(compositional == "none", solution_type == "compositional") |> 
    ggplot(aes(n_button_compositional)) +
    geom_bar()

fig()
