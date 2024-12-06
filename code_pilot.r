# %% --------
suppressPackageStartupMessages(source("base.r"))

version <- "code-pilot-v4"
FIGS_PATH <- glue("figs/codes/code-pilot-{version}/")

df <- read_csv(glue("data/{version}/trials.csv")) |>
    mutate(
        # full_solution_type = paste0(solution_type, if_else(used_manual, "-M", "-?")),
        full_solution_type = paste0(solution_type, if_else(used_manual, "-M", "-?")),
        compositional = factor(compositional, levels = c("none", "partial", "full", "exact")),
        version = substr(version, 12, 15)
    ) |> labelize(bespoke, "available", "unavailable")



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

df


# %% --------

df |>
    ggplot(aes(compositional, fill = full_solution_type)) +
    geom_bar(position = "fill") +
    coord_equal(ratio = 3) +
    ggplot2::facet_grid(bespoke~version) +
    labs(x="compositional manual info", y="proportion",
    fill="solution type",
    caption="-M means by manual, -? means by guessing") +
    spal
DPI <- 300
fig("solution-rates", w=8, h=4)
# %% --------
df |>
    ggplot(aes(compositional, 1 * (solution_type == "compositional"), fill = bespoke)) +
    stat_summary(fun = mean, geom = "bar", position = "dodge") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .3, position = position_dodge(width = 1)) +
    ylab("Compositional solution") +
    coord_equal(ratio = 3) +
    facet_wrap(~version)

# %% --------

# %% --------

df |> 
    filter(bespoke == "unavailable") |>
    ggplot(aes(compositional, duration, color=solution_type)) +
    geom_quasirandom(size=.2) + cpal +
    stat_summary(fun = mean, size = .5, shape = 4) +
    facet_wrap(~version) +
    labs(caption="bespoke unavailable")

fig("solution-time", w=8, h=2.5)
# %% --------

df |>
    filter(compositional == "partial", bespoke == "unavailable") |> 
    select(duration, solution_type)

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

pids = df |> 
    distinct(uid, version) |> 
    group_by(version) |>
    mutate(
        pid = paste0(version, "-", as.character(row_number()))
    )

df = df |> left_join(pids)
# %% --------

df |>
    mutate(uid = substr(uid, 12, 100)) |>
    mutate(bespoke = substr(bespoke, 1, 1)) |>
    ggplot(aes(bespoke, compositional, fill = full_solution_type)) +
    geom_tile() +
    ggplot2::facet_wrap(~pid, ncol = 15) +
    coord_equal() + spal

fig("individual-solutions", w = 10, h = 5)


# %% --------

df |>
    filter(compositional == "none") |>
    ggplot(aes(bespoke, fill = full_solution_type)) +
    geom_bar(position = "fill") +
    facet_wrap(~uid, ncol = 14) +
    coord_equal()
fig("compositional-bespoke-none", w = 10, h = 5)