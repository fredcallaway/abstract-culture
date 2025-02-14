# %% --------
suppressPackageStartupMessages(source("base.r"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
    # Pull from experiment config
    config <- readLines("../machine-task/config.txt")
    version <- gsub("experiment_code_version = (.*)", "\\1", config[grep("experiment_code_version", config)])
} else {
    version <- args[1]
}

version <- "code-pilot-v12"
versions <- c(version)
FIGS_PATH <- glue("figs/codes/{version}/")

df <- read_csvs(c(version), "trials") |>
    mutate(
        # full_solution_type = paste0(solution_type, if_else(used_manual, "-M", "-?")),
        full_solution_type = paste0(solution_type, if_else(used_manual, "-M", "-?")),
        # compositional = factor(compositional, levels = c("none", "partial", "full", "exact")),
        compositional = factor(compositional, levels = c("none", "partial", "full", "exact")),
        version = substr(version, 12, 15)
    ) |>
    labelize(bespoke, "available", "unavailable")
    # mutate(pid = paste0(version, "-", as.character(dense_rank(pid))), .by=version)

df %>% count(pid)
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

nrow(df)

# %% --------
df |>
    mutate(solution_type = if_else(n_button_bespoke > 20, "bespoke", solution_type)) |>
    ggplot(aes(compositional, 1 * (solution_type == "compositional"))) +
    bars(fill=RED) +
    facet_grid(~bespoke) +
    labs(x="Compositional in Manual?", y="Compositional Rate")

fig("compositional-rate", w=5)

# %% --------

df %>% filter(bespoke=="unavailable", compositional=="none") %>% 
    ggplot(aes(n_button_bespoke, n_button_compositional)) +
    geom_jitter()

fig()
    
# %% --------

df |> 
    # filter(duration < quantile(duration, .95)) |> 
    ggplot(aes(compositional, duration, color=solution_type)) +
    geom_quasirandom(size=.2) + cpal +# %% --------yy
    points() +
    facet_wrap(~bespoke)

fig("solution-time", w=6, h=2.5)

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
    ggplot(aes(compositional, 1*used_manual)) +
    bars() +
    facet_grid(~bespoke) +
    labs(x="Compositional in Manual?", y="Manual Usage")

fig("manual-rate", w=5)

# %% --------


df %>% 
    # filter(used_manual) %>% 
    mutate(rt_button = pmin(rt_bespoke, rt_comp, na.rm=TRUE)) %>% 
    pivot_longer(c(rt_select, rt_button)) %>% 
    ggplot(aes(pid, value, color=name)) +
    geom_line(aes(group=interaction(trial_number, pid)), color="gray", linewidth=0.5) +
    geom_point() +
    ggplot2::facet_grid(bespoke~compositional) +
    no_xaxis_ticks
    # coord_cartesian(ylim=c(0, 120))

fig("rt", w=8, h=2.8)

# %% --------

df %>% 
    filter(trial_number == 1) %>% 
    mutate(rt_button = pmin(rt_bespoke, rt_comp, na.rm=TRUE)) %>% 
    pivot_longer(c(rt_select, duration, rt_button)) %>% 
    ggplot(aes(pid, value, color=name)) +
    geom_line(aes(group=interaction(trial_number, pid)), color="gray", linewidth=0.5) +
    geom_point() +
    ggplot2::facet_grid(bespoke~compositional) +
    no_xaxis_ticks +
    coord_cartesian(ylim=c(0, 120))

fig("rt-trial", w=8, h=2.8)

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

# %% ===== timing =======================================================

button_times <- read_csvs(versions, "button_times")

button_times |> 
    mutate(delay = time - lag(time)) %>% 
    filter(delay < 2, delay>0) |> 
    ggplot(aes(delay, fill=kind)) +
    geom_histogram(position="identity", alpha=0.8)

fig()

# %% ===== events =============================================================

events <- read_csvs(versions, "events")
info <- df %>% transmute( pid, trial_number, info = paste0(substr(bespoke, 1, 2), "-", substr(compositional, 1, 2)))
events %>% 
    left_join(info, by=c("trial_number", "pid")) %>% 
    filter(time < 120) %>% 
    ggplot(aes(time, trial_number, color=kind, shape=event, size=event)) +
    scale_size_manual(values = c(button=1.4, dial=1.8, solution=4)) +
    geom_label(aes(label=info, x=-7, shape=NULL, size=NULL, color=NULL), data=info, size=3, color="black") +
    geom_vline(xintercept = 0, color="black", linetype="solid") +
    scale_shape_manual(values = c(button='square', dial='circle', solution='diamond')) +
    geom_point() +
    scale_color_manual(values = c(compositional=RED, bespoke=TEAL, wrong=GRAY, ambiguous="#80b056")) +
    facet_wrap(~pid, ncol=4)
fig("events", w=20, h=13)

# %% ===== feedback ===========================================================

feedback <- read_csvs(versions, "feedback")

feedback$feedback
feedback$manual
feedback$preference

# %% --------

read_csvs(versions, "instructions-survey") %>% 
    count(hints)
