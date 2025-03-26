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
versions <- c(version)
FIGS_PATH <- glue("figs/codes/{version}/")

participants <- read_csvs(versions, "participants") %>% 
    select(-c(useragent, wid, active_minutes))

df <- read_csvs(c(version), "trials") |>
    mutate(
        # full_solution_type = paste0(solution_type, if_else(used_manual, "-M", "-?")),
        # full_solution_type = paste0(solution_type, if_else(used_manual, "-M", "-?")),
        # compositional = factor(compositional, levels = c("none", "partial", "full", "exact")),
        compositional = factor(compositional, levels = c("none", "partial", "full", "exact")),
        is_compositional = 1* (solution_type == "compositional"),
        version = substr(version, 12, 15)
    ) |>
    labelize(bespoke, "available", "unavailable") %>% 
    mutate(trial_type = paste0(
        "B", if_else(bespoke=="available", "*", "0"), 
        "-", 
        "C", case_when(
            compositional=="none" ~ "0",
            compositional=="partial" ~ "1",
            compositional=="full" ~ "2",
            compositional=="exact" ~ "*"
        )
    )) %>% 
    left_join(select(participants, pid, bespokeSide), by="pid")


df %>% 
    group_by(bespoke, compositional) %>% 
    summarise(p_compositional = mean(solution_type == "compositional")) %>% 
    write_csv(glue("tmp/compositional-rates-{version}.csv"))


DPI <- 300
RED <- "#E86623"
TEAL <- "#07A9C0"

cpal <- scale_colour_manual(values = c(
    bespoke = TEAL,
    compositional = RED,
    `-1` = TEAL,
    `1` = RED
), aesthetics = c("fill", "colour"), name = "")


print(glue("----- ANALYZING VERSION {version} -----"))
print(glue("{length(unique(df$pid))} participants and {nrow(df)} trials"))

# %% --------

df |>
    mutate(compositionalSide = if_else(bespokeSide == "left", "right", "left")) |>
    # mutate(solution_type = if_else(n_button_bespoke > 20, "bespoke", solution_type)) |>
    ggplot(aes(compositionalSide, 1 * (solution_type == "compositional"))) +
    bars(fill=RED) +
    ylim(0, 1) +
    labs(x="Compositional Side", y="Compositional Rate")

fig("compositional-rate-single")

# %% --------

df |>
    # mutate(solution_type = if_else(n_button_bespoke > 20, "bespoke", solution_type)) |>
    ggplot(aes(compositional, 1 * (solution_type == "compositional"))) +
    bars(fill=RED) +
    facet_grid(~bespoke) +
    ylim(0, 1) +
    labs(x="Compositional in Manual?", y="Compositional Rate")

fig("compositional-rate", w=5)

# %% --------

df |> 
    filter(duration < quantile(duration, .95)) |> 
    ggplot(aes(compositional, duration, color=solution_type)) +
    geom_quasirandom(size=.2) + cpal +# %% --------yy
    points() +
    facet_wrap(~bespoke)

fig("solution-time", w=6, h=2.5)

# %% --------

besp_dials <- 4
comp_dials <- 3
avg_dial_tries <- 4.5

expected <- distinct(df, bespoke, compositional, solution_type) %>% 
    rowwise() %>% 
    mutate(n_try = case_when(
        solution_type == "bespoke" && bespoke == "available" ~ besp_dials,
        solution_type == "bespoke" && bespoke == "unavailable" ~ besp_dials * avg_dial_tries,
        solution_type == "compositional" && compositional == "none" ~ 2 * comp_dials * avg_dial_tries,
        solution_type == "compositional" && compositional == "partial" ~ comp_dials + comp_dials * avg_dial_tries,
        solution_type == "compositional" && compositional == "full" ~ 2 * comp_dials,
        solution_type == "compositional" && compositional == "exact" ~ 2 * comp_dials,
        TRUE ~ NA_real_
    ))


chance_lines <- function(yvar, linetype="dashed", ...) {
    geom_errorbar(mapping=aes(y={{yvar}}, ymin = {{yvar}}, ymax = {{yvar}}), linetype = linetype, size=.3, data=expected, ...)
}

df |> 
    # filter(duration < quantile(duration, .95)) |> 
    ggplot(aes(compositional, n_try, color=solution_type)) +
    chance_lines(n_try, linetype="solid") +
    geom_quasirandom(size=.4) + cpal +
    stat_summary(fun=mean, shape='X', geom="point", size=3) +
    facet_wrap(~bespoke)

fig("ntry", w=6, h=2.5)

# %% --------

df |>
    # drop duplicate trials
    slice_head(n=1, by=c(pid, bespoke, compositional)) |> 
    mutate(bespoke = substr(bespoke, 1, 2)) |>
    ggplot(aes(bespoke, compositional, fill = solution_type)) +
    geom_tile() +
    ggplot2::facet_wrap(~pid, nrow = 2) +
    coord_equal() + cpal + no_gridlines

fig("individual-solutions", w = 10, h = 5)

# %% ===== timing =======================================================

times <- read_csvs(versions, "times") 

times %>% summarise(across(-c(pid, version), mean))

times %>% 
    mutate(expected = debrief + instructions + main / 8) %>% 
    summarise(mean(expected), median(expected))


# %% --------

button_times <- read_csvs(versions, "button_times")

button_times |> 
    mutate(delay = time - lag(time)) %>% 
    filter(delay < 2, delay>0) |> 
    ggplot(aes(delay, fill=kind)) +
    geom_histogram(position="identity", alpha=0.8)

fig()

# %% ===== events =============================================================
events <- read_csvs(versions, "events")
# info <- df %>% 
#     transmute( pid, trial_number, info = paste0(substr(bespoke, 1, 2), "-", substr(compositional, 1, 2)))
info <- df %>% 
    transmute( pid, trial_number, info = paste0(
        "B", if_else(bespoke=="available", "1", "0"), 
        "-", 
        "C", case_when(
            compositional=="none" ~ "0",
            compositional=="partial" ~ "1",
            compositional=="full" ~ "2",
            compositional=="exact" ~ "*"
        )
    ))

events %>% 
    left_join(info, by=c("trial_number", "pid")) %>% 
    filter(time < 120) %>% 
    ggplot(aes(time, info, color=dial, size=correct)) +
    geom_vline(xintercept = 0, color="black", linetype="solid") +
    geom_point() +
    scale_size_manual(values = c(`TRUE`=1.4, `FALSE`=0.5)) +
    scale_color_manual(values = c(
        left='#de6240',
        right='#e69d3e',
        bespoke='#1faac9'
    )) +
    facet_wrap(~pid, ncol=4)
fig("events", w=20, h=13)

# %% --------

events %>% 
    left_join(info, by=c("trial_number", "pid")) %>% 
    filter(pid == 9, info=="B1-C0")

read_csvs(versions, "participants") %>% select(pid, condition)

# %% --------

events %>% 
    left_join(select(df, pid, trial_number, bespoke, compositional), by=c("trial_number", "pid")) %>% 
    filter(time < 120) %>% 
    ggplot(aes(time, pid, color=dial, size=correct)) +
    geom_label(aes(label=info, x=-7, shape=NULL, size=NULL, color=NULL), data=info, size=3, color="black") +
    geom_vline(xintercept = 0, color="black", linetype="solid") +
    # scale_shape_manual(values = c(button='square', dial='circle', solution='diamond')) +
    geom_point() +
    scale_size_manual(values = c(`TRUE`=1.4, `FALSE`=0.5)) +
    scale_color_manual(values = c(
        left='#de6240',
        right='#e69d3e',
        bespoke='#1faac9'
    )) +
    facet_grid(bespoke~compositional)

fig("events", w=20, h=13)



# %% --------

events %>% 
    left_join(select(df, pid, trial_number, solution_type, compositional), by=c("trial_number", "pid")) %>% 
    filter(solution_type == "compositional", compositional=="none") %>% 
    group_by(pid, trial_number, dial) %>% 
    count(pos)

# %% --------

events %>% 
    filter(pid == 3, trial_number ==2) %>% 
    select(time, dial, pos, val) %>% 
    print(n=100)

# %% ===== feedback ===========================================================

feedback <- read_csvs(versions, "feedback")
feedback %>% 
    arrange(preference) %>% 
    select(pid, preference, preference_reason) %>% 
    print(n=100)

# %% --------

quiz <- read_csvs(versions, "quiz")

quiz %>% 
    group_by(question) %>% 
    summarise(p_correct = mean(correct))

quiz %>% 
    count(question, correct, answer)


# %% --------


# %% ===== old stuff ==========================================================

# %% --------

df %>% 
    ggplot(aes(trial_number, is_compositional)) +
    points() +
    gam_fit()

fig()

df %>% filter(trial_number > 2) %>% with(mean(is_compositional))

# %% --------

df %>% 
    filter(trial_type == "B0-C0") %>% 
    group_by(pid) %>% 
    mutate(order = row_number()) %>% 
    select(order, solution_type)  %>% 
    pivot_wider(names_from = order, values_from = solution_type) %>% 
    identity

# %% --------

df %>% 
    ggplot(aes(trial_number, pid, color=solution_type)) +
    geom_point() + cpal + no_legend +
    no_yaxis_ticks +
    no_gridlines +
    grid_y + ybreaks(30)

fig(w=2.5, h=5)

# %% --------
