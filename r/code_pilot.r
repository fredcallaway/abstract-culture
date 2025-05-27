# %% --------
source("base.r")

FIGS_PATH <- "figs/pilot-v23/"
version <- "pilot-v23"

df <- read_csvs("../data/code-pilot-v23/trials.csv") %>% 
    mutate(is_main = !is_catch & !is_practice) %>% 
    select(-data_file)


print(glue("{length(unique(df$pid))} participants and {nrow(df)} trials"))


# %% ===== exclusions =========================================================

pass_rate <- df %>% 
    filter(is_catch) %>% 
    summarise(
        pass_rate = mean(choose_best),
        n_fail = sum(!choose_best),
        .by=pid
    )

pass_rate %>% 
    ggplot(aes(factor(pass_rate))) +
    geom_bar()


main_trials <- df %>% 
    left_join(pass_rate %>% transmute(pid, excluded=n_fail>1)) %>% 
    filter(is_main, !excluded) %>% 
    # filter(pass_rate == 1) %>% 
    mutate(
        bespoke = factor(
            substr(trial_type, 4, 8),
            levels = c("zilch", "exact")
        ),
        compositional = factor(
            substr(trial_type, 16, 30), 
            levels = c("zilch", "partial", "full", "exact")),
        solution_type = if_else(choose_compositional, "compositional", "bespoke")
    )

n_final <- main_trials %>% with(length(unique(pid)))
n_initial <- df %>% with(length(unique(pid)))
n_drop <- n_initial - n_final

main_trials |> 
    write_csv(glue("tmp/main_trials-{version}.csv"))

print(glue("Dropped {n_drop}/{n_initial} ({round(n_drop / n_initial * 100)}%) participants; {n_final} participants remain"))

# %% ===== compositionality rate ==============================================

main_trials |>
    ggplot(aes(compositional, 1 * choose_compositional)) +
    bars(fill=C_COMP) +
    facet_grid(~bespoke) +
    ylim(0, 1) +
    labs(x="Compositional in Manual?", y="Compositional Rate")

fig("compositional-rate", w=2)

# %% --------

main_trials %>% 
    mutate(effort_difference = if_else(choose_compositional == choose_left, effort_difference, -effort_difference)) %>% 
    agg(choose_compositional, c(trial_type, effort_difference))  %>% 
    ggplot(aes(effort_difference, 1 * choose_compositional)) +
    geom_point() + expand_limits(y=c(0, 1))

fig()


# %% ===== catch trials =======================================================

df %>% 
    filter(is_catch) %>% 
    ggplot(aes(trial_type, 1*choose_best)) +
    bars() +
    coord_flip()
    
fig()

# %% --------
df %>% 
    filter(is_catch | (trial_number == 0)) %>% 
    mutate(trial_type = fct_reorder(trial_type, trial_number)) %>%    
    ggplot(aes(pid, trial_type, fill=choose_best)) +
    geom_tile() +
    scale_fill_manual(values = c(`TRUE`="forestgreen", `FALSE`="red"))

fig(w=7, h=2)

harder <- read_csvs(versions, "bespoke_efficiency")

pass_instructions <- df %>% 
    filter(is_practice) %>% 
    drop_na(effort_difference) %>% 
    transmute(pid, pass_practice = choose_best)

pass_instructions %>% 
    left_join(harder) %>% 
    group_by(choice) %>% 
    summarise(p_pass = mean(pass_practice), n=n())


# %% ===== feedback ===========================================================

feedback <- read_csvs(versions, "feedback")

feedback %>% 
    select(pid, description) %>% 
    print(n=100)

# %% --------

feedback %>% 
    select(pid, feedback) %>% 
    print(n=100)

# %% ===== timing =======================================================


main_trials |>
    group_by(solution_type, bespoke, compositional) %>% 
    filter(solution_type == "bespoke") %>% 
    summarise(duration = median(duration))


# %% --------


main_trials |> 
    group_by(bespoke, compositional, solution_type) %>% 
    filter(duration < quantile(duration, .95)) |> 
    ungroup() %>% 
    ggplot(aes(compositional, duration/1000, color=solution_type)) +
    # geom_quasirandom(size=.2) +
    points() +
    cpal +
    facet_wrap(~bespoke)

fig("solution-time", w=2)



# %% --------

times <- read_csvs(versions, "times")



times %>% summarise(across(-c(pid, version), median))

participants %>% with(start_time)


# %% --------

instruct_times <- read_csvs(versions, "instruct_times")

instruct_times %>% arrange(-time)

instruct_times %>% 
    agg(time, stage, median)


# %% --------

type_times <- df %>% 
    # filter(duration < 200) %>% 
    group_by(trial_type) %>% 
    drop_extreme(duration, q_lo=0.05, q_hi=0.95) %>% 
    summarise(duration = mean(duration))

# %% --------

main_times <- main_trials %>% 
    filter(duration < 200) %>% 
    group_by(bespoke, compositional) %>% 
    summarise(duration =mean(duration))

get_time <- function(bespoke, compositional) {
    main_times %>% 
        filter(bespoke == !!bespoke, compositional == !!compositional) %>% 
        pull(duration)
}

main_time <- get_time("exact", "zilch") * 3 +
    get_time("zilch", "zilch") * 3 +
    get_time("zilch", "partial") + get_time("zilch", "full") * 2

not_main + (main_time + catch_time) / 60


# %% --------
df %>% 
    mutate(solution_type = if_else(choose_compositional, "compositional", "bespoke")) %>% 
    ggplot(aes(trial_type, duration, color=solution_type)) +
    geom_point() +
    coord_flip() +
    cpal +
    theme(legend.position = "none")

fig()

# %% --------

df %>% 
    ggplot(aes(trial_number, duration)) +
    points() +
    geom_smooth(method="lm")

fig()


# %% --------


5 + 8

# (15 / 4) * 1.333 * 4/3

# %% --------

button_times <- read_csvs(versions, "button_times")

button_times |> 
    mutate(delay = time - lag(time)) %>% 
    filter(delay < 2, delay>0) |> 
    ggplot(aes(delay, fill=kind)) +
    geom_histogram(position="identity", alpha=0.8)

fig()

# %% ===== IDK ================================================================


besp_dials <- 4
comp_dials <- 3
avg_dial_tries <- 5

expected <- distinct(main_trials, bespoke, compositional, solution_type) %>% 
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

main_trials |> 
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


# %% ===== events =============================================================

events <- read_csvs(versions, "events") %>% 
    group_by(pid, trial_number) %>% 
    mutate(
        rt = time - lag(time, default=0),
        action_number = row_number()
    ) %>% 
    ungroup()

# %% --------

events %>% 
    group_by(pid, trial_number) %>% 
    mutate(rt = time - lag(time)) %>% 
    summarise(rt = median(rt, na.rm=TRUE)) %>% 
    left_join(select(participants, pid, workerid)) %>% 
    ggplot(aes(trial_number, rt, group=pid, color=workerid == "6647c6138ad0a72e618533d0")) +
    geom_line(linewidth=.1) +
    scale_color_manual(values = c(`TRUE`="red", `FALSE`="black")) + no_legend

fig()

# %% --------

events %>% 
    left_join(select(participants, pid, workerid)) %>% 
    filter(workerid == "6647c6138ad0a72e618533d0") %>% 
    select(dial, pos, rt, val, correct) %>% print(n=100)



# %% --------

events %>% 
    filter(trial_number > 0) %>% 
    filter(action_number != 1) %>% 
    drop_extreme(rt, q_hi=.95) %>% 
    with(mean(rt))

# %% --------

df %>% 
    left_join(participants) %>% 
    filter(workerid == "67abc507448bec46e7dfbffa") %>% 
    print(n=100)


# %% --------

times %>% filter(total < 15) %>% 
    left_join(participants) %>% 
    select(workerid, total, main)

# %% --------



events %>% 
    left_join(times) %>% 
    mutate(fast = total < 15) %>% 
    filter(rt < 4) %>% 
    ggplot(aes(rt, color=fast)) +
    geom_density()
    
fig()

# %% --------

events %>% 
    left_join(participants) %>% 
    left_join(times) %>% 
    filter(rt < 0.5) %>% 
    count(workerid, total)

# %% --------



times %>% 
    left_join(pass_rate) %>% 
    left_join(
        df %>% filter(!is_catch, !is_practice) %>% agg(choose_left)
    ) %>% 
    ggplot(aes(total, choose_left)) +
    geom_point()

fig()
    

# %% --------


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



# %% --------

feedback %>% 
    left_join(select(participants, pid, workerid)) %>% 
    left_join(pass_rate) %>% 
    select(pid, pass_rate, description)

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


# %% ===== mousetracking ======================================================

mt <- read_csvs(versions, "mousetracking")

mt %>% 
    filter(trial_number == 3) %>% 
    filter(pid == 8) %>% 
    mutate(time = time - min(time)) %>% 
    ggplot(aes(x, y, color=time)) +
    geom_point()

fig()

# %% --------
