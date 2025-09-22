# %% --------
source("base.r")

version <- "cost-pilot-v2"
FIGS_PATH <- glue("figs/{version}/")

load_data <- function(name) {
    read_csvs(glue("../data/{version}/{name}.csv")) %>% select(-data_file)
}

df <- load_data("trials") %>% 
    mutate(is_main = !is_catch & !is_practice)

participants <- load_data("participants")

events <- load_data("events")

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
# %% --------

df %>% filter(trial_number == 0) %>% 
    left_join(participants %>% transmute(pid, delay=solutionDelay/1000)) %>% 
    group_by(delay) %>%
    summarise(mean(choose_compositional))

# %% --------

main_trials <- df %>% 
    # left_join(pass_rate %>% transmute(pid, excluded=n_fail>1)) %>% filter(!excluded) %>%
    filter(is_main) %>%
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
    ) %>% 
    left_join(participants %>% transmute(pid, delay=solutionDelay/1000))

excluded <- main_trials %>% 
    mutate(clear_error = 
        compositional != "exact" & bespoke == "exact" & choose_compositional
    ) %>%
    group_by(pid) %>%
    summarise(p_left = mean(choose_left), n_error = sum(clear_error)) %>%
    mutate(excluded = n_error > 0) %>% 
    arrange(n_error, p_left)

# main_trials <- main_trials %>% left_join(excluded) %>% filter(!excluded)

# %% --------

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
    facet_grid(delay~bespoke) +
    ylim(0, 1) +
    labs(x="Compositional in Manual?", y="Compositional Rate")

fig("compositional-rate", w=2, h=1.5)

# %% --------

main_trials %>% 
    mutate(effort_difference = if_else(choose_compositional == choose_left, effort_difference, -effort_difference)) %>% 
    agg(choose_compositional, c(trial_type, effort_difference))  %>% 
    ggplot(aes(effort_difference, 1 * choose_compositional)) +
    geom_point() + expand_limits(y=c(0, 1))

fig()

# %% ===== completion time =================================================

main_trials |> 
    group_by(bespoke, compositional, solution_type) %>% 
    filter(duration < quantile(duration, .95)) |> 
    ungroup() %>% 
    ggplot(aes(compositional, duration/1000, color=solution_type)) +
    # geom_quasirandom(size=.2) +
    points() +
    cpal +
    expand_limits(y=c(0, 1)) +
    facet_grid(delay~bespoke)

fig("duration-by-solution-type", w=2, h=1.5)

# %% --------

durations <- main_trials |> 
    group_by(delay, bespoke, compositional, solution_type) %>% 
    filter(duration < quantile(duration, .95)) %>% 
    summarise(duration = mean(duration)/1000) %>% 
    pivot_wider(names_from=solution_type, values_from=duration, names_prefix="duration_") %>% 
    ungroup()


zilch_durations <- bind_rows(
    durations %>% 
        filter(bespoke == "zilch") %>%
        pivot_wider(names_from=compositional, values_from=duration_compositional, id_cols=c(delay)) %>% 
        mutate(kind = "compositional")
    ,
    durations %>% 
        filter(compositional == "zilch") %>%
        pivot_wider(names_from=bespoke, values_from=duration_bespoke, id_cols=c(delay)) %>% 
        mutate(kind = "bespoke")
) %>% group_by(kind, delay)


zilch_durations %>% transmute(search_cost = zilch - exact)

zilch_durations %>% filter(kind == "bespoke") %>% transmute(target_action_cost = 0.4 * (zilch - exact))

durations %>% 
    mutate(compositional = factor(compositional, levels = c("zilch", "exact"))) %>%
    filter(compositional == bespoke) %>%
    mutate(action_cost = duration_compositional - duration_bespoke)
    
# %% --------

durations <- main_trials |> 
    group_by(pid, delay, bespoke, compositional, solution_type) %>% 
    filter(duration < 120 * 1000) %>% 
    summarise(duration = mean(duration)/1000) %>% 
    pivot_wider(names_from=solution_type, values_from=duration, names_prefix="duration_") %>% 
    ungroup()


zilch_durations <- bind_rows(
    durations %>% 
        filter(bespoke == "zilch") %>%
        pivot_wider(names_from=compositional, values_from=duration_compositional, id_cols=c(delay, pid)) %>% 
        mutate(kind = "compositional")
    ,
    durations %>% 
        filter(compositional == "zilch") %>%
        pivot_wider(names_from=bespoke, values_from=duration_bespoke, id_cols=c(delay, pid)) %>% 
        mutate(kind = "bespoke")
) %>% group_by(pid, kind, delay)

# %% --------

zilch_durations %>% 
    transmute(search_cost = zilch - exact) %>% filter(kind == "bespoke") %>% 
    ggplot(aes(delay, search_cost)) +
    geom_quasirandom() +
    points()
    
fig()




# %% --------


main_trials %>% 
    group_by(delay) %>% 
    summarise(mean(duration))
    
# %% --------

x1 <- main_trials %>% 
    filter(compositional == "exact", bespoke == "zilch", solution_type == "compositional") %>%
    group_by(uid) %>%
    summarise(exact = mean(duration)/1000)

x2 <- main_trials %>% 
    filter(compositional == "zilch", bespoke == "zilch", solution_type == "bespoke") %>%
    group_by(uid) %>%
    summarise(bespoke = mean(duration)/1000)
    
inner_join(x1, x2, by="uid") %>% 
    ggplot(aes(exact, bespoke)) +
    geom_abline(color=RED, slope=1, intercept=0) +
    geom_point(size=.3)

fig()

# %% --------
touches <- events %>% 
    group_by(pid, trial_number, dial) %>% 
    summarise(first_touch = min(time), last_touch = max(time), correct=max(correct)) %>% 
    mutate(dial_kind = if_else(dial == "bespoke", "bespoke", "compositional"))

comp_touches <- touches %>% 
    filter(dial_kind == "compositional") %>% 
    pivot_wider(names_from=dial, values_from=c(first_touch, last_touch)) %>% 
    filter(first_touch_right > last_touch_left) %>% 
    mutate(
        left_rt = first_touch_left, 
        left_search = last_touch_left - first_touch_left,
        right_rt = first_touch_right - last_touch_left,
        right_search = last_touch_right - first_touch_right
    )

comp_times <- main_trials %>% 
    left_join(comp_touches) %>% 
    filter(compositional == "zilch", dial_kind == "compositional") %>% 
    filter(choose_compositional) %>% 
    group_by(pid, delay) %>%
    summarise(
        left_rt = mean(left_rt),
        left_search = mean(left_search),
        right_rt = mean(right_rt - delay),
        right_search = mean(right_search)
    )

# %% --------

events %>% 
    filter(trial_number > 0) %>%
    group_by(pid, trial_number, dial) %>%
    mutate(dt = time - lag(time)) %>% 
    group_by(pid) %>%
    filter(dt < 5) %>% 
    ggplot(aes(pid, dt)) +
    points() +
    ylab("Seconds between guesses")
    # geom_quasirandom(size=.1)
    
fig(w=3)
    

# %% --------

# NOTE: not very informative because ignores luck of how many guesses it takes

figure("comp_times", w=2, h=1, wrap_plots(ncol=2, guides="collect",
    comp_times %>% 
        ggplot(aes(left_rt, left_search, color=factor(delay))) +
        geom_point() +
        coord_cartesian(xlim=c(0, 20), ylim=c(0, 45)) +
        gridlines +
        theme()
    ,
    comp_times %>% 
        ggplot(aes(right_rt, right_search, color=factor(delay))) +
        geom_point() +
        coord_cartesian(xlim=c(0, 20), ylim=c(0, 45)) +
        gridlines +
        theme()
))


# %% --------

bespoke_times <- main_trials %>% 
    left_join(touches) %>% 
    filter(bespoke == "zilch", dial_kind == "bespoke") %>% 
    filter(!choose_compositional) %>%
    transmute(delay, rt = first_touch, search_time = last_touch - first_touch)

figure("bespoke_times", bespoke_times %>% 
    ggplot(aes(rt, search_time)) +
    # geom_quasirandom() +
    # points() +
    geom_point(size=.5) +
    facet_wrap(~delay) +
    theme()
)    
    

# %% --------
main_trials %>% 
    filter(choose_compositional) %>%
    select(uid, trial_number, compositional, bespoke) %>% 
    left_join(events) %>% 
    filter(pid == 2) %>% 
    select(trial_number, compositional, bespoke, dial, pos, correct, time) %>% 
    print(n=100)

# %% --------
   
