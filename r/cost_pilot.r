# %% --------
source("base.r")

version <- "cost-pilot-v1"
FIGS_PATH <- glue("figs/{version}/")

load_data <- function(name) {
    read_csvs(glue("../data/{version}/{name}.csv"))
}

df <- load_data("trials") %>% 
    mutate(is_main = !is_catch & !is_practice) %>% 
    select(-data_file)


print(glue("{length(unique(df$pid))} participants and {nrow(df)} trials"))

participants <- load_data("participants")

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
    facet_wrap(~bespoke)

fig("duration-by-solution-type", w=2)

# %% --------

durations <- main_trials |> 
    group_by(bespoke, compositional, solution_type) %>% 
    filter(duration < quantile(duration, .95)) %>% 
    summarise(duration = mean(duration)/1000) %>% 
    pivot_wider(names_from=solution_type, values_from=duration, names_prefix="duration_") %>% 
    ungroup()


zilch_durations <- bind_rows(
    durations %>% 
        filter(bespoke == "zilch") %>%
        pivot_wider(names_from=compositional, values_from=duration_compositional, id_cols=c()) %>% 
        mutate(kind = "compositional")
    ,
    durations %>% 
        filter(compositional == "zilch") %>%
        pivot_wider(names_from=bespoke, values_from=duration_bespoke, id_cols=c()) %>% 
        mutate(kind = "bespoke")
) %>% group_by(kind)


zilch_durations %>% transmute(search_cost = zilch - exact)

durations %>% 
    mutate(compositional = factor(compositional, levels = c("zilch", "exact"))) %>%
    filter(compositional == bespoke) %>%
    mutate(action_cost = duration_compositional - duration_bespoke)
    

bespoke_zilch <- zilch_durations %>% filter(kind == "bespoke") %>% with(zilch)
bespoke_exact <- zilch_durations %>% filter(kind == "bespoke") %>% with(exact)

range <- bespoke_zilch - bespoke_exact

range * 0.4

# %% --------

participants %>% 
    transmute(pid, generation = if_else(substr(config, 1, 1) == "0", 2, 7)) %>% 
    right_join(main_trials) %>% 
    group_by(generation) %>% 
    summarise(mean(duration))
    



    

# %% --------


# 18 # min (bespoke exact)
# 46 # baseline (bespoke zilch)
# 28 # bespoke search cost
# 7  # seconds per dial searching
# 57 # partial

# %% --------

x1 <- main_trials %>% 
    filter(compositional == "partial", bespoke == "zilch", solution_type == "compositional") %>%
    group_by(uid) %>%
    summarise(partial = mean(duration)/1000)

x2 <- main_trials %>% 
    filter(compositional == "zilch", bespoke == "zilch", solution_type == "bespoke") %>%
    group_by(uid) %>%
    summarise(bespoke = mean(duration)/1000)
    
inner_join(x1, x2, by="uid") %>% 
    ggplot(aes(partial, bespoke)) +
    geom_abline(color=RED, slope=1, intercept=0) +
    geom_point(size=.3)

fig()