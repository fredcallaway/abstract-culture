# %% --------
source("base.r")

FIGS_PATH <- "figs/codes/reg-v2/"
version <- "reg-v2"
participants <- read_csvs("../data/reg-v2-g*/participants.csv") %>% 
    select(-c(useragent, active_minutes))

df <- read_csvs("../data/reg-v2-g*/trials.csv") %>% 
    mutate(is_main = !is_catch & !is_practice) %>% 
    right_join(select(participants, uid, generation, excluded), by="uid") %>% 
    mutate(pid = glue("{generation}.{pid}"))

participants <- participants %>% 
    mutate(pid = glue("{generation}.{pid}"))

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

fig()

main_trials <- df %>% 
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

# %% --------

participants %>% 
    left_join(pass_rate) %>% 
    with(sum(failed_catch != (n_fail > 1)))  # why is this 1?

participants %>% 
    filter(!excluded) %>% 
    count(repetition) %>% 
    with(stopifnot(all(n == 500)))

participants %>% 
    group_by(generation) %>% 
    filter(!excluded) %>% 
    count(config) %>% 
    with(stopifnot(all(n == 1)))

participants %>% 
    count(workerid) %>% 
    with(stopifnot(all(n == 1)))

main_trials %>% 
    count(chain_id) %>% 
    with(stopifnot(all(n == 500) && length(chain_id) == 18))

main_trials %>% 
    count(trial_id, sort=T) %>% 
    with(stopifnot(all(n == 1)))

# %% ===== compositionality rate ==============================================

human <- main_trials %>% 
    group_by(generation, chain_id) %>% 
    summarise(compositionality = mean(choose_compositional)) %>%
    mutate(D=as.numeric(sub(".*-D", "", chain_id)))

model_predictions <- read_csv(glue("../results/predictions-epsilon-v23.csv")) %>% 
    filter(N==50, gen<11) %>% 
    rename(generation = gen)


figure("compositionality-curve", model_predictions |> 
    ggplot(aes(generation, compositionality)) +
    geom_line(mapping=aes(group=pop), linewidth=.2, color=C_COMP, alpha=.1) +
    # stat_mean_and_quantiles(color=RED) +
    geom_line(data=human, mapping=aes(group=chain_id), linewidth=1, color=BLACK) +
    facet_wrap(~D) + ylim(0, 1)
)

# %% ===== completion time =================================================


figure("solution-time", main_trials |> 
    filter(duration < quantile(duration, .95)) %>% 
    group_by(chain_id, generation) %>% 
    summarise(duration = mean(duration)) %>% 
    mutate(D=as.numeric(sub(".*-D", "", chain_id))) %>% 
    ggplot(aes(generation, duration/1000)) +
    point_line() +
    facet_wrap(~D)
)

# %% --------

figure("solution-time", main_trials |> 
    group_by(bespoke, compositional, solution_type) %>% 
    filter(duration < quantile(duration, .95)) |> 
    ungroup() %>% 
    ggplot(aes(compositional, duration/1000, color=solution_type)) +
    # geom_quasirandom(size=.2) + cpal +
    points() +
    cpal +
    facet_wrap(~bespoke)
)

