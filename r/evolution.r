# %% --------
source("base.r")


# version <- "code-pilot-v23"
versions <- c(
    "reg-v2-g1",
    "reg-v2-g2C",
    "reg-v2-g3",
    "reg-v2-g4",
    "reg-v2-g5",
    "reg-v2-g6",
    "reg-v2-g7",
    "reg-v2-g8",
    "reg-v2-g9",
    "reg-v2-g10"
)

FIGS_PATH <- glue("figs/codes/reg-v2/")

participants <- read_csvs(versions, "participants") %>% 
    select(-c(useragent, wid, active_minutes))


df <- read_csvs(versions, "trials") %>% 
    mutate(is_main = !is_catch & !is_practice) %>% 
    left_join(select(participants, version, generation, pid, workerid, excluded)) %>% 
    mutate(pid = glue("{generation}.{pid}"))

participants <- participants %>% 
    mutate(pid = glue("{generation}.{pid}"))

DPI <- 300
RED <- "#E86623"
TEAL <- "#07A9C0"

cpal <- scale_colour_manual(values = c(
    bespoke = TEAL,
    compositional = RED,
    `-1` = TEAL,
    `1` = RED
), aesthetics = c("fill", "colour"), name = "")

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

model_predictions <- read_csv(glue("tmp/predictions-v23.csv")) %>% 
    filter(N==50, gen<11) %>% 
    rename(generation = gen)

model_predictions |> 
    ggplot(aes(generation, compositionality)) +
    geom_line(mapping=aes(group=pop), linewidth=.2, color=RED, alpha=.1) +
    # stat_mean_and_quantiles(color=RED) +
    geom_line(data=human, mapping=aes(group=chain_id), linewidth=1, color=BLACK) +
    facet_wrap(~D) + ylim(0, 1)

fig("compositionality-curve", w=5)
