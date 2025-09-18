# %% --------
source("base.r")
library(coin)  # for wilcox_test

FIGS_PATH <- "figs/reg-v2/"
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
        solution_type = if_else(choose_compositional, "compositional", "bespoke"),
        D=as.numeric(sub(".*-D", "", chain_id))
    )

n_final <- main_trials %>% with(length(unique(pid)))
n_initial <- df %>% with(length(unique(pid)))
n_drop <- n_initial - n_final

main_trials %>% 
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
    count(chain_id, generation) %>% 
    with(stopifnot(all(n == 50)))

main_trials %>% 
    count(trial_id, sort=T) %>% 
    with(stopifnot(all(n == 1)))

# %% ===== compositionality rate ==============================================

human <- main_trials %>% 
    group_by(generation, D, chain_id) %>% 
    summarise(compositionality = mean(choose_compositional))


plot_compositionality <- function(pred_file="../results/predictions-epsilon-v23.csv") {
    model_predictions <- read_csv(pred_file) %>% 
        filter(N==50, gen<11) %>% 
        rename(generation = gen)

    model_predictions %>% 
        ggplot(aes(generation, compositionality)) +
        geom_line(mapping=aes(group=pop), linewidth=.2, color=C_COMP, alpha=.1) +
        # stat_mean_and_quantiles(color=RED) +
        geom_line(data=human, mapping=aes(group=chain_id), linewidth=1, color=BLACK) +
        facet_wrap(~D, labeller=label_glue("D = {D}")) + ylim(0, 1)
}

figure("compositionality-curve", plot_compositionality())


# %% ===== alternate predictions ==============================================

figure("compositionality-curve-partial0.5", plot_compositionality(glue("../results/predictions-epsilon-partial0.5-v23.csv")))
figure("compositionality-curve-partial0", plot_compositionality(glue("../results/predictions-epsilon-partial0-v23.csv")))
figure("compositionality-curve-empirical", plot_compositionality(glue("../results/predictions-empirical-v23.csv")))
figure("compositionality-curve-empirical-main", plot_compositionality(glue("../results/predictions-empirical-reg-v2.csv")))
figure("compositionality-curve-ez", plot_compositionality(glue("../results/predictions-ez.csv")))
figure("compositionality-curve-zp", plot_compositionality(glue("../results/predictions-zp.csv")))

# %% ===== wilcox test ========================================================

run_wilcox <- function(data, comparison) {
    subdata <- data %>% 
        filter(D %in% c(comparison, 8)) %>% 
        filter(generation == 10) %>% 
        arrange(D == 8) %>% 
        mutate(D8 = factor(D == 8, levels=c(TRUE, FALSE)))
    
    result <- wilcox_test(compositionality ~ D8, data=subdata, alternative="greater", distribution = "exact")
        # write_tex()
    m8 = subdata %>% filter(D == 8) %>% with(median(compositionality)) %>% fmt_percent
    mx = subdata %>% filter(D != 8) %>% with(median(compositionality)) %>% fmt_percent
    fmt("{m8} vs. {mx}, Z = {statistic(result):.2}, {pval(pvalue(result))}") %>% 
        write_tex("wilcox-test/{comparison}")
    # fmt("$Z = {statistic(result):.2}$, ${pval(pvalue(result))}$")
    # filter()
}

run_wilcox(human, 2)
run_wilcox(human, 32)

human %>% 
    filter(generation == 10) %>% 
    ggplot(aes(factor(D), compositionality)) +
    geom_point()

fig()

# %% --------

main_trials %>% 
    group_by(bespoke, compositional) %>% 
    summarise(p_compositional = mean(choose_compositional)) %>% 
    mutate(agent = "human") %>% 
    bind_rows(mutate(read_csv("../results/policy-epsilon-v23.csv"), agent="model")) %>% 
    ggplot(aes(compositional, p_compositional, color=agent)) +
    facet_wrap(~bespoke) +
    expand_limits(y=c(0,1)) +
    geom_point()

fig("policy-comparison", w=2)


# %% ===== completion time =================================================

figure("solution-time", main_trials %>% 
    filter(duration < quantile(duration, .95)) %>% 
    group_by(chain_id, generation) %>% 
    summarise(duration = mean(duration)) %>% 
    mutate(D=as.numeric(sub(".*-D", "", chain_id))) %>% 
    ggplot(aes(generation, duration/1000)) +
    point_line() +
    facet_wrap(~D)
)

main_trials %>% 
    filter(duration < quantile(duration, .95), generation > 7) %>% 
    group_by(D) %>% 
    summarise(duration = mean(duration))

# %% --------

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

main_trials |> 
    group_by(bespoke, compositional, solution_type) %>% 
    filter(duration < quantile(duration, .95)) %>% 
    summarise(duration = mean(duration)/1000) %>% 
    pivot_wider(names_from=solution_type, values_from=duration, names_prefix="duration_")

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

# %% --------



main_trials %>% 
    mutate(
        comp_solution = 1 * (solution_type == "compositional"),
        besp_solution = 1 * (solution_type == "bespoke"),
        bespokezilch = 1 * (solution_type == "bespoke" & bespoke == "zilch"),
        compositional = relevel(compositional, "exact")
    ) %>% 
    mutate(duration = duration / 1000) %>%
    filter(duration < 90) %>%
    regress(duration ~ comp_solution + comp_solution:compositional + besp_solution:bespokezilch, print_table=F)

# %% --------

main_trials %>% 
    filter(bespoke=="zilch", compositional=="partial", generation > 1) %>% 
    group_by(solution_type) %>% 
    summarise(duration = mean(duration)) %>% 
    rowwise() %>% group_walk(~ with(.x,
        write_tex("{duration/1000:.1}", "zilch-partial-duration/{solution_type}")
    ))

# %% --------

main_trials %>% 
    filter(generation > 1, D == 32) %>% 
    drop_extreme(duration, q_hi=.99) %>% 
    group_by(chain_id, generation) %>% 
    summarise(duration = mean(duration), compositionality = mean(choose_compositional)) %>% 
    regress(duration ~ compositionality)

# %% --------

main_trials %>% 
    filter(generation > 1, D == 32) %>% 
    drop_extreme(duration, q_hi=.99) %>% 
    group_by(generation) %>% 
    summarise(duration = mean(duration)) %>% 
    regress(duration ~ generation)

# %% --------

main_trials %>% 
    group_by(bespoke, compositional) %>% 
    summarise(p_compositional = mean(choose_compositional)) %>% 
    write_csv("../tmp/compositional-rates-reg-v2.csv")

# %% --------

events <- read_csvs("../data/reg-v2-g*/events.csv") %>% 
    select(-data_file) %>% 
    right_join(select(participants, uid, generation, excluded), by="uid") %>% 
    mutate(pid = glue("{generation}.{pid}"))

# %% --------

events %>% 
    filter(trial_number > 0) %>%
    transmute(dt = time - lag(time)) %>% 
    drop_na() %>% 
    filter(dt > 0, dt < 10) %>%
    ggplot(aes(dt)) +
    geom_histogram() +
    

fig()

# %% --------

events %>% 
    filter(trial_number > 0) %>%
    transmute(dt = time - lag(time)) %>% 
    drop_na() %>% 
    filter(dt > 0, dt < 5) %>% 
    with(mean(dt))
