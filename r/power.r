# %% ===== power analysis utilities ===========================================

source("base.r")
library(coin)


FIGS_PATH <- "figs/predictions/"
version <- "v23"

sample_stat = function(groups, N, statistic) {
    data = sample(groups, N, replace=T) %>% 
        # map(~ mutate(.x, wid=round(1e10 * runif(1)))) %>%
        bind_rows
    tryCatch(statistic(data), error=function(c) NaN)
}

power_analysis = function(groups, N, n_sim, statistic) {
    results = map(N, ~ replicate(n_sim, sample_stat(groups, .x, statistic))) %>% unlist
    expand.grid(
        sim_i = 1:n_sim,
        N = N
    ) %>% 
    rowwise() %>% 
    mutate(
        stat = sample_stat(groups, N, statistic)
    ) %>% ungroup()
}

make_groups <- function(data, key) {
    data %>% 
        nest_by({{key}}, .keep=TRUE) %>% 
        with(data)
}

calculate_power <- function(results, alpha=.05) {
    results %>% 
        group_by(N) %>% 
        summarise(
            power = mean(stat < alpha),
            n_sim = n(),
            power_lower = power - 1.96 * sqrt(power * (1-power) / n_sim),
            power_upper = power + 1.96 * sqrt(power * (1-power) / n_sim)
        )
}

# %% --------
version <- "v23"

model_predictions <- read_csv(glue("tmp/predictions-epsilon-{version}.csv")) %>% filter(N==50) %>% filter(gen < 11)
human_predictions <- read_csv(glue("tmp/predictions-{version}.csv")) %>% filter(N==50) %>% filter(gen < 11)

model_predictions |> 
    ggplot(aes(gen, compositionality)) +
    geom_line(mapping=aes(group=pop), linewidth=.2, color=RED, alpha=.1) +
    # stat_mean_and_quantiles(color=RED) +
    geom_line(data=filter(human_predictions, pop <= 300 / N), mapping=aes(group=pop), linewidth=1, color=BLACK) +
    facet_grid(N~D) + ylim(0, 1)

fig("possible_result", w=7, h=3)

# %% --------

data <- human_predictions %>% filter(gen == 10, pop < 7) %>% filter(D != 2) %>% mutate(D8 = factor(D == 8))
test <- wilcox_test(compositionality ~ D8, data=data, distribution = "exact", alternative="less")
pvalue(test)

# %% --------

get_power_by_gen <- function(data, n_gen, comparison) {
    groups <- human_predictions %>% 
        filter(D %in% c(comparison, 8)) %>% 
        filter(gen %in% c(8,9,10)) %>% 
        summarise( compositionality = mean(compositionality), .by=c(D, pop)) %>% 
        make_groups(pop)
    
    # N <- c(3,6,9)
    N <- c(6)
    n_sim <- 1000

    p1 <- power_analysis(groups, N, n_sim, . %>% 
        wilcox.test(compositionality ~ I(D == 8), data=., alternative="less", correct=F) %>% 
        with(p.value)
    )
    calculate_power(p1, alpha=0.01) %>% 
        rename(populations = N) %>% 
        mutate(comparison=comparison, n_gen=n_gen)
}

result <- expand.grid(
        D = c(2, 32),
        n_gen = 1:5
    ) %>% 
    rowwise() %>% 
    reframe(get_power_by_gen(human_predictions, n_gen, D))

result %>% 
    mutate(comparison = glue("8 vs. {comparison}")) %>% 
    ggplot(aes(n_gen, power, color=comparison)) +
        geom_point() +
        geom_line(aes(group=comparison)) +
        geom_errorbar(aes(ymin=power_lower, ymax=power_upper), width=0.2)

fig(w=5)


get_power <- function(data, prm_N, comparison) {
    groups <- human_predictions %>% 
        filter(N == prm_N) %>% 
        filter(D %in% c(comparison, 8)) %>% 
        filter(gen == 10) %>% 
        mutate(D8 = factor(D == 8)) %>% 
        select(D8, pop, compositionality) %>% 
        make_groups(pop)
    
    N <- c(6)
    n_sim <- 100

    power_analysis(groups, N, n_sim, . %>% 
        wilcox_test(compositionality ~ D8, data=., alternative="less", distribution = "exact") %>% 
        pvalue
    )
}

get_power(human_predictions, 50, 32) %>% count(stat)



# %% --------

result <- human_predictions %>% 
    distinct(N, D) %>% 
    filter(D != 8) %>% 
    rowwise() %>% 
    reframe(get_power(human_predictions, N, D))

result

result %>% 
    mutate(comparison = glue("8 vs. {comparison}")) %>% 
    fctrize(populations) %>% 
    ggplot(aes(populations, power, color=comparison)) +
        geom_point() +
        geom_line(aes(group=comparison)) +
        geom_errorbar(aes(ymin=power_lower, ymax=power_upper), width=0.2) +
        facet_wrap(~N)

fig(w=4)    


# %% --------

groups <- human_predictions %>% 
    filter(N == 50) %>% 
    filter(gen == 9) %>% 
    select(D, pop, compositionality) %>% 
    filter(D %in% c(2, 8)) %>% 
    make_groups(pop)

N <- seq(3, 9)
n_sim <- 100

p1 <- power_analysis(groups, N, n_sim, . %>% 
    wilcox.test(compositionality ~ I(D == 8), data=., alternative="greater", exact=T) %>% 
    with(p.value)
)


p1 %>% 
    calculate_power() %>% 
    ggplot(aes(N, power)) +
        geom_point() +
        geom_line()
        # geom_smooth(se=F)

fig()
# %% --------
