
# %% ===== cheaters ===========================================================

n_guess <- read_csvs(versions, "n_guess")

reg <- n_guess %>% 
    fctrize(pid) %>% 
    mutate(diff = n - 5) %>% 
    regress(diff ~ pid + 0, print_table=F) %>% 
    tidy %>% 
    mutate(pid = as.numeric(sub("^pid", "", term)))


n_guess %>% 
    agg(n, pid) %>% 
    left_join(reg, by="pid") %>% 
    left_join(times) %>% 
    left_join(select(participants, pid, workerid)) %>% 
    ggplot(aes(n, p.value, color=workerid == "67de239c06b421f44e118eaf")) +
    geom_point() +
    scale_y_log10() +
    expand_limits(x=c(1,9)) +
    xlab("Average Number of Guesses") +
    geom_vline(xintercept = 5, linetype="dashed") +
    no_legend + 
    scale_color_manual(values = c(`TRUE`="red", `FALSE`="black"))
    
fig()

cheaters <- reg %>% 
    filter(p.value < .001) %>% 
    select(pid)

# %% --------

n_guess %>% 
    right_join(cheaters) %>% 
    ggplot(aes(n)) +
    geom_bar()

fig()


# %% --------

selects <- read_csvs(versions, "selects")

selects
# %% --------


df %>% 
    filter(is_catch | (trial_number == 0)) %>% 
    mutate(mach_chosen = if_else(choose_left, mach1, mach2)) %>% 
    filter(endsWith(mach_chosen, "zilch")) %>% 
    mutate(n_dial = as.numeric(substr(mach_chosen, 2, 2))) %>% 
    mutate(suspicious = n_click < 11) %>% 
    group_by(pid) %>% 
    summarise(suspicious = mean(suspicious), n= n())

sus %>% filter(suspicious > 0)
# %% --------

read_csvs(versions, "selects") %>% 
    left_join(transmute(sus, pid, sus=suspicious > 0)) %>% 
    agg(correct, c(pid, knowledge, is_main, sus)) %>% 
    ggplot(aes(knowledge, correct, group=pid, color=sus, alpha=sus)) +
    geom_line(linewidth=0.5) +
    facet_wrap(~is_main)

fig(w=5)