@everywhere include("red_black.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim

R"""
FIGS_PATH = " ~/obsidian/Web/cultural-abstractions/figs/abstract/2024-07-12-"
MAKE_PDF = FALSE
"""

# %% ==================== learning ====================

df = dataframe(grid(S=[5, 10, 20])) do (;S)
    map([1:10S; 10S:S:3_000]) do D
        idiosyncratic = prob_observe(1 / (S^2), D)
        r = prob_observe(1 / S, D)
        compositional = r ^ 2
        # partial = r * ¬r
        (;D, idiosyncratic, compositional)
    end
end
@rput df

R"""
df %>%
    # filter(D > S) %>%
    pivot_longer(c(compositional, idiosyncratic), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(D, value, color=name)) +
    scale_colour_manual(values=c(compositional=RED, partial=lighten(RED, .5), idiosyncratic=TEAL ), aesthetics=c("fill", "colour"), name="") +
    # no_legend +
    geom_line() +
    # labs(x="S (number of starts/goals)", y="p(observe my solution)") +
    scale_x_log10(labels=scales::comma) +
    facet_wrap(~S, nrow=1) +
    gridlines +
    labs(x="Number of demonstrations", y="observation probability")
    theme()


fig("p_observe", w=8)
"""

# %% --------
df = dataframe(grid(S=2:100)) do (;S)
    idiosyncratic = optimize(0, 100 * S^2) do D
        idiosyncratic = prob_observe(1 / (S^2), D)
        abs(idiosyncratic - 0.5)
    end
    compositional = optimize(0, 100 * S) do D
        r = prob_observe(1 / S, D)
        compositional = r ^ 2
        abs(compositional - 0.5)
    end
    map(Optim.minimizer, (;idiosyncratic, compositional))
end
@rput df

R"""
df %>%
    filter(S < 21) %>%
    pivot_longer(c(idiosyncratic, compositional), names_to="name", values_to="value") %>%
    ggplot(aes(S, value, color=name)) +
    cpal + no_legend +
    geom_line() +
    # scale_x_log10() +
    # scale_y_luog10() +
    ylab("Demonstrations for\n 50% Observation Prob")
fig("necessary")
"""



# %% ==================== bottleneck breaks ====================


df = dataframe(grid(S=[100], x0=0:.01:1)) do (;S, x0)
   map([250, 10S, S^2, 5 * S^2]) do D
       idiosyncratic = prob_observe((1-x0) / (S^2), D)
       partial = prob_observe(x0 / S, D)
       compositional = partial ^ 2
       (;D, idiosyncratic, partial, compositional)
   end
end
@rput df

R"""
df %>%
   pivot_longer(c(compositional, partial, idiosyncratic), names_to="name", values_to="value", names_prefix="") %>%
   mutate(`D/S` = factor(D/S, labels=c(2.5, 10, "S", "5S"))) %>%
   filter(name != "partial") %>%
   ggplot(aes(x0, value, color=name)) +
   scale_colour_manual(values=c(compositional=RED, partial=lighten(RED, .5), idiosyncratic=TEAL ), aesthetics=c("fill", "colour"), name="") +
   # no_legend +
   geom_abline(linetype="dashed", color=RED, alpha=0.5) +
   geom_abline(linetype="dashed", color=TEAL, alpha=0.5, slope=-1, intercept=1) +
   geom_line(linewidth=.8) +
   no_gridlines +
   facet_grid(~`D/S`) +
   xbreaks(3) + ybreaks(3) +
   labs(x="Compositionality Rate", y="Observation probability") +

   # labs(x="Bottleneck Size (D/S)", y="observation probability")
   theme()

fig("bottleneck_breaks", w=8, h=2.2)
"""

# %% ==================== curve ====================

curve = dataframe(grid(p0=0:.001:1, S=20, M=70, p_r=0)) do (;p0, S, M, p_r)
    env = RedBlackEnv(;S, M, p_r)
    (;p0, p1=transition(env, p0))
end |> DataFrame
@rput curve

R"""
D = curve %>%
    mutate(
        delta=p1 - p0,
        segment = pmax(1, cumsum(replace_na(sign(delta) - lag(sign(delta)) != 0, FALSE)))
    ) %>%
    group_by(segment) %>%
    mutate(direction = sign(last(delta)))

ggplot(D) +
    geom_abline(linetype="solid", color=GRAY) +
    annotate("rect", xmin = 0, xmax = stable$start, ymin = -Inf, ymax = Inf, alpha = .3, fill=TEAL) +
    annotate("rect", xmin = stable$start, xmax = 1, ymin = -Inf, ymax = Inf, alpha = .1, fill=RED) +
    lapply(split(D, D$segment), function(df)
      geom_line(data = df, aes(p0, p1),
                size=.8,
                arrow = arrow(
                     length = unit(.1, "in"), type = 'open',
                     ends = if (first(df$direction) < 0) 'first' else 'last',
                ))
    ) +
    # gridlines +
    coord_fixed(expand=T) +
    # labs(x="previous compositionality rate", y="new compositionality rate") +
    labs(x="previous compositionality", y="new compositionality") +
    cpal + no_legend

fig("rate_rate", w=2.9)
"""

R"""
D = curve %>%
    mutate(
        delta=p1 - p0,
        segment = pmax(1, cumsum(replace_na(sign(delta) - lag(sign(delta)) != 0, FALSE)))
    ) %>%
    group_by(segment) %>%
    mutate(direction = sign(last(delta)))

ggplot(D) +
    geom_hline(yintercept=0, linetype="solid", color=GRAY) +
    geom_line(mapping=aes(p0, delta), linewidth=.5) +
    geom_point(mapping=aes(stable$start, 0)) +
    annotate("rect", xmin = 0, xmax = stable$start, ymin = -Inf, ymax = Inf, alpha = .3, fill=TEAL) +
    annotate("rect", xmin = stable$start, xmax = stable$stop, ymin = -Inf, ymax = Inf, alpha = .1, fill=RED) +
    lapply(split(D, D$segment), function(df)
      geom_line(data = df, aes(p0, 0, color = factor(direction)),
                size=.8,
                arrow = arrow(
                     length = unit(.1, "in"), type = 'open',
                     ends = if (first(df$direction) < 0) 'first' else 'last',
                ))
    ) +
    # gridlines +
    # coord_fixed(expand=T) +
    # labs(x="previous compositionality rate", y="new compositionality rate") +
    labs(x="compositionality", y="change in compositionality") +
    cpal + no_legend

fig("rate_rate", w=2.9)
"""

# %% ==================== MpS stable ====================


df = dataframe(grid(S=[5,10,20,40,80], p_r = [0., .5, 1.], p_brr = [0., .5, 1.])) do (;S, p_r, p_brr)
    map([1:10S; 10S:S:500S]) do M
        (;M, find_stable_points(;S, M, p_r, p_brr)...)
    end
end
df
@rput df

R"""
plot_start_stop = function(data) {
    data %>%
    filter(M/S > .3) %>%
    pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
    ggplot(aes(M / S, value, color=factor(S), group=interaction(name, S))) +
    geom_line(linewidth=.8) +
    scale_x_log10() +
    teals_pal() +
    labs(y="Compositionality", x="Demonstration Ratio (D/S)")
}

df %>%
    mutate(p_r = 100 * p_r, p_brr = 100 * p_brr) %>%
    plot_start_stop +
    gridlines +
    facet_grid(p_brr~p_r, labeller=label_glue(cols="{p_r}% partial", rows="{p_brr}% both"))

fig("start_stop_grid", w=6, h=4)
"""

# %% --------

SS = 100
d100 = dataframe(grid(S=SS, p_r = 0., p_brr = 0.)) do (;S, p_r, p_brr)
    map([1:10S; 10S:S:1000S]) do M
        (;M, find_stable_points(;S, M, p_r, p_brr)...)
    end
end
d100
@rput d100 SS

R"""

plot_zones = function(SS) {
    zone_data = tibble(
        xmin=c(0, 2.5, 10, SS, 5*SS),
     ) %>% mutate(xmax = lead(xmin, default=Inf), zone=factor(row_number()))
    text_data = tibble(
        x=c(1.6, 25, 5*SS),
        name=c("too narrow", "just right", "too wide"),
     ) %>% mutate(zone=factor(row_number()))
    list(
        geom_rect(
            data=zone_data,
            mapping=aes(xmin=xmin, xmax=xmax, fill=zone, x=NULL, y=NULL, color=NULL, group=NULL),
            ymin=-Inf, ymax=Inf, alpha=.3,
        ),
        geom_text(data=text_data, mapping=aes(x=x, label=name, color=zone), y=1.12, hjust=0.5, nudge_x=.1),
        coord_cartesian(expand=T, xlim=c(1, 1000), ylim=c(0, 1), clip='off'),
        scale_fill_manual(values=c(TEAL, "#94DCE6", RED, "#94DCE6", TEAL)),
        scale_color_manual(values=c(TEAL, RED, TEAL)),
        theme(plot.margin=margin(t=15, l=5, r=5, b=5)),
        no_gridlines,
        # scale_fill_manual(values=c(TEAL, GRAY, RED, GRAY, TEAL)),
        no_legend
    )
}

d100 %>%
    filter(M > S) %>%
    pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
    ggplot(aes(M/S, value, group=name)) +
    plot_zones(SS) +
    geom_line(linewidth=.8) +
    scale_x_log10() +
    labs(y="Compositionality", x="Bottleneck Size (D/S)") +
    theme()

fig("start_stop")
"""

# %% --------

R"""
SS = 100
d100 %>%
    filter(M > S) %>%
    filter(p_r == 0, p_brr == 0, S==SS) %>%
    pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
    ggplot(aes(M, value, group=name)) +
    annotate("rect", xmin = SS*0, xmax = SS*2.5, ymin = -Inf, ymax = Inf, alpha = .3, fill=TEAL) +
    annotate("rect", xmin = SS*2.5, xmax = SS*10, ymin = -Inf, ymax = Inf, alpha = .1, fill=RED) +
    annotate("rect", xmin = SS*10, xmax = SS*SS, ymin = -Inf, ymax = Inf, alpha = .3, fill=RED) +
    annotate("rect", xmin = SS*SS, xmax = SS*Inf, ymin = -Inf, ymax = Inf, alpha = .2, fill=TEAL) +
    geom_line(linewidth=.8) +
    scale_x_log10(breaks=c(250, 1000, 10000)) +
    theme()

fig("start_stop")
"""


# %% --------



# %% --------


R"""
D = df %>%
    filter(M < 70) %>%
    pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
    drop_na()

D2 = df %>%
    filter(M < 70) %>%
    replace_na(list(stop = 0, start=0, zero=0))

ggplot(D, aes(M)) +
    geom_ribbon(data=D2, mapping=aes(ymin=stop, ymax=1), fill=TEAL, alpha=0.3) +
    geom_ribbon(data=D2, mapping=aes(ymin=start, ymax=stop), fill=RED, alpha=0.3) +
    geom_ribbon(data=D2, mapping=aes(ymin=0, ymax=start), fill=TEAL, alpha=0.3) +
    # geom_line(mapping=aes(group=name, y=value), linewidth=.5, color=GRAY) +
    geom_line(
        data=filter(D, mod(M, 5) == 0),
        mapping=aes(group=M, y=value),
        arrow = arrow(ends='last', length = unit(.1, "in")),
        color=RED
    ) +
    facet_wrap(~S)
    facet_wrap(~S)

fig(h=4, w=7)
"""

# %% ==================== MS stable ====================


df = dataframe(grid(S=3:50, M=1:50, p_r = 0:.2:1)) do (;S, M, p_r)
# df = dataframe(grid(S=[10, 20], M=[40, 80], p_r = 0:.01:1)) do (;S, M, p_r)
    find_stable_points(;S, M, p_r)
end

@rput df

# %% --------

R"""
df %>%
    replace_na(list(start=1)) %>%
    ggplot(aes(M, S, fill=start)) +
    rasterise(geom_tile(), dpi=DPI) +
    coord_fixed(expand=F) +
    no_gridlines +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=2, mid=0.5, rev=T) +
    facet_wrap(~p_r)

fig(w=8, h=4)
"""

R"""
df %>%
    replace_na(list(stop=0)) %>%
    ggplot(aes(M, S, fill=stop)) +
    rasterise(geom_tile(), dpi=DPI) +
    coord_fixed(expand=F) +
    no_gridlines +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=2, mid=0.5) +
    facet_wrap(~p_r)

fig(w=8, h=4)
"""

# %% --------

df = dataframe(grid(p0=0:.01:1, S=10, M=1:100)) do (;p0, S, M)
    env = RedBlackEnv(;S, M, p_r=0.)
    (;p0, p1=transition(env, p0))
end |> DataFrame
@rput df

R"""
df %>%
    mutate(
        delta = p1 - p0,
        # logdelta = if_else(delta == 0, 0, sign(delta) * log(abs(delta)))
        logdelta = sign(delta) * abs(delta) ^ (1/3)
    ) %>%
    # ggplot(aes(M, p0, fill=2*(delta > 0) - 1)) +
    ggplot(aes(p0, M, fill=logdelta)) +
    geom_tile() +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=1.5) + no_legend

fig()
"""



# %% ==================== experiment ====================

(;sim, tdf) = deserialize("tmp/experiment")

@rput sim tdf


R"""
human = tdf %>%
    filter(start != 5, goal != 5) %>%
    group_by(M, population, generation) %>%
    summarise(compositionality = mean(path_length == 2)) %>%
    mutate(agent = "human")

# df = bind_rows(mutate(sim, agent="model", population=100+population), human)

sim %>%
    # filter(generation < 9) %>%
    ggplot(aes(generation, 1*compositionality, group=population)) +
    geom_line(linewidth=.5, color=RED, alpha=.3) +
    # geom_line(linewidth=.5, color="#BA1109", alpha=0.5) +
    geom_line(data=human) +
    facet_wrap(~M, labeller=label_glue("{M} Demos"), scales="free_y") +
    xbreaks() + ybreaks() +
    expand_limits(y=1.)


fig("experiment", w=7)
"""
