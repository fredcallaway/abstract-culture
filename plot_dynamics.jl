@everywhere include("red_black.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim

# %% --------

R"""
FIGS_PATH = "~/papers/cultural-abstractions/figs/"
MAKE_PDF = TRUE
"""

# %% ==================== one-step ====================

$1 - (1 - 1/S)^D$
$1 - \left(1-\frac{x_t}{S}\right)^D$

S = 10; D = 20; x = .2
prob_observe(x / S, D)


# %% ==================== fixed points ====================

find_stable_points(;S=10, D=100)

curve = dataframe(grid(p0=0:.001:1, S=20, D=60)) do (;p0, S, D)
    env = RedBlackEnv(;S, D, p_r=0)
    (;p0, p1=transition(env, p0))
end |> DataFrame
@rput curve

stable = find_stable_points(;S=20, D=60)
@rput stable

R"""
D = curve %>%
    mutate(
        delta=p1 - p0,
        segment = pmax(1, cumsum(replace_na(sign(delta) - lag(sign(delta)) != 0, FALSE)))
    ) %>%
    group_by(segment) %>%
    mutate(direction = sign(last(delta)))



plot_prev_new = ggplot(D) +
    geom_abline(linetype="solid", color=GRAY) +
    annotate("rect", xmin = 0, xmax = stable$start, ymin = -Inf, ymax = Inf, alpha = .3, fill=TEAL) +
    annotate("rect", xmin = stable$start, xmax = 1, ymin = -Inf, ymax = Inf, alpha = .25, fill=RED) +
    lapply(split(D, D$segment), function(df)
      geom_line(data = df, aes(p0, p1),
                size=.8,
                arrow = arrow(
                     length = unit(.1, "in"), type = 'open',
                     ends = if (first(df$direction) < 0) 'first' else 'last',
                ))
    ) +
    geom_point(x=stable$start, y=stable$start) +
    # gridlines +
    coord_fixed(expand=T) +
    # labs(x="previous compositionality rate", y="new compositionality rate") +
    labs(x="previous compositionality", y="new compositionality") +
    cpal + no_legend

fig("prev_new", w=2.9)
"""


R"""
D = curve %>%
    mutate(
        delta=p1 - p0,
        segment = pmax(1, cumsum(replace_na(sign(delta) - lag(sign(delta)) != 0, FALSE))),
        attractor = factor(segment, labels=c("idiosyncratic", "compositional", "compositional"))
    ) %>%
    group_by(segment) %>%
    mutate(direction = sign(last(delta)))

plot_ = ggplot(D) +
    geom_hline(yintercept=0, linetype="solid", color=GRAY) +
    geom_line(mapping=aes(p0, delta), linewidth=.5) +
    # annotate("rect", xmin = 0, xmax = stable$start, ymin = -Inf, ymax = Inf, alpha = .3, fill=TEAL) +
    # annotate("rect", xmin = stable$start, xmax = 1, ymin = -Inf, ymax = Inf, alpha = .25, fill=RED) +
    lapply(split(D, D$segment), function(df)
      geom_line(data = df, aes(p0, 0, color = attractor),
                size=.8,
                arrow = arrow(
                     length = unit(.1, "in"), type = 'open',
                     ends = if (first(df$direction) < 0) 'first' else 'last',
                ))
    ) +
    geom_point(x=0, y=0, shape=21, size=2, fill=TEAL) +
    geom_point(x=stable$start, y=0, shape=21, size=2, fill="white") +
    geom_point(x=stable$stop, y=0, shape=21, size=2, fill=RED) +
    no_gridlines +
    # coord_fixed(expand=T) +
    # labs(x="previous compositionality rate", y="new compositionality rate") +
    labs(x="compositionality", y="change in compositionality") +
    cpal + no_legend

fig("x_dx", w=2.9)
"""



# %% ==================== bottleneck ====================
include("red_black.jl")

SS = 20
d100 = dataframe(grid(S=SS)) do (;S)
    map([1:10S; 10S:S:1000S]) do D
        (;D, find_stable_points(;S, D)...)
    end
end
@rput d100 SS

R"""

plot_zones = function(SS) {
    zone_data = tibble(
        xmin=SS * c(0, 2.5, 10, SS, 5*SS),
     ) %>% mutate(
        xmax = lead(xmin, default=Inf),
        zone=factor(row_number())
    )
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
        # geom_text(data=text_data, mapping=aes(x=x, label=name, color=zone), y=1.12, hjust=0.5, nudge_x=.1),
        coord_cartesian(expand=T, xlim=c(1, 1000), ylim=c(0, 1), clip='off'),
        scale_fill_manual(values=c(TEAL, "#94DCE6", RED, "#94DCE6", TEAL)),
        theme(plot.margin=margin(t=15, l=5, r=5, b=5)),
        no_gridlines
    )
}


plot_bottle = d100 %>%
    pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
    ggplot(aes(D, value)) +
    plot_zones(SS) +
    geom_line(linewidth=1.5, mapping=aes(group=name)) +
    geom_line(linewidth=.7, mapping=aes(color=name)) +
    scale_x_log10() +
    labs(y="Compositionality", x="Demonstrations") +
    scale_colour_manual(values=c(
        start="white", stop=RED,
        `1`=TEAL, `2`=RED, `3`=TEAL
    ), aesthetics=c("colour"), name="") +
    no_legend +
    coord_cartesian(expand=T, xlim=c(SS, 10 * SS**2), ylim=c(0, 1)) +
    # coord_cartesian(xlim=c(NULL), ylim=c(0, .01)) +
    theme()

fig("bottleneck")
"""

# %% --------
# D = unique(round.(logscale.(0:.01:1, 1, 10000)))
D=1:100
# D = 2 .^ (0:15)
fixed_grid = dataframe(grid(;S=1:10, D)) do (;S, D)
    find_stable_points(;S, D)
end
@rput fixed_grid

R"""
fixed_grid %>%
    replace_na(list(stop = 0)) %>%
    ggplot(aes(D, S, fill=(start < .01) * stop)) +
    geom_tile() +
    # scale_x_continuous(trans="log2") +
    # scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=2, mid=0.5, rev=F) +
    scale_fill_continuous_sequential(h1=350, h2=NA, c1=180, l1=20, l2=95, p1=1, p2=1.5,
            name="Asymptotic Compositionality", labels=scales::percent_format(), limits=c(0, 1)) +
    no_gridlines +
    theme()

fig(w=4)
"""

# %% --------

D = [10:999; round.(logscale.(0:.01:1, 1000, 10000))]

df = dataframe(grid(;S=[5, 10, 20, 40], D)) do (;S, D)
    find_stable_points(;S, D)
end
@rput df

R"""
df %>%
    filter(D/S > 1, D/S < 500) %>% 
    pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
    ggplot(aes(D / S, value, color=factor(S), group=interaction(name, S))) +
    geom_line() +
    scale_x_log10() +
    theme()

fig()
"""


# %% ==================== MpS stable ====================


df = dataframe(grid(S=[5,10,20,40,80], p_r = [0., .5, 1.], p_brr = [0., .5, 1.])) do (;S, p_r, p_brr)
    map([1:10S; 10S:S:500S]) do D
        (;D, find_stable_points(;S, D, p_r, p_brr)...)
    end
end
df
@rput df

R"""
plot_start_stop = function(data) {
    data %>%
    filter(D/S > .3) %>%
    pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
    ggplot(aes(D / S, value, color=factor(S), group=interaction(name, S))) +
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


R"""
D = df %>%
    filter(D < 70) %>%
    pivot_longer(c(start, stop), names_to="name", values_to="value") %>%
    drop_na()

D2 = df %>%
    filter(D < 70) %>%
    replace_na(list(stop = 0, start=0, zero=0))

ggplot(D, aes(D)) +
    geom_ribbon(data=D2, mapping=aes(ymin=stop, ymax=1), fill=TEAL, alpha=0.3) +
    geom_ribbon(data=D2, mapping=aes(ymin=start, ymax=stop), fill=RED, alpha=0.3) +
    geom_ribbon(data=D2, mapping=aes(ymin=0, ymax=start), fill=TEAL, alpha=0.3) +
    # geom_line(mapping=aes(group=name, y=value), linewidth=.5, color=GRAY) +
    geom_line(
        data=filter(D, mod(D, 5) == 0),
        mapping=aes(group=D, y=value),
        arrow = arrow(ends='last', length = unit(.1, "in")),
        color=RED
    ) +
    facet_wrap(~S)
    facet_wrap(~S)

fig(h=4, w=7)
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

# %% ==================== MS stable ====================


df = dataframe(grid(S=3:50, D=1:50, p_r = 0:.2:1)) do (;S, D, p_r)
# df = dataframe(grid(S=[10, 20], D=[40, 80], p_r = 0:.01:1)) do (;S, D, p_r)
    find_stable_points(;S, D, p_r)
end

@rput df

# %% --------

R"""
df %>%
    replace_na(list(start=1)) %>%
    ggplot(aes(D, S, fill=start)) +
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
    ggplot(aes(D, S, fill=stop)) +
    rasterise(geom_tile(), dpi=DPI) +
    coord_fixed(expand=F) +
    no_gridlines +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=95, p1=1, p2=2, mid=0.5) +
    facet_wrap(~p_r)

fig(w=8, h=4)
"""

# %% --------

df = dataframe(grid(p0=0:.01:1, S=10, D=1:100)) do (;p0, S, D)
    env = RedBlackEnv(;S, D, p_r=0.)
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
    # ggplot(aes(D, p0, fill=2*(delta > 0) - 1)) +
    ggplot(aes(p0, D, fill=logdelta)) +
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
    group_by(D, population, generation) %>%
    summarise(compositionality = mean(path_length == 2)) %>%
    mutate(agent = "human")

# df = bind_rows(mutate(sim, agent="model", population=100+population), human)

sim %>%
    # filter(generation < 9) %>%
    ggplot(aes(generation, 1*compositionality, group=population)) +
    geom_line(linewidth=.5, color=RED, alpha=.3) +
    # geom_line(linewidth=.5, color="#BA1109", alpha=0.5) +
    geom_line(data=human) +
    facet_wrap(~D, labeller=label_glue("{D} Demos"), scales="free_y") +
    xbreaks() + ybreaks() +
    expand_limits(y=1.)


fig("experiment", w=7)
"""
