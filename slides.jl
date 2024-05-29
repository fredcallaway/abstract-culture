@everywhere include("black_red.jl")
@everywhere using NamedTupleTools
include("r.jl")
using Optim
# %% --------


R"""
FIGS_PATH = "figs/neuromonster/"
MAKE_PDF = FALSE
DPI = 500
"""

# %% ==================== setup ====================

function run_sim_infinite(;n_gen=50, init=NaN,
                 p_0 = [1e-6],
                 p_brr = [0.],
                 p_r = [1.],
                 n = [5, 10, 20],
                 m = [10, 20, 40])

    g = grid(; p_0, p_brr, p_r, n, m)
    df = dataframe(g) do prm
        env = Environment(;prm...)
        sim = simulate(env, n_gen; init)
        map(enumerate(sim)) do (gen, red)
            (;gen, red)
        end
    end
    @rput df
    df
end

R"""
plot_red = function(...) {
    df %>%
        ggplot(aes(gen, red, ...)) +
        geom_line() +
        facet_grid(m ~ n, labeller=label_glue(cols='S={n}', rows='M={m}'))

}
"""


function run_sim_finite(;n_gen=30, init=NaN,
                 p_0 = [0],
                 p_brr = [0.],
                 p_r = [1.],
                 n = [5, 10, 20],
                 m = [10, 20, 40],
                 k = [1],
                 N = [500],
                 foresight = [false],
                 repeats = 5)

    g = grid(; p_0, p_brr, p_r, n, m, k, N, foresight, pop=1:repeats)
    df = dataframe(g; parallel=true) do prm
        env = Environment(;delete(prm, :pop)...)
        sim = simulate(env, n_gen)
        map(enumerate(sim)) do (gen, pop)
            (;gen, red = red_rate(pop))
        end
    end
    @rput df
    df
end

# %% --------

g = grid(
    m=1:160,
    n=[1,2,4,8]
)

df = dataframe(g) do (;n, m)
    black = prob_observe(1 / (n^2), m)
    # prob observe one of the desired red edges (not both)
    red = prob_observe(1 / n, m)^2
    (;black, red)
end

@rput df

R"""
df %>%
    pivot_longer(c(red, black), names_to="name", values_to="value", names_prefix="") %>%
    ggplot(aes(m, value, color=name)) +
    scale_colour_manual(values=c(
        BLACK, RED
    ), aesthetics=c("fill", "colour"), name="") +
    no_legend +
    geom_line() +
    labs(x="m (number of demonstrations)", y="p(observe my solution)") +
    facet_wrap(~n, nrow=1, labeller=label_glue('S = {n}',))

fig("p_observe", w=6, h=2)
"""

# %% ==================== heatmap ====================

g = grid(
    m=2:1:100,
    n=2:1:100
)

df = dataframe(g) do (;n, m)
    black = prob_observe(1 / (n^2), m)
    red = prob_observe(1 / n, m)^2
    (;black, red)
end

df2 = map(1:100) do n
    res = optimize(1, 20n) do m
        black = prob_observe(1 / (n^2), m)
        red = prob_observe(1 / n, m)^2
        -(red - black)
    end
    (;n, n2=n^2, m=res.minimizer, val=-res.minimum)
end |> DataFrame

@rput df df2

R"""
df2 = filter(df2, val > 0)
df %>%
    ggplot(aes(n, m, fill=red-black)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    geom_line(aes(y=n2, fill=NULL), df2, linewidth=.5, linetype="dashed") +
    no_gridlines +
    geom_abline() +
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(expand=F, ylim=c(2,100)) +
    labs(fill="Compositional Learning Advantage\n", x="Goals (S)", y="Social Observations (M)")

fig("advantage_heat_alt", w=4)

"""

# %% --------

g = grid(
    m=2:1:100,
    n=2:1:100
)
df = dataframe(g) do (;m, n)
    env = Environment(;m, n, p_0=.01)
    red, gen = get_limit(env; max_gen=1000)
    [(;gen, red)]
end
@rput df

R"""
df %>%
    ggplot(aes(n, m, fill=red)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    # geom_line(aes(y=n2, fill=NULL), df2, linewidth=.5, linetype="dashed") +
    no_gridlines +
    geom_abline() +
    geom_line(aes(y=n2, fill=NULL), df2, linewidth=.5, linetype="dashed") +
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(expand=F, ylim=c(2,100)) +
    labs(fill="Asymptotic Compositionality", x="Goals (S)", y="Social Observations (M)")

fig("limit_heat", w=4.2)
"""

# %% ==================== individual ====================

run_sim_finite(;n_gen = 1, m=[0], k=[20], foresight=[true])

R"""
df %>%
    group_by(n) %>%
    summarise(mean(red))
"""
# %% --------




# %% ==================== m and n ====================

# run_sim_infinite(p_0=[1e-6], p_brr = [0.], p_r = [1])

run_sim_infinite(p_0=[1e-6], n=[4], m=[10])


R"""
df %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED) +
    ylab("Compositionality")

fig("basic_single", w=4, h=2)
"""

# %% --------

run_sim_infinite(p_0=[1e-6], n=[4], m=[2,8,32])

R"""
df %>%
    ggplot(aes(gen, red)) +
    geom_line(color=RED) +
    facet_wrap(~ m, labeller=label_glue('M={m}')) +
    # facet_grid(n ~ m, labeller=label_glue(cols='M={m}', rows='S={n}'))
    no_legend

fig("basic", w=5, h=2)
"""


# %% --------


g = grid(
    m=[2,8,32],
    n=[4]
)

learning = dataframe(g) do (;n, m)
    black = prob_observe(1 / (n^2), m)
    # prob observe one of the desired red edges (not both)
    red = prob_observe(1 / n, m)^2
    (;black, red)
end

R"""
learning %>%
    mutate(diff = red - black)
"""