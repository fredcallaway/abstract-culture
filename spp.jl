@everywhere include("black_red.jl")
include("r.jl")

R"""
FIGS_PATH = "figs/spp/"
"""

# %% ==================== individual ====================
g = grid(
    k=1:100,
    n = [10, 20, 40],
)
df = dataframe(g; parallel=true) do prm
    env = Environment(;m=0, N=1000, foresight=true, p_r=0, prm...)
    (;red = red_rate(simulate(env, 1)[1]))
end

@rput df

R"""
df %>%
    ggplot(aes(k, red, color=factor(n))) +
    geom_line() +
    discrete_sequential("")

fig()
"""
# %% --------


K = env.K
fill!(K, false)
for b in observed
    learn!(K, b)
end
if env.foresight
    # note: this modifies K
    find_compositions!(env, tasks)
end


# %% ==================== heatmap ====================
prob_black(n, m) = prob_observe(1 / (n^2), m)
prob_red(n, m) = prob_observe(1 / n, m)^2

prob_black(100, 1000)
prob_black(1000, 10000)

prob_red(100, 1000)

# %% --------

g = grid(
    m=5:5:5000,
    n=5:5:500
)

df = dataframe(g) do (;n, m)
    black = prob_observe(1 / (n^2), m)
    red = prob_observe(1 / n, m)^2
    (;black, red)
end

df2 = map(1:500) do n
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
    scale_fill_continuous_sequential(h1=0, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1,p2=1) +
    coord_cartesian(xlim=c(0,500), ylim=c(0,5000), expand=F) +
    labs(fill="Compositional Learning Advantage\n", x="Number of Starts and Goals (n)", y="Social Observations (m)", )


fig("advantage_heat", w=4.5, h=3)
"""

# %% ==================== heatmap ====================

g = grid(
    m=1:1:2500,
    n=1:1:50
)

df = dataframe(g) do (;n, m)
    black = prob_observe(1 / (n^2), m)
    red = prob_observe(1 / n, m)^2
    (;black, red)
end
@rput df

R"""
df %>%
    mutate(
        width = (n+1)^2 - n^2,
        n2 = n^2
    ) %>%
    ggplot(aes(n2, m, fill=red-black, width=width)) +
    geom_tile() +
    # geom_line(aes(fill=NULL), df2, color="white", linewidth=.5) +
    # geom_line(aes(y=n2, fill=NULL), df2, linewidth=.5, linetype="dashed") +
    no_gridlines +
    geom_abline() +
    geom_line(data=tibble(n2 = seq(1,2600,10)), mapping=aes(x=n2, y=sqrt(n2), width=NULL, fill=NULL), linetype="dashed", linewidth=.5) +
    scale_fill_continuous_sequential(h1=1, h2=NA, limits=c(0,1), c1=200, l1=30, l2=70, p1=1, p2=1, labels=scales::percent_format()) +
    coord_cartesian(xlim=c(0,2500), ylim=c(0,2500), expand=F) +
    labs(fill="Compositional Learning Advantage\n", x="Unique Tasks", y="Social Observations", )

fig("advantage_heat", w=4.5, h=3)
"""

