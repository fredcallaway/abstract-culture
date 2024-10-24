include("utils.jl")
include("r.jl")
# %% --------

properties((;B, N)) =     (
        guess_rate = B / N,
        chance_compositionality = 1 / B,
        partial_solution_prob = 1/√N,
        partial_advantage = √N / B,
    )


df = dataframe(properties, grid(B=1:20, N=2 .^ (5:8)))
R"""
$df %>%
    ggplot(aes(B, partial_advantage, color=factor(N))) +
    geom_line() +
    teals_pal()

fig()
"""

R"""
$df %>%
    filter(partial_advantage > 1) %>%
    ggplot(aes(partial_advantage, chance_compositionality, color=guess_rate)) +
    geom_point()

fig(w=8)
"""

# %% --------


df = dataframe(properties, grid(B=(16, 20, 27), N=(10:24) .^ 2))

R"""
$df %>% ggplot(aes(guess_rate, partial_advantage, color=factor(B))) +
    geom_point() +
    geom_hline(yintercept=1) +
    geom_vline(xintercept=.1)g

fig()
"""
# %% --------

R"""
$df %>% ggplot(aes(guess_rate, partial_advantage, color=factor(N))) +
    geom_line() +
    teals_pal() +
    geom_hline(yintercept=1) +
    geom_vline(xintercept=.1)

fig()
"""

# %% --------


params = flatmap(4:6) do S
    map(1:5) do m
        (;B=S*m, N=S^4, m)
    end
end

params = grid(B=10:25, N=(4^4, 5^4, 6^4))

df = dataframe(properties, params)

R"""
$df %>%
    filter(B > 10) %>%
    ggplot(aes(guess_rate, partial_advantage, color=factor(B))) +
    geom_point() +
    geom_hline(yintercept=1) +
    geom_vline(xintercept=.1) +
    facet_grid(~N)

fig(w=8)
"""

# %% --------


1321

1382
