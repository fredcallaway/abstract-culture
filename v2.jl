include("utils.jl")
using Distributions
using StatsBase
using Random
include("r.jl")
include("figure.jl")


function learning(type, observed)
    learned = type  # will learn own strategy if no observed strategy
    for o in observed
        if o == type
            return o
        elseif o != 0
            learned = o
        end
    end
    learned
end

function step!(props, p1, n_observe)
    w = Weights(props)
    type = rand(Bernoulli(p1)) ? 1 : 2
    observed = sample(0:2, w, n_observe)
    strategy = learning(type, observed)
    old_strategy = sample(0:2, w)
    props[old_strategy+1] -= 1
    props[strategy+1] += 1
end

function simulate(f::Function=copy; N=1000, p1=0.5, n_observe=1, init=[N, 0, 0], n_step=1000)
    type_dist = Categorical([p1, 1-p1])
    props = init
    repeatedly(n_step) do
        step!(props, p1, n_observe)
        f(props)
    end
end

# %% --------


map(0:.01:1) do p1
    m = 1
    p2 = 1 - p1
    p_new1 = 0.5 * ((1 - p2^m) + p1^m)
end

# %% --------
using LinearAlgebra

params = grid(
    p1 = 0:.01:1,
    m = 1:4
)
df = map(params[:]) do (;p1, m)
    p2 = 1 - p1
    p_dead1 = p1

    p_new1 = 0.5 * ((1 - p2^m) + p1^m)
    P = [p_dead1, (1-p_dead1)] * [p_new1 1 - p_new1]
    (;p1, m, down = P[1, 2], stay = sum(diag(P)), up = P[2, 1])
end |> DataFrame


@rput df

R"""
df %>% rename(prop1=p1) %>%
    pivot_longer(c(up, stay, down), names_to="change", values_to="probability", names_prefix="") %>%
    filter(change != "stay") %>%
    ggplot(aes(prop1, probability, color=change)) +
    geom_line(alpha=0.5) +
    facet_wrap(~m, labeller=label_both, nrow=1)

fig(w=8)
"""



# %% --------
params = grid(
    N = [100],
    n_observe = [1, 2, 3],
    chain = 1:10
)

res = @showprogress map(params) do (;N, n_observe, chain)
    step = 0
    simulate(;N, n_observe, n_step=30000) do sc
        step += 1
        prop1 = sc[2] / (sc[2] + sc[3])
        (;N, n_observe, chain, step, prop1)
    end
end

df = DataFrame(reduce(vcat, res))
@rsubset! df mod(:step, 10) == 0
@rput df
R"""
df %>%
    ggplot(aes(step, prop1, group=chain)) +
    geom_line(size=.5, alpha=0.5) +
    facet_wrap(~n_observe)

fig(w=6)
"""

# %% --------

function convergence_time(; N, n_observe, p1=0.5, max_steps=500000)
    props = [N, 0, 0]
    n_step = 0
    while props[2] != N && props[3] != N
        n_step += 1
        if n_step == max_steps
            return missing
        end
        step!(props, p1, n_observe)
    end
    n_step
end

params = grid(
    N = [50, 100, 200],
    n_observe = [1, 2, 3],
    chain = 1:10
)

res = @showprogress map(params) do (; N, n_observe)
    (; N, n_observe, n_steps = convergence_time(;N, n_observe))
end
df = DataFrame(;invert(collect(res)[:])...)

@rput df


R"""
tibble(df)
df %>%
    rename(M = n_observe) %>%
    ggplot(aes(n_steps)) +
    geom_histogram(position="identity", alpha=0.7) +
    facet_wrap(N~M)

fig(h=8, w=8)
"""
