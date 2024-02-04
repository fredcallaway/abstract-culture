using QuadGK
using Distributions

Strategy = Float64
Problem = Float64

cost(problem, strat::Strategy) = abs(problem - strat)

function cost(problem, strats::Vector{Strategy})
    minimum(strats) do strat
        cost(problem, strat)
    end
end

expectation(f, lo, hi) = first(quadgk(f, lo, hi))

function expectation(f::Function, d::Distribution; lo=quantile(d, .00001), hi=quantile(d, .99999))
    expectation(lo, hi) do x
        pdf(d, x) * f(x)
    end
end

function expected_cost(prob_dist::Distribution, strats)
    expectation(prob_dist) do problem
        cost(problem, strats)
    end
end

function ev_new_strategy(prob_dist::Distribution, strat_dist::Distribution, init_strats)
    current = expected_cost(prob_dist, init_strats)
    strats = [init_strats; NaN]
    current - expectation(strat_dist) do strat
        strats[end] = strat
        expected_cost(prob_dist, strats)
    end
end

function ev_new_strategy(prob_dist::Distribution, strat::Strategy, init_strats)
    current = expected_cost(prob_dist, init_strats)
    alt = expected_cost(prob_dist, [init_strats; strat])
    current - alt
end

function individual_learning(prob_dist::Distribution, strat_dist::Distribution, init_strats, learn_cost)
    strats = copy(init_strats)
    while ev_new_strategy(prob_dist, strat_dist, strats) > learn_cost
        push!(strats, rand(strat_dist))
    end
    strats
end

function social_learning(prob_dist::Distribution, strategies::Vector, init_strats, learn_cost)
    strats = copy(init_strats)
    # so arbitrary....
    for strat in strategies
        ev_new_strategy(prob_dist, strat, strats)
        push!(strats, strat)
    end
    strats
end

prob_dist = Normal(0, 1)
strat_dist = Normal(0, 1)
social_strats = [0.]
learn_cost = .03
strats = individual_learning(prob_dist, strat_dist, social_strats, learn_cost)

prob_dist = Normal(0,5); strat_dist = prob_dist
strats = individual_learning(prob_dist, strat_dist, strats, learn_cost)

# %% --------

@time ev_new_strategy(prob_dist, strat_dist, social_strats)

expected_cost(prob_dist, strats)
