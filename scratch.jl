include("infinite_env.jl")

env = InfiniteEnv(S=2, D=40)
fixed = fixed_points(env)

transition(env, fixed[end])

transition(env, Rational{BigInt}(1))
transition(env, BigFloat(1))

observation_probabilities(1, 40, 0)