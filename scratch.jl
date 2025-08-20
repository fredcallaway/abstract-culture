
prm = reparametrize((;S=4, D=16, act_cost=8, search_cost=2, β=2.0, ε=0.05))
env, C = get_env_costs(prm)

compute_evolution(prm)
env.agent_policy
simulate(env, 1; init=FreqPop())


simulate(env, 10; init=FreqPop())
observation_probabilities(env, FreqPop())


