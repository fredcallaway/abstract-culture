include("model_finite.jl")
include("model_infinite.jl")

# %% --------

S = 4
G = 3
D = 8
c = .1

fmodel = FiniteModel(;S, G, D, N=1_000_000)
imodel = InfiniteModel(;S, G, D)

fpop = transition(fmodel, initial_population(fmodel, c))
ipop = transition(imodel, FreqPop(CompPop(c)))

costs = Costs(100, 10, 150, 75, 50)

@test cost(costs, fpop) ≈ cost(costs, ipop) atol=0.1
