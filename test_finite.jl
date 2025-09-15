using Test
using Distributed
using ProgressMeter

nprocs() == 1 && addprocs(8)

@everywhere begin
    include("model_finite.jl")
    include("model_infinite.jl")
    include("test_utils.jl")
end

# %% --------

test_envs = create_test_objects((
    S = (1, 10),
    G = (1, 10),
    D = (5, 30),
), n_rand=2)

@testset "FiniteModel matches InfiniteModel" begin
    results = @showprogress pmap(test_envs) do prm
        pwr = 7
        fmodel = FiniteModel(;prm..., N=10^pwr)
        imodel = InfiniteModel(;prm...)

        c = round(rand(), digits=pwr-1)

        icomp = transition(imodel, CompPop(c)) |> compositional_rate
        fcomp = transition(fmodel, initial_population(fmodel, c)) |> compositional_rate

        icomp, fcomp
    end

    for (icomp, fcomp) in results
        @test icomp ≈ fcomp atol=1e-3
    end
end
