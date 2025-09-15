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

        fpop = transition(fmodel, initial_population(fmodel, c))
        ipop = transition(imodel, FreqPop(CompPop(c)))

        icomp = compositional_rate(ipop)
        fcomp = compositional_rate(fpop)

        costs = Costs(rand(), rand(), rand(), rand(), rand())
        icost = cost(costs, ipop)
        fcost = cost(costs, fpop)

        icomp, fcomp, icost, fcost
    end

    @testset "Compositional rate" for (icomp, fcomp) in results
        @test icomp ≈ fcomp atol=1e-3
    end

    @testset "Costs" for (_, _, icost, fcost) in results
        @test icost ≈ fcost atol=1e-3
    end
end;
