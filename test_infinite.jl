using Test

include("infinite_env.jl")
include("test_utils.jl")

# %% --------

test_envs = create_test_objects(splatify(InfiniteEnv), (
    S = (1, 50), 
    D = (0, 100), 
    b0c0 = (0., 1.), 
    b0c1 = (0., 1.)
))


@testset "observation_probabilities sum to 1" begin
    foreach(test_envs) do env
        foreach([0., rand(), 1.]) do c
            P = observation_probabilities(env, CompPop(c))
            @test sum(P) ≈ 1
        end
    end
end


@testset "FreqPop matches CompPop" begin
    
    for env in test_envs
        S = env.S
        # C = normalize(rand(S, S))
        # B = normalize(rand(S, S))
        c = rand()

        # full = FullPop{S}(c)
        comp = CompPop(c)
        freq = FreqPop(CompPop(c))

        # @test compositional_rate(full) ≈ c
        @test compositional_rate(comp) ≈ c
        @test compositional_rate(freq) ≈ c


        # @test transition(env, full) |> compositional_rate ≈ transition(env, c)
        @test transition(env, comp) |> compositional_rate ≈ transition(env, c)
        @test transition(env, freq) |> compositional_rate ≈ transition(env, c)
    end
end

