using Test

include("infinite_env.jl")
include("test_utils.jl")

# %% --------

test_envs = create_test_objects(splatify(InfiniteEnv), (
    S = (1, 50), 
    D = (0, 100), 
    p_0 = (0., 1.), 
    p_r = (0., 1.)
))
length(test_envs)

@testset "observation_probabilities" begin
    foreach(test_envs) do env
        foreach([0., rand(), 1.]) do c
            P = observation_probabilities(env.S, env.D, c)
            @test sum(P) ≈ 1
        end
    end
end

# %% --------

# Example usage of create_test_objects function with FreqPop
test_pops = create_test_objects(x->normalize(FreqPop(;x...)), (
    bespoke_zilch = (0., 1.), 
    bespoke_full = (0., 1.), 
    comp_zilch = (0., 1.), 
    comp_partial = (0., 1.), 
    comp_full = (0., 1.)
); n_rand=1) |> pops->filter(p->!any(isnan, p), pops)

@testset "transition" begin
    foreach(product(test_envs, test_pops)) do (env, pop)
        pop2 = transition(env, pop)
        @test sum(pop2) ≈ 1
    end
end