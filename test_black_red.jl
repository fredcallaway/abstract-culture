using Test
include("black_red.jl")

@testset "matrix vs frequency" begin
    env = Environment()
    sim = simulate(env, 10)
    p1 = map(sim) do P
        n = size(P, 1)
        sum(P[1:n, n+1:end]) / 2
    end

    p2 = simulate(env, 10; state=0.)
    @test p1 ≈ p2
end