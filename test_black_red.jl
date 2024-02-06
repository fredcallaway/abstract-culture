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


@testset "agent vs frequency" begin
    env = Environment(k=1, m=10, n=5; ε=0.)


    foreach(0:5) do n_red
        red = vcat(fill(true, n_red), fill(false, 5-n_red))
        p_red = mean(red)

        g = grid(
            task = taskdist(env.T).vals,
            red = red
        )

        pop = map(repeat(collect(g), 1000)[:]) do ((s, g), red)
            Behavior(s,g,red)
        end
        pop = reshape(pop, 1, :)

        pop1 = transition(env, pop)
        @test mean(get.(pop1, :red)) ≈ transition(env, p_red) atol=.02
        # agent-based is systematically higher than infinite pop...
    end
end