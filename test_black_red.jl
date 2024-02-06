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



using Combinatorics
function find_compositions_exhaustive(tasks)
    tasks = unique(tasks)
    S, G = invert(tasks)
    S = unique(S)
    G = unique(G)


    best_cost = base_cost = length(tasks)
    optimal = Tuple{Vector{Int64}, Vector{Int64}}[]

    compositions = product(powerset(S), powerset(G))
    foreach(compositions) do (ss, gg)
        black_cost = sum(tasks) do (s, g)
            s in ss && g in gg ? 0 : 1
        end
        red_cost = length(ss) + length(gg)
        total_cost = black_cost + red_cost
        if total_cost < best_cost
            best_cost = total_cost
            empty!(optimal)
            push!(optimal, (ss, gg))
        elseif total_cost == best_cost && total_cost < base_cost
            push!(optimal, (ss, gg))
        end
    end
    optimal
end
find_compositions_exhaustive(tasks)