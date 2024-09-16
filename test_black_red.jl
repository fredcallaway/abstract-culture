using Test
include("red_black.jl")


function consistency_test()
    Random.seed!(1)
    env = Environment(k=15, m=10, n=10; ε=0.)
    map(red_rate, simulate(env, 100; state=initial_population(env, 100)))
end

@testset "consistency" begin
    if !isfile(".consistency")
        @info "Generating .consistency"
        serialize(".consistency", consistency_test())
    end
    @test consistency_test() ≈ deserialize(".consistency")
end

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


# env = RedBlackEnv(S=5, K=10)
# (;S, K) = env

# k = fresh_knowledge(env)
# k.black[2, 1] = k.black[1,2] = true
# k.red_a[2] = k.red_b[2] = k.red_b[3] = true

# empirical = monte_carlo(100_000) do
#     k1 = deepcopy(k)
#     n_node = n_edge = 0
#     for (a, b) in sample_tasks(env)
#         if !k1.red_a[a]
#             n_node += 1
#             k1.red_a[a] = true
#         end
#         if !k1.red_b[b]
#             n_node += 1
#             k1.red_b[b] = true
#         end
#         if !(k1.black[a,b] || k.red_a[a] && k.red_b[b])
#             n_edge += 1
#             k1.black[a,b] = true
#         end
#     end
#     [n_node, n_edge]
# end

# predicted = expected_cost(env, k)
# empirical = typeof(predicted)(empirical)

# DataFrame([predicted, empirical])

# using Combinatorics
# function find_compositions_exhaustive(tasks)
#     tasks = unique(tasks)
#     S, G = invert(tasks)
#     S = unique(S)
#     G = unique(G)


#     best_cost = base_cost = length(tasks)
#     optimal = Tuple{Vector{Int64}, Vector{Int64}}[]

#     compositions = product(powerset(S), powerset(G))
#     foreach(compositions) do (ss, gg)
#         black_cost = sum(tasks) do (s, g)
#             s in ss && g in gg ? 0 : 1
#         end
#         red_cost = length(ss) + length(gg)
#         total_cost = black_cost + red_cost
#         if total_cost < best_cost
#             best_cost = total_cost
#             empty!(optimal)
#             push!(optimal, (ss, gg))
#         elseif total_cost == best_cost && total_cost < base_cost
#             push!(optimal, (ss, gg))
#         end
#     end
#     optimal
# end
