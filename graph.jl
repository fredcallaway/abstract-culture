using Memoize
using Graphs

include("utils.jl")
# include("find_compositions.jl")

¬(p::Real) = 1 - p
prob_observe(p, m) = ¬((¬p) ^ m)


@kwdef struct Environment
    S::Int = 5  # number of states
    M::Int = 5  # number of models
    K::Int = 1  # number of tasks per agent
    ε::Float64 = 0. # lapse rate
    # T::Matrix{Float64} = normalize!(ones(S, S))  # task distribution
    N::Real = 100 # population size
    # foresight::Bool = false  # choose composition in advance?
    # red frequencies for ambiguous cases
    # p_0::Float64 = 0.  # no info
    # p_r::Float64 = 1.  # part red
    # p_brr::Float64 = 0.  # full both

    # used to avoid memory allocation
    E::BitMatrix = falses(S, S)
end

function initial_population(env::Environment)
    normalize!(ones(env.S, env.S))
end

@memoize function taskdist(env::Environment)
    S = env.S
    filter(collect(Iterators.product(1:S, 1:S))) do (s,g)
        s != g
    end
end

function transition(env::Environment, pop::Matrix{Float64})
    (;N, E, M) = env

    pop1 = zeros(size(pop))
    wts =  Weights(@view pop[:])
    n_obs = min(sum(!isequal(0), wts), M)

    edge_cost = 1 .+ .01 * rand(size(E)...)

    for i in 1:N
        fill!(E, false)
        observed = sample(eachindex(pop), wts, n_obs; replace=false)
        E[observed] .= true
        g = DiGraph(E)

        (start, goal) = rand(taskdist(env))

        path = a_star(g, start, goal, edge_cost)
        if isempty(path)
            push!(path, Edge(start, goal))
        end
        for edge in path
            pop1[edge.src, edge.dst] += 1
        end
    end
    normalize!(pop1)
end


function simulate(env::Environment, n_gen; init=initial_population(env))
    pop = init
    # note: initial pop is a dummy, containing no cultural knowledge
    repeatedly(n_gen) do
        pop = transition(env, pop)
        pop
    end
end

function get_limit(env::Environment; init=initial_population(env), max_gen=100000)
    pop = init
    @assert env.N == Inf
    for gen in 0:max_gen
        pop′ = transition(env, pop)
        if pop′ ≈ pop
            return (pop, gen)
        end
        pop = pop′
    end
    @warn "hit max_gen"
    missing
end

# %% --------
include("figure.jl")
using GraphMakie
using NetworkLayout

# pop = zeros(env.S, env.S)
# pop[1, 2:end] .= 1.
# pop[2:end, 1] .= 1
# # pop[1, 2] = 1.
# # pop[2, 3] = 1.
# # pop[3, 4] = 1.
# # pop[4, 1] = 1.
# pop .+= .1
# normalize!(pop)
# # sim = simulate(env, 100; init=pop)

# %% --------

env = Environment(S=5, M=6, N=100)
monte_carlo(100) do
    sim = simulate(env, 30)
    visit_rates = sum(sim[end]; dims=1)[:]
    hubbiness, hub = findmax(visit_rates)
    hubbiness /= (1/env.S)
end

# %% --------

env = Environment(S=5, M=6, N=30)
sim = simulate(env, 100)

pop = sim[end]
figure() do
    g = complete_digraph(5)
    edge_width = 20 .* [pop[e.src, e.dst] for e in edges(g)]
    gp = graphplot!(g, layout=Shell(), node_size=25, arrow_size=10 .* (edge_width .^ 0.5), arrow_shift=:end; edge_width)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end

# %% --------
sim = simulate(env, 100)
initial_population(env)

g = complete_digraph(5)
fig, ax, gp = graphplot(g, layout=Shell(), node_size=25, arrow_size=ones(env.S^2-env.S), arrow_shift=:end; edge_width=ones(env.S^2-env.S))
ax = current_axis()
hidedecorations!(ax); hidespines!(ax)
ax.aspect = DataAspect()

# animation settings
framerate = 10

record(fig, "anim.mp4", sim; framerate = framerate) do pop
    edge_width = 20 .* [pop[e.src, e.dst] for e in edges(g)]
    gp.edge_width[] = edge_width
    gp.arrow_size[] = 10 .* (edge_width .^ 0.5)
    gp.arrow_size[]
end
run(`open anim.mp4`)