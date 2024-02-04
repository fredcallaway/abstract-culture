using Distributions
using Combinatorics
using Primes
using Memoize


primes(100)[1:10]
include("utils.jl")
include("figure.jl")

struct FactorizationProblemDistribution
    k::Int
    base::Vector{Int}
end

@memoize function problems(fpd::FactorizationProblemDistribution)
    collect(Iterators.map(prod, with_replacement_combinations(fpd.base, fpd.k)))
end

# %% --------

Library = PriorityQueue{Int, Int}

function add_option(lib::Library, o::Int)
    lib[o] = length(factor(Set, o))
end

lib = Library(Base.Order.Reverse)

function solve(problem::Int, library::Library)
    steps = 0
    for n in keys(library)
        if problem % n == 0
            steps += 1
            problem ÷= n
            problem == 1 && break
        end
    end
    @assert problem == 1
    steps
end

add_option(lib, 7)
add_option(lib, 2 * 5)
add_option(lib, 2 * 3 * 5)

x = solve(2 * 3 * 5 * 7, lib)

# %% --------





struct Agent
    k::Int
    library::Vector{Strategy}
end

function solutions(agent::Agent)
    Iterators.flatmap(1:agent.k) do k
        with_replacement_combinations(agent.library, k)
    end
end

function cognitive_cost(agent::Agent)
    sum(1:agent.k) do k
        length(with_replacement_combinations(agent.library, k))
    end
end

function world_cost(problem::Int, sol::Solution)
    abs(prod(sol) - problem)
end

function world_cost(problem::Int, agent::Agent)
    minimum(solutions(agent)) do sol
        world_cost(problem, sol)
    end
end

function total_cost(fpd::FactorizationProblemDistribution, agent::Agent; N=nothing)
    probs = isnothing(N) ? problems(fpd) : sample(problems(fpd), N)
    mean(probs) do prob
        world_cost(prob, agent)
    end + cognitive_cost(agent)
end

function individual_learning!(agent::Agent, fpd::FactorizationProblemDistribution)
    union!(agent.library, rand(fpd.base))
end

function social_learning!(agent::Agent, model::Agent)
    union!(agent.library, rand(model.library))
end

function simulate(;n_agent=100, n_gen=10, n_social=1)
    base = primes(50)
    pk = 4
    k = 2
    β_learn = .01

    pop = repeatedly(n_agent) do
        Agent(k, [])
    end
    prev_pop = similar(pop)
    p = fill(NaN, n_agent)
    costs = zeros(length(pop))

    map(1:n_gen) do generation
        for i in 1:n_agent
            agent = pop[i] = Agent(k, [])
            if generation > 1
                for i in 1:n_social
                    social_learning!(agent, sample(prev_pop, Weights(p)))
                end
            end
            fpd = FactorizationProblemDistribution(k, sample(base, pk; replace=false))
            individual_learning!(agent, fpd)
            costs[i] = total_cost(fpd, agent)
        end
        prev_pop = copy(pop)
        p .= softmax(β_learn .* costs)
        mean(costs)
    end
end

x0 = simulate(n_gen=100; n_social=0)
x1 = simulate(n_gen=100; n_social=1)
x2 = simulate(n_gen=100; n_social=2)

figure() do
    lines!(x0, label="0 model")
    lines!(x1, label="1 model")
    lines!(x2, label="2 model")
    axislegend()
end