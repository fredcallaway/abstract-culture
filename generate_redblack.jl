using StatsBase
using JSON
using Random

include("utils.jl")
include("data.jl")

n_start = n_goal = 4
n_chemical = n_start + n_goal + 1
n_mode = 8
n_task = 10

n_observed = 8
generation = 1


Random.seed!(hash("vR1.0"))

transitions = zeros(Int, n_chemical, n_chemical)

for c1 in 1:(n_start+1)
    for c2 in (n_start+1):n_chemical
        if c1 ≠ c2
            transitions[c1, c2] = rand(1:n_mode)
        end
    end
end

all_tasks = filter(collect(Iterators.product(1:n_chemical, 1:n_chemical))[:]) do (a, b)
    transitions[a, b] != 0
end

flat_transitions = map(all_tasks) do (a, b)
    [a, transitions[a, b], b]
end

function choose_example(recipes)
    for (chem1, mode, chem2)  in recipes
        options = filter(flat_transitions) do (a, m, b)
            a == chem1 && m != mode && b != chem2
        end
        isempty(options) && continue
        chem3 = rand(options)[end]
        @assert 1 ≤ chem1 ≤ n_chemical
        @assert 1 ≤ chem2 ≤ n_chemical
        @assert 1 ≤ chem3 ≤ n_chemical
        @assert transitions[chem2, chem3] != mode
        return (;chem1, mode, chem2, chem3)
    end
    error("no example")
end

function edges_to_recipes(edges)
    map(edges) do (a, b)
        [a, transitions[a, b], b]
    end
end

function sample_recipes(type="random")
    sample(flat_transitions, 2*(n_chemical-1), replace=false)
end




zero_index(x::Int) = x - 1
zero_index(x::AbstractArray) = map(zero_index, x)
zero_index(x::Tuple) = map(zero_index, x)
zero_index(x::NamedTuple) = map(zero_index, x)


if generation == 1
    foreach(1:30) do i
        recipes = unique(sample(flat_transitions, n_observed))
        example = choose_example(recipes)

        write("../machine-task/static/json/middle-$i.json", json((;
            generation,
            transitions = zero_index(transpose(transitions)),
            tasks = zero_index(sample(all_tasks, n_task, replace=false)),
            recipes = zero_index(recipes),
            example = zero_index(example),
            nChemical = n_chemical,
            nMode = n_mode,
        )))
    end
else
    participants = load_participants("v4.0")
    @rsubset! participants :generation == generation - 1
    uids = participants.uid
    trials = mapreduce(load_trials, vcat, uids)

    solutions = map(trials) do t
        Tuple.(t.path)
    end

    Random.seed!(hash("vR1.0$generation"))
    foreach(1:30) do i
        recipes = edges_to_recipes(unique(reduce(vcat, sample(solutions, 10, replace=false))))
        example = choose_example(recipes)
        @assert !isnothing(example)

        write("../machine-task/static/json/middle-$i.json", json((;
            generation,
            transitions = zero_index(transpose(transitions)),
            tasks = zero_index(sample(all_tasks, n_task, replace=false)),
            recipes = zero_index(recipes),
            example = zero_index(example),
            nChemical = n_chemical,
            nMode = n_mode,
        )))
    end
end


