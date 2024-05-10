using StatsBase
using JSON
using Random

include("utils.jl")
include("data.jl")

n_start = n_goal = 4
n_chemical = n_start + n_goal + 1
n_mode = 8
n_task = 5

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

function edges_to_recipes(edges)
    map(edges) do (a, b)
        [a, transitions[a, b], b]
    end
end

zero_index(x::Int) = x - 1
zero_index(x::AbstractArray) = map(zero_index, x)
zero_index(x::Tuple) = map(zero_index, x)
zero_index(x::NamedTuple) = map(zero_index, x)


if generation == 1
    foreach(1:30) do i
        recipes = unique(sample(flat_transitions, n_observed))

        write("../machine-task/static/json/middle-$i.json", json((;
            generation,
            transitions = zero_index(transpose(transitions)),
            tasks = zero_index(sample(all_tasks, n_task, replace=false)),
            recipes = zero_index(recipes),
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
        @assert !isnothing(example)

        write("../machine-task/static/json/middle-$i.json", json((;
            generation,
            transitions = zero_index(transpose(transitions)),
            tasks = zero_index(sample(all_tasks, n_task, replace=false)),
            recipes = zero_index(recipes),
            nChemical = n_chemical,
            nMode = n_mode,
        )))
    end
end


