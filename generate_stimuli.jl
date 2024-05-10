using StatsBase
using JSON
using Random

include("utils.jl")
include("data.jl")

n_chemical = 6
n_mode = 8
n_task = 10

generation = 10


Random.seed!(hash("v4.0"))

transitions = rand(1:n_mode, (n_chemical, n_chemical))
for c in 1:n_chemical
    transitions[c, c] = 0
end

all_tasks = filter(collect(Iterators.product(1:n_chemical, 1:n_chemical))[:]) do (a, b)
    a != b
end

flat_transitions = map(all_tasks) do (a, b)
    [a, transitions[a, b], b]
end


function rejection_sample(generate, condition; max_try=10000)
    for i in 1:max_try
        x = generate()
        condition(x) && return x
    end
    error("Hit max_try")
end

function valid_example(recipes)
    chem1, mode, chem2 = recipes[1]
    !any(isequal(mode), transitions[chem2, :])
end

function choose_example(recipes)
    chem1, mode, chem2 = recipes[1]
    options = findall(x-> x ∉ (mode, 0), transitions[chem2, :])
    isempty(options) && return nothing
    chem3 = rand(options)
    @assert 1 ≤ chem1 ≤ n_chemical
    @assert 1 ≤ chem2 ≤ n_chemical
    @assert 1 ≤ chem3 ≤ n_chemical
    @assert transitions[chem2, chem3] != mode
    (;chem1, mode, chem2, chem3)
end

function edges_to_recipes(edges)
    map(edges) do (a, b)
        [a, transitions[a, b], b]
    end
end

function sample_recipes(type)
    if type == "random"
        sample(flat_transitions, 2*(n_chemical-1), replace=false)
    elseif type == "ring"
        ring = randperm(n_chemical)
        edges = map(1:n_chemical) do i
            (ring[i], ring[mod1(i+1, n_chemical)])
        end
        shuffle(edges_to_recipes(edges))
    elseif type == "hub"
        hub, others... = shuffle(1:n_chemical)
        edges = mapreduce(vcat, others) do o
            [(hub, o), (o, hub)]
        end
        shuffle(edges_to_recipes(edges))
    end
end


zero_index(x::Int) = x - 1
zero_index(x::AbstractArray) = map(zero_index, x)
zero_index(x::Tuple) = map(zero_index, x)
zero_index(x::NamedTuple) = map(zero_index, x)


if generation == 1
    info_types = ["random"]

    foreach(1:30) do i
        information_type = info_types[mod1(i, length(info_types))]
        recipes = rejection_sample(valid_example) do
            sample_recipes(information_type)
        end
        @assert all(recipes) do r
            r in flat_transitions &&
            transitions[r[1], r[3]] == r[2]
        end

        write("../machine-task/static/json/$i.json", json((;
            transitions = zero_index(transpose(transitions)),
            tasks = zero_index(sample(all_tasks, n_task, replace=false)),
            recipes = zero_index(recipes),
            nChemical = n_chemical,
            nMode = n_mode,
            information_type
        )))
    end
else
    prev_gen = generation - 1
    participants = load_participants("v4.0g$prev_gen")
    # participants = load_participants("v4.0g4")
    @rsubset! participants :generation == prev_gen
    uids = participants.uid
    @show length(uids)
    @assert length(uids) ≥ 9
    trials = mapreduce(load_trials, vcat, uids)

    solutions = map(trials) do t
        Tuple.(t.path)
    end

    Random.seed!(hash("v4.0g$generation"))
    foreach(1:30) do i
        recipes = edges_to_recipes(unique(reduce(vcat, sample(solutions, 10, replace=false))))
        example = choose_example(recipes)
        @assert !isnothing(example)

        write("../machine-task/static/json/$i.json", json((;
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


