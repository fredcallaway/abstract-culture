include("utils.jl")
using Distributions

"""
Creates test parameter combinations for all edge cases and random values within given ranges.
For each parameter, includes both edge values and n_rand random values.
Returns an array of named tuples with (2+n_rand)^K entries where K is the number of parameters.
Each random value is freshly sampled for each specific combination.
"""

function create_test_objects(constructor, ranges::NamedTuple; n_rand::Int=3)
    # Create parameter value generators for each field
    @time param_generators = map(ranges) do range
        if range isa Tuple && length(range) == 2
            low, high = range
            if low isa Integer && high isa Integer
                # Integer parameters: edge cases plus random generators
                [() -> low, () -> high, (() -> rand(low:high) for _ in 1:n_rand)...]
            else
                # Float parameters: edge cases plus random generators  
                [() -> low, () -> high, (() -> low + rand() * (high - low) for _ in 1:n_rand)...]
            end
        else
            # Discrete values: create constant functions
            [() -> v for v in collect(range)]
        end
    end

    # Create all combinations using the existing grid function
    @time generator_grid = grid(; zip(keys(ranges), param_generators)...)
    
    # Apply each generator combination to create actual parameter values
    @time parameter_combinations = map(generator_grid) do generators
        map(f -> f(), generators)
    end
    
    # Apply constructor to each combination
    @time map(constructor, parameter_combinations)
end 