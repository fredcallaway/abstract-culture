include("search.jl")

@kwdef struct LibrarySearchProblem <: AbstractProblem
    search::Search
    pd::ProblemDistribution
    init::Library = minimal_library(pd)
    options::Vector{Int} = problems(pd)
end

LibrarySearchProblem(search::Search, pd::ProblemDistribution) = LibrarySearchProblem(;search, pd)

action_type(::Type{<:LibrarySearchProblem}) = Int  # option to add/remove
state_type(::Type{<:LibrarySearchProblem}) = Library

goal_test(::LibrarySearchProblem, s) = false
cost(::LibrarySearchProblem, s, a, s1) = 0.

# library(p::LibrarySearchProblem, used) = Library(p.options[used])
quality(p::LibrarySearchProblem, lib) = -expected_cost(p.search, lib, p.pd)

initial_state(p::LibrarySearchProblem) = p.init

function transition(p::LibrarySearchProblem, lib, o)
    len = p.pd.problem_sizes[o]

    # copy everything we will change
    levels = copy(lib.levels)
    if length(levels) ≥ len
        levels[len] = copy(lib.levels[len])
    end
    lib = Library(levels)

    if has_option(lib, o, len)
        remove_option!(lib, o, len)
    else
        add_option!(lib, o, len)
    end
    lib
end

actions(p::LibrarySearchProblem, s) = p.options

# function greedy_library(search, pd; rev=true, lib=rev ? maximal_library(pd) : minimal_library(pd))
#     rev && return greedy_library_rev(search, pd, lib)
#     options = problems(pd)
#     current_cost = expected_cost(search, lib, pd)
#     while !isempty(options)
#         c, o = minimum(options) do o
#             add_option!(lib, o)
#             c = expected_cost(search, lib, pd)
#             delete!(lib, o)
#             (c, o)
#         end
#         c > current_cost && return lib
#         setdiff!(options, o)
#         add_option!(lib, o)
#     end
#     lib
# end

# function greedy_library_rev(search, pd, lib)
#     options = problems(pd)
#     lib = maximal_library(pd)
#     current_cost = expected_cost(search, lib, pd)
#     @info current_cost
#     while length(lib) > length(pd.base)
#         c, o = minimum(keys(lib)) do o
#             lib[o] == 1 && return (Inf, o)
#             lib1 = copy(lib)
#             delete!(lib1, o)
#             c = expected_cost(search, lib, pd)
#             (c, o)
#         end
#         @info c o
#         c ≥ current_cost && return lib
#         delete!(lib, o)
#     end
#     lib
# end
