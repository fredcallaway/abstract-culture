using Primes
using DataStructures: PriorityQueue
using Combinatorics
using Memoize

include("utils.jl")

@kwdef struct Library
    levels::Vector{Set{Int}} = []
end

function Base.display(lib::Library)
    println("Library")
    for level in lib.levels
        println(" |", join(sort(collect(level)), " "))
    end
end

function Base.show(io::IO, lib::Library)
    print(io, "Library ", join(map(length, lib.levels), " "))
end

function add_option!(lib::Library, n::Int, len=length(factor(Set, n)))
    while length(lib.levels) < len
        push!(lib.levels, Set{Int}())
    end
    push!(lib.levels[len], n)
end

function remove_option!(lib::Library, n::Int, len=length(factor(Set, n)))
    delete!(lib.levels[len], n)
end

function has_option(lib::Library, n::Int, len=length(factor(Set, n)))
    length(lib.levels) ≥ len && n in lib.levels[len]
end

Base.iterate(lib::Library) = iterate(Iterators.flatten(Iterators.reverse(lib.levels)))
Base.iterate(lib::Library, state) = iterate(Iterators.flatten(Iterators.reverse(lib.levels)), state)
Base.length(lib::Library) = sum(length, lib.levels)

function library(nums)
    lib = Library()
    for n in nums
        add_option!(lib, n)
    end
    lib
end

isdiv(num, div) = num % div == 0

function solve(library::Library, problem::Int; allow_lookup=true)
    steps = 0
    allow_lookup && problem in library && return steps
    for n in library
        if isdiv(problem, n)
            steps += 1
            problem ÷= n
            # allow_lookup && problem in keys(library) && return steps
            problem == 1 && return steps
        end
    end
    error("solve failed ", problem)
    # steps + length(factor(2*3*5))  # ASSUME no repetition
end


struct Search
    allow_lookup::Bool
    method::Symbol
end

breadth_cost(L, S) = S == 0 ? 0 : (L ^ (S+1)-1) ÷ (L-1) - 1
best_cost(L, S) = L * S

function cost(search::Search, lib::Library, problem::Int)
    S = solve(lib, problem; search.allow_lookup)
    L = length(lib)
    if search.method == :breadth
        breadth_cost(L, S)
    else @assert search.method == :best
        best_cost(L, S)
    end
end


struct ProblemDistribution
    base::Set{Int}
    frequent::Dict{Int, Float64}
    problem_sizes::Dict{Int, Int}
    problem_weights::Dict{Int, Float64}
end
Base.Broadcast.broadcastable(x::ProblemDistribution) = Ref(x)

function ProblemDistribution(base::Set{Int}, freq::Dict)
    problems = map(prod, filter(x->length(x) ≥ 2, collect(powerset(collect(base)))))
    sizes = map(problems) do n
        length(factor(Set, n))
    end
    weights = map(problems) do n
        sum(freq; init=0.) do (n, f)
            isdiv(problem, n) ? f : 0.
        end
    end
    softmax!(weights)
    ProblemDistribution(base, freq, Dict(problems .=> sizes), Dict(problems .=> weights))
end

ProblemDistribution(N::Int, freq::Dict) = ProblemDistribution(first_primes(N), freq)
ProblemDistribution(base) = ProblemDistribution(base, Dict())

function first_primes(N)
    @assert N ≥ 1
    res = [2]
    while length(res) < N
        push!(res, nextprime(res[end]+1))
    end
    Set(res)
end


problems(pd::ProblemDistribution) = collect(keys(pd.problem_sizes))
minimal_library(pd::ProblemDistribution) = library(pd.base)
maximal_library(pd::ProblemDistribution) = library(union(pd.base, problems(pd)))

function possible_libraries(pd::ProblemDistribution)
    map(powerset(problems(pd))) do options
        library(union(pd.base, options))
    end
end

function expected_cost(search::Search, lib::Library, pd::ProblemDistribution)
    sum(pd.problem_weights) do (problem, p)
        p * cost(search, lib, problem)
    end
end
