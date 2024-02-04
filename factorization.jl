using Primes
using DataStructures: PriorityQueue
using Combinatorics

include("utils.jl")
include("r.jl")


Library = PriorityQueue{Int, Int}

function add_option!(lib::Library, o::Int)
    lib[o] = length(factor(Set, o))
end


function library(nums)
    lib = Library(Base.Order.Reverse)
    for n in nums
        add_option!(lib, n)
    end
    lib
end

isdiv(num, div) = num % div == 0

function solve(library::Library, problem::Int; allow_lookup=true)
    steps = 0
    allow_lookup && problem in keys(library) && return steps
    for n in keys(library)
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
end
ProblemDistribution(base) = ProblemDistribution(base, Dict())
Base.Broadcast.broadcastable(x::ProblemDistribution) = Ref(x)

function problems(pd::ProblemDistribution)
    map(prod, filter(x->length(x) ≥ 2, collect(powerset(collect(pd.base)))))
end

function possible_libraries(pd::ProblemDistribution)
    map(powerset(problems(pd))) do options
        library(union(pd.base, options))
    end
end

function score(pd::ProblemDistribution, problem::Int)
    sum(pd.frequent; init=0.) do (n, f)
        isdiv(problem, n) ? f : 0.
    end
end

function weighted_problems(pd::ProblemDistribution)
    probs = problems(pd)
    p = score.(pd, probs)
    softmax!(p)
    Dict(probs .=> p)
end
