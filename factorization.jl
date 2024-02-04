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

function solve(problem::Int, library::Library; allow_lookup=true)
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

# %% --------

breadth_cost(L, S) = S == 0 ? 0 : (L ^ (S+1)-1) / (L-1) - 1
best_cost(L, S) = L * S


base = primes(7)
n_primitive = length(base)
pd = ProblemDistribution(Set(base), Dict())
libs = possible_libraries(pd)
wp = weighted_problems(pd)

df = flatmap([true, false]) do allow_lookup
    flatmap(libs) do lib
        breadth, best = sum(wp) do (problem, p)
            S = solve(problem, lib; allow_lookup)
            L = length(lib)
            p .* [breadth_cost(L,S), best_cost(L,S)]
        end
        average_length = mean(filter(!isequal(1), collect(values(lib))))
        (;allow_lookup, breadth, best, average_length, n_options=length(lib) - n_primitive)
    end
end |> DataFrame
@rput df


# %% --------

R"""
df %>%
    pivot_longer(c(breadth, best), names_to="strategy", values_to="cost", names_prefix="") %>%
    ggplot(aes(n_options, cost, color=allow_lookup)) +
    geom_point(alpha=.01) +
    lines(f=min, min_n=1) +
    facet_wrap(~strategy, scales="free_y")

fig("factorization", w=5)
"""
