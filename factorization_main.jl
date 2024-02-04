include("factorization.jl")
# %% --------

base = primes(7)
n_primitive = length(base)
pd = ProblemDistribution(Set(base), Dict())
libs = possible_libraries(pd)
wp = weighted_problems(pd)

g = grid(
    allow_lookup = [true, false],
    method = [:breadth, :best]
)

df = flatmap(g) do (;allow_lookup, method)
    flatmap(libs) do lib
        cost = expected_cost(Search(allow_lookup, method), lib, pd)
        average_length = mean(filter(!isequal(1), collect(values(lib))))
        (;allow_lookup, method=string(method), cost, average_length, n_options=length(lib) - n_primitive)
    end
end |> DataFrame
@rput df


# %% --------

R"""
df %>%
    ggplot(aes(n_options, cost, color=allow_lookup)) +
    geom_point(alpha=.01) +
    lines(f=min, min_n=1) +
    facet_wrap(~method, scales="free_y")

fig("factorization", w=5)
"""
