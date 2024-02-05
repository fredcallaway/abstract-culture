include("factorization.jl")
include("r.jl")

# %% ==================== plot all libraries ====================

N = 4

function fmt_lib(lib::Library)
    levels = map(lib.levels[2:end]) do lev
        join(sort(collect(lev)), " ")
    end
    join(levels, " | ")
end

g = grid(
    structured = [true, false],
    allow_lookup = [true, false],
    method = [:breadth, :best]
)

df = dataframe(g) do (;allow_lookup, method, structured)
    freqs = structured ? Dict(35 => 1., 6 => 1.) : Dict()
    pd = ProblemDistribution(N, freqs)
    libs = possible_libraries(pd)

    flatmap(libs) do lib
        cost = expected_cost(Agent(allow_lookup, method), lib, pd)
        average_length = mean(filter(!isequal(1), collect(values(lib))))
        (;lib=fmt_lib(lib), method=string(method),
          cost, average_length, n_options=length(lib) - N)
    end
end
@rput df



R"""
plot_cost = function(data) {
    data %>%
        ggplot(aes(n_options, cost, color=allow_lookup)) +
        geom_point(alpha=.01) +
        lines(f=min, min_n=1) +
        facet_wrap(~method, scales="free_y")
}


df %>% filter(!structured) %>%  plot_cost
fig("factorization_costs", w=5)

df %>% filter(structured) %>%  plot_cost
fig("factorization_costs_structured", w=5)

"""

# %% ==================== optimization ====================
include("library_optimization.jl")

# %% --------

N = 5
pd = ProblemDistribution(N, Dict())

srch = Search(true, :best)
lsp = LibrarySearchProblem(srch, pd)
node = search(BestFirst(), lsp; max_iter=10000).best

expected_cost(srch, maximal_library(pd), pd)


# %% --------

lib = maximal_library(pd)

for k in sample(collect(setdiff(keys(lib), pd.base)), 4; replace=false)
    delete!(lib, k)
end
lib
expected_cost(search, lib, pd)
expected_cost(search, maximal_library(pd), pd)



greedy_library(search, pd; lib, rev=true)