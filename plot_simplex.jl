include("infinite_env.jl")
include("utils.jl")

function simplex_grid(n::Int)
    """
    Generate a grid of points over the 2-simplex (triangle).
    Returns an array of (x, y, z) tuples where x + y + z = 1.
    n: number of divisions along each edge
    """
    points = []
    step = 1.0 / n
    
    for i in 0:n
        for j in 0:(n-i)
            x = i * step
            y = j * step
            z = 1.0 - x - y
            if z >= -eps()  # ensure we're within the simplex
                push!(points, (x, y, z))
            end
        end
    end
    
    return points
end

collapse(pop::FreqPop) = (;
    bespoke_zilch = pop.bespoke_zilch,
    bespoke_full = pop.bespoke_full,
    comp = pop.comp_full + pop.comp_partial,
)

expand(x, y, z) = FreqPop(
    bespoke_zilch = x,
    bespoke_full = y,
    comp_full = z
)

# %% --------

g = grid(
    S = [5, 10],
    D = 1 .* 3 .^ (1:4),
    p_r = 1,
    p_0 = 0.0
)

dataframe(g) do prm
    env = InfiniteEnv(;prm...)
    map(simplex_grid(10)) do (x, y, z)
        pop = expand(x, y, z)
        p0 = collapse(pop)
        p1 = collapse(transition(env, pop))
        @assert sum(p1) ≈ 1 "$p1"
        (;
            bespoke_zilch = p0.bespoke_zilch,
            bespoke_full = p0.bespoke_full,
            comp = p0.comp,
            bespoke_zilch_end = p1.bespoke_zilch,
            bespoke_full_end = p1.bespoke_full,
            comp_end = p1.comp
        )
    end
end |> CSV.write("results/simplex.csv")

# run(`cd r $&$& Rscript simplex.r`)

cd("r") do
    run(`Rscript simplex.r`)
end
