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
    comp = compositional_rate(pop)
)

expand(x, y, z) = FreqPop(
    bespoke_zilch = x,
    bespoke_full = y,
    comp_full = z
)

env = InfiniteEnv(S=5, D=15, p_0 = 0.0, p_r = 1.0)

# Generate arrow data for vector field
arrow_data = map(simplex_grid(10)) do (x, y, z)
    pop = expand(x, y, z)
    p0 = collapse(pop)
    p1 = collapse(transition(env, pop))
    (;
        bespoke_zilch = p0.bespoke_zilch,
        bespoke_full = p0.bespoke_full,
        comp = p0.comp,
        bespoke_zilch_end = p1.bespoke_zilch,
        bespoke_full_end = p1.bespoke_full,
        comp_end = p1.comp
    )
end

DataFrame(arrow_data) |> CSV.write("results/simplex.csv")

# %% --------

sim = imap(simulate(env, 10, init=.01)) do gen, pop
    (;
        gen,
        pop.bespoke_zilch,
        pop.bespoke_full,
        comp = compositional_rate(pop)
    )
end

DataFrame(sim) |> CSV.write("results/chains.csv")
