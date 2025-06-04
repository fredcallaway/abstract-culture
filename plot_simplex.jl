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



# %% --------
using Roots
function find_stable_points(env::InfiniteEnv; atol=1e-5)
    stable = find_zeros(0, 1) do comp
        comp2 = transition(env, CompPop(comp)).comp
        comp2 - comp
    end
    @show stable

    i = findfirst(stable) do x
        x ≈ 1 && return false
        x += 1e-8
        transition(env, CompPop(x)).comp > x
    end

    start = if isnothing(i)
        if transition(env, CompPop(0.)).comp > 0
            0.
        else
            NaN
        end
    else
        stable[i]
    end

    i = findlast(stable) do x
        x ≈ 0 && return false
        transition(env, CompPop(x - atol)).comp > x - atol && transition(env, CompPop(x + atol)).comp < x + atol
    end

    stop = if isnothing(i)
        NaN
    else
        stable[i]
    end

    (;start, stop)
end

find_stable_points(;params...) = find_stable_points(InfiniteEnv(;params...))

# %% --------

env = InfiniteEnv(;S=5, D=15, p_0=1e-5, p_r=0.5)
find_stable_points(env)
p = transition(env, CompPop(0.))
transition(env, p)

# %% --------

g = grid(
    S = [5, 10],
    D = 1 .* 3 .^ (1:4),
    p_r = 1,
    p_0 = 0.0
)

dataframe(g) do prm
    env = InfiniteEnv(;prm...)
    map(simplex_grid(10)) do (indiv, bespoke, comp)
        pop = FreqPop(
            bespoke_full = bespoke,
            comp_full = comp,
            comp_zilch = indiv * env.p_0,
            bespoke_zilch = indiv * (1 - env.p_0),
            # note: comp_partial doesn't affect evolution
        )
        pop1 = transition(env, pop)
        
        (;
            indiv, bespoke, comp,
            indiv_end = pop1.bespoke_zilch + pop1.comp_zilch,
            bespoke_end = pop1.bespoke_full,
            comp_end = pop1.comp_full + pop1.comp_partial
        )
    end
end |> CSV.write("results/simplex.csv")

# run(`cd r $&$& Rscript simplex.r`)

cd("r") do
    run(`Rscript simplex.r`)
end
