include("infinite_model.jl")
include("utils.jl")

function simplex_grid(n::Int)
    """
    Generate a grid of points over the 2-simplex (triangle).
    Returns an array of (x, y, z) tuples where x + y + z = 1.
    n: number of divisions along each edge
    """
    points = Vector{Float64}[]
    step = 1.0 / n
    
    for i in 0:n
        for j in 0:(n-i)
            x = i * step
            y = j * step
            z = 1.0 - x - y
            if z >= -eps()  # ensure we're within the simplex
                push!(points, [x, y, z])
            end
        end
    end
    
    return points
end



# %% --------
using Roots
function find_stable_points(env::InfiniteModel; atol=1e-5)
    fixed = find_zeros(0, 1) do comp
        comp2 = transition(env, CompPop(comp)).comp
        comp2 - comp
    end

    filter(fixed) do comp
        δ = 1e-5
        more = CompPop(comp + δ)
        less = CompPop(comp - δ)
        (more.comp > 1. || transition(env, more) < more) && 
        (less.comp < 0. || transition(env, less) > less)
    end


    # i = findfirst(stable) do x
    #     x ≈ 1 && return false
    #     x += 1e-8
    #     transition(env, CompPop(x)).comp > x
    # end

    # start = if isnothing(i)
    #     if transition(env, CompPop(0.)).comp > 0
    #         0.
    #     else
    #         NaN
    #     end
    # else
    #     stable[i]
    # end

    # i = findlast(stable) do x
    #     x ≈ 0 && return false
    #     transition(env, CompPop(x - atol)).comp > x - atol && transition(env, CompPop(x + atol)).comp < x + atol
    # end

    # stop = if isnothing(i)
    #     NaN
    # else
    #     stable[i]
    # end

    # (;start, stop)
end

find_stable_points(;params...) = find_stable_points(InfiniteModel(;params...))

# %% --------

function find_stable_points_3d(env::InfiniteModel)
    stable = find_stable_points(env)
    map(filter(!isnan, collect(stable))) do x
        pop = @infiltry transition(env, FreqPop(CompPop(x)))
        pop1 = transition(env, pop)
        @assert isapprox(pop1, pop, atol=1e-3) "\n$pop1\n$pop"
        Pop3(pop1)
    end
end

# %% --------


# g = grid(
#     S = 10,
#     D = [5, 30, 150, 500],
#     p_r = 1e-5,
#     p_0 = 1e-5,
# )

# g = grid(
#     S = 10,
#     D = [5, 50, 150, 500],
#     p_r = 1.,
#     p_0 = 0.,
# )

# g = grid(
#     S = [5, 10],
#     D = 2 .^ (2:5),
#     p_r = 1.,
#     p_0 = 0.01
# )


dataframe(g) do prm
    env = InfiniteModel(;prm...)
    fixed_points = find_stable_points_3d(env)
    grid_points = Pop3.(simplex_grid(10))
    filter!(grid_points) do pop
        !any(fixed_points) do fixed_point
            all(abs.(fixed_point .- pop) .< .05)
        end
    end
    points = vcat(fixed_points, grid_points)
    is_fixed = vcat( fill(true, length(fixed_points)), fill(false, length(grid_points)))

    map(points, is_fixed) do (indiv, bespoke, comp), is_fixed
        pop = Pop3(;indiv, bespoke, comp)
        pop1 = transition(env, pop)
        (;
            indiv, bespoke, comp,
            indiv_end = pop1.indiv,
            bespoke_end = pop1.bespoke,
            comp_end = pop1.comp,
            is_fixed
        )
    end
end |> CSV.write("results/simplex.csv")

# run(`cd r $&$& Rscript simplex.r`)

using RCall
cd("r") do
    R"""source("simplex.r")"""
    # run(`Rscript simplex.r`)
end
