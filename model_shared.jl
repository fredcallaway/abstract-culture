using StaticArrays

include("utils.jl")

abstract type Model end

¬(p::Real) = 1 - p
prob_observe(p, k) = ¬((¬p) ^ k)

# %% ===== task-relevant information ==========================================

struct Info
    bespoke::Int  # 0: none, 1: full
    comp::Int  # 0: none, 1: partial, 2: full
end

# used for observation and action probabilities
@kwdef struct InfoRates <: FieldMatrix{2, 3, Float64}
    b0c0::Float64
    b1c0::Float64
    b0c1::Float64
    b1c1::Float64
    b0c2::Float64
    b1c2::Float64
end

Base.getindex(ir::InfoRates, info::Info) = getindex(ir, info.bespoke + 1, info.comp + 1)

# %% ===== costs ==============================================================

@kwdef struct Costs
    bespoke_zilch::Float64 = 0.
    bespoke_full::Float64 = 0.
    comp_zilch::Float64 = 0.
    comp_partial::Float64 = 0.
    comp_full::Float64 = 0.
end

function cost(C::Costs, info::Info, comp_solution::Bool)
    if comp_solution
        (C.comp_zilch, C.comp_partial, C.comp_full)[info.comp + 1]
    else
        (C.bespoke_zilch, C.bespoke_full)[info.bespoke + 1]
    end
end

function Base.show(io::IO, ::MIME"text/plain", costs::Costs)
    print(io, "Costs")
    fields = fieldnames(Costs)
    for f in fields
        val = getfield(costs, f)
        print(io, "\n  ", string(f), " = ", val)
    end
end


# %% ===== agent policy =======================================================

function agent_policy(;kws...)
    default = (
        b0c0 = 0.,  # no info
        b1c0 = 0.,
        b0c1 = 1.,  # no bespoke, half comp
        b1c1 = 0.,
        b0c2 = 1.,  # no bespoke, full comp
        b1c2 = 0.,
    )
    InfoRates(;default..., kws...)
end
bespoke_policy() = zeros(InfoRates)
comp_policy() = ones(InfoRates)

function rational_policy(C::Costs; β=1e10, ε=0.)
    map(product(0:1, 0:2)) do (bk, ck)
        info = Info(bk, ck)
        rel_cost = cost(C, info, true) - cost(C, info, false)
        p = logistic(β * -rel_cost)
        lapse(p, ε)
    end |> InfoRates
end
