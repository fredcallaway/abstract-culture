using Random
using Base: @kwdef
using Serialization
using AxisKeys
using SplitApplyCombine
import Base.Iterators: product
using Statistics
using StatsBase
using DataFrames, DataFramesMeta, CSV
using Printf
using ProgressMeter
using DataStructures: DefaultDict
using Distributions
using Dates


const TOL = 1e-5

# %% ==================== Project-specific ====================


expectation(f, lo, hi) = first(quadgk(f, lo, hi))

function expectation(f::Function, d::Distribution; lo=quantile(d, TOL), hi=quantile(d, 1-TOL))
    expectation(lo, hi) do x
        pdf(d, x) * f(x)
    end
end

"Expected maximum of N samples from a Normal distribution"
function expected_maximum(k::Real, d::Normal)
    mcdf(x) = cdf(d, x)^k
    lo = d.μ - 10d.σ; hi = d.μ + 10d.σ
    - quadgk(mcdf, lo, 0, atol=1e-5)[1] + quadgk(x->1-mcdf(x), 0, hi, atol=1e-5)[1]
end

"Expected maximum of samples from any set of distributions"
function expected_maximum(dists)
    mcdf(x) = mapreduce(*, dists) do d
        cdf(d, x)
    end
    lo = maximum(dists) do d
        quantile(d, TOL)
    end
    hi = maximum(dists) do d
        quantile(d, 1-TOL)
    end
    - quadgk(mcdf, lo, 0, atol=1e-5)[1] + quadgk(x->1-mcdf(x), 0, hi, atol=1e-5)[1]
end

# %% ==================== General Purpose ====================

mutable struct LineProgress
    N::Int
    every::Int
    n::Int
    started::Float64
end

LineProgress(N::Int; every=10) = LineProgress(N, every, 0, 0.)

function ProgressMeter.next!(p::LineProgress)
    if p.n == 0
        p.started = time()
    end
    p.n += 1
    if p.n == 1 || p.n == p.N || p.n % p.every == 0
        println(p.n, "/", p.N, "   ", round(time() - p.started; digits=1), "s")
        flush(stdout)
    end
end

is_logging(io) = isa(io, Base.TTY) == false || (get(ENV, "CI", nothing) == "true")

function prog_pmap(f, xs; every=10)
    N = length(xs)
    progress = is_logging(stdout) ? LineProgress(N; every) : Progress(N)
    progress_pmap(f, xs; progress)

end

function write_result(name, val; dir="results")
    mkpath("$dir/$name")
    serialize("results/$name/$(now())", val)
end

function load_result(name, timestamp; dir="results")
    deserialize("$dir/$name/$timestamp")
end

function load_result(name; dir="results")
    timestamp = readdir("$dir/$name")[end]
    load_result(name, timestamp; dir)
end

struct GenerativeSampler{X,F}
    f::F
end
GenerativeSampler(f,X) = GenerativeSampler{X,typeof(f)}(f)
GenerativeSampler(f) = GenerativeSampler{typeof(f()),typeof(f)}(f)

Base.eltype(::Type{GenerativeSampler{X}}) where X = X

function Base.rand(rng::AbstractRNG, s::Random.SamplerTrivial{<:GenerativeSampler})
    s[].f()
end

struct SetSampler{T}
    vals::Vector{T}
    probs::Vector{Float64}
end
Base.eltype(::Type{SetSampler{T}}) where T = T

function Base.rand(rng::AbstractRNG, s::Random.SamplerTrivial{<:SetSampler})
    s = s[]
    s.vals[rand(Categorical(s.probs))]
end
SetSampler(vals) = SetSampler(vals, normalize!(ones(length(vals))))


Base.get(name::Symbol) = Base.Fix2(getproperty, name)
Base.get(i::Int) = Base.Fix2(getindex, i)
Base.get(x, name::Symbol) = getproperty(x, name)
Base.get(x, i::Int) = getindex(x, i)

getfrom(x) = Base.Fix1(get, x)

function integer_labeler(T::DataType)
    N = 0
    DefaultDict{T,Int}() do
        N += 1
        N
    end
end

flatmap(f, xs...) = mapreduce(f, vcat, xs...)

function softmax!(x)
    x .= exp.(x .- maximum(x))
    x ./= sum(x)
end
softmax(x) = softmax!(copy(x))

function softmax!(x, i)
    x .= exp.(x .- maximum(x))
    x[i] / sum(x)
end

function sliding_window(xs, k)
    map(1:length(xs) - (k-1)) do i
        @view xs[i:i+(k-1)]
    end
end

function print_header(txt; color=:magenta)
    display_width = displaysize(stdout)[2]
    n_fill = fld(display_width - length(txt) - 2, 2)
    n_space = 2
    n_dash = n_fill - n_space
    print(' '^n_space)
    printstyled('-'^n_dash; color, bold=true)
    print(' ', txt, ' ')
    printstyled('-'^n_dash; color, bold=true)
    print(' '^n_space)
    print("\n")
end

macro catch_missing(expr)
    esc(quote
        try
            $expr
        catch
            missing
        end
    end)
end

function pooled_mean_std(ns::AbstractVector{<:Integer},
                        μs::AbstractVector{<:Number},
                        σs::AbstractVector{<:Number})
    nsum = sum(ns)
    meanc = ns' * μs / nsum
    vs = replace!(σs .^ 2, NaN=>0)
    varc = sum((ns .- 1) .* vs + ns .* abs2.(μs .- meanc)) / (nsum - 1)
    return meanc, .√(varc)
end

function cache(f, file; disable=false, read_only=false, overwrite=false)
    disable && return f()
    !overwrite && isfile(file) && return deserialize(file)
    read_only && error("No cached result $file")
    result = f()
    serialize(file, result)
    result
end

function mutate(x::T; kws...) where T
    for field in keys(kws)
        if !(field in fieldnames(T))
            error("$(T.name) has no field $field")
        end
    end
    return T([get(kws, fn, getfield(x, fn)) for fn in fieldnames(T)]...)
end

function grid(;kws...)
    X = map(Iterators.product(values(kws)...)) do x
        (; zip(keys(kws), x)...)
    end
    KeyedArray(X; kws...)
end

function initialize_keyed(val; keys...)
    KeyedArray(fill(val, (length(v) for (k, v) in keys)...); keys...)
end

function wrap_counts(df::DataFrame; dims...)
    @chain df begin
        groupby(collect(keys(dims)))
        combine(nrow => :n)
        AxisKeys.populate!(initialize_keyed(0.; dims...), _, :n)
    end
end

function wrap_pivot(df::DataFrame, val, f; dims...)
    @chain df begin
        groupby(collect(keys(dims)))
        combine(val => f => :_val)
        AxisKeys.populate!(initialize_keyed(0.; dims...), _, :_val)
    end
end

macro bywrap(x, what, val, default=missing)
    arg = :(:_val = $val)
    esc(quote
        b = $(DataFramesMeta.by_helper(x, what, arg))
        what_ = $what isa Symbol ? ($what,) : $what
        wrapdims(b, :_val, what_..., sort=true; default=$default)
    end)
end

function keyed(name, xs)
    KeyedArray(xs; Dict(name => xs)...)
end

function dataframe(x::KeyedArray)
    @chain x begin
        DataFrame
        @rtransform $AsTable = :value
        select(Not(:value))
    end
end

macro require(ex)
    quote
        $ex || return missing
    end |> esc
end

function dataframe(f, params)
    flatmap(params) do x
        rows = f(x)
        @require !ismissing(rows)
        map(rows) do row
            @require !ismissing(row)
            (;x..., row...)
        end
    end |> skipmissing |> collect |> DataFrame
end

keymax(X::KeyedArray) = (; (d=>x[i] for (d, x, i) in zip(dimnames(X), axiskeys(X), argmax(X).I))...)
keymax(x::KeyedArray{<:Real, 1}) = axiskeys(x, 1)[argmax(x)]
keymin(X::KeyedArray) = (; (d=>x[i] for (d, x, i) in zip(dimnames(X), axiskeys(X), argmin(X).I))...)
keymin(x::KeyedArray{<:Real, 1}) = axiskeys(x, 1)[argmin(x)]

round1(x) = round(x; digits=1)
round2(x) = round(x; digits=2)
round3(x) = round(x; digits=3)
round4(x) = round(x; digits=4)

fmt(digits, x) = Printf.format(Printf.Format("%.$(digits)f"), x)

function Base.diff(K::KeyedArray; dims, removefirst::Bool=true)
    range = removefirst ? (2:size(K, dims)) : (1:size(K,dims)-1)
    out = similar(selectdim(K, dims, range) )
    out[:] = Base.diff(parent(parent(K)); dims=AxisKeys.dim(parent(K),dims))
    return out
end

Base.dropdims(idx::Union{Symbol,Int}...) = X -> dropdims(X, dims=idx)
squeezify(f) = (X, dims...) -> dropdims(f(X; dims); dims)
smaximum = squeezify(maximum)
sminimum = squeezify(minimum)
smean = squeezify(mean)
ssum = squeezify(sum)

safe_maximum(x; default) = isempty(x) ? default : maximum(x)
safe_maximum(f::Function, x; default) = isempty(x) ? default : maximum(f, x)

function monte_carlo(f, N=10000)
    N \ mapreduce(+, 1:N) do i
        f()
    end
end

function repeatedly(f, N=10000)
    map(1:N) do i
        f()
    end
end

linscale(x, low, high) = low + x * (high-low)
logscale(x, low, high) = exp(log(low) + x * (log(high) - log(low)))
unlinscale(x, low, high) = (x - low) / (high-low)
unlogscale(x, low, high) = (log(x) - log(low)) / (log(high) - log(low))

juxt(fs...) = x -> Tuple(f(x) for f in fs)
clip(x, lo, hi) = min(hi, max(lo, x))

nanreduce(f, x) = f(filter(!isnan, x))
nanmean(x) = nanreduce(mean, x)
nanstd(x) = nanreduce(std, x)
normalize(x) = x ./ sum(x)
normalize!(x) = x ./= sum(x)