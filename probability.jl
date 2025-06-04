# using Quadgk

const EXPECTATION_TOL = 1e-5
expectation(f, lo, hi) = first(quadgk(f, lo, hi))

function expectation(f::Function, d::Distribution; lo=quantile(d, EXPECTATION_TOL), hi=quantile(d, 1-EXPECTATION_TOL))
    expectation(lo, hi) do x
        pdf(d, x) * f(x)
    end
end

function expectation(f::Function, d::DiscreteDistribution)
    sum(support(d)) do x
        pdf(d, x) * f(x)
    end
end

"Expected maximum of N samples from a Normal distribution"
function expected_maximum(k::Real, d::Normal)
    mcdf(x) = cdf(d, x)^k
    lo = d.μ - 10d.σ; hi = d.μ + 10d.σ
    - quadgk(mcdf, lo, 0, atol=EXPECTATION_TOL)[1] + quadgk(x->1-mcdf(x), 0, hi, atol=EXPECTATION_TOL)[1]
end

"Expected maximum of samples from any set of distributions"
function expected_maximum(dists)
    mcdf(x) = mapreduce(*, dists) do d
        cdf(d, x)
    end
    lo = maximum(dists) do d
        quantile(d, EXPECTATION_TOL)
    end
    hi = maximum(dists) do d
        quantile(d, 1-EXPECTATION_TOL)
    end
    - quadgk(mcdf, lo, 0, atol=EXPECTATION_TOL)[1] + quadgk(x->1-mcdf(x), 0, hi, atol=EXPECTATION_TOL)[1]
end