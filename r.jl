using RCall

cd("r") do
    R"""source("base.r")"""
end

function run_rscript(script::String)
    cd("r") do
        R"""source($script)"""
    end
end