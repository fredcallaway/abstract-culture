push!(ARGS, "")
versions = [n for n in readdir("data") if startswith(n, "reg-v2")]
for version in versions
    ARGS[1] = version
    include("preprocess_codemachine.jl")
end