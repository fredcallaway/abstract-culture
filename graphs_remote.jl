using Distributed
addprocs()
@everywhere include("graph.jl")
using RemoteREPL
@async serve_repl()
println("Serving")