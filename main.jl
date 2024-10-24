include("red_black.jl")
include("r.jl")
using NamedTupleTools
using Optim

# %% --------

R"""
FIGS_PATH = "~/papers/cultural-abstractions/figures/src/"
MAKE_PDF = TRUE
"""

# %% --------

include("plot_evolution.jl")
include("plot_cost.jl")
include("plot_learning.jl")
include("plot_dynamics.jl")
include("plot_multiple_tasks.jl")