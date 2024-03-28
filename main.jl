include("graph.jl")
include("figure.jl")
using GraphMakie
using NetworkLayout


# %% --------

env = Environment(S=5, M=8, N=100)

pop = zeros(env.S, env.S)
pop[1, 2:end] .= 1.
pop[2:end, 1] .= 1
# # pop[1, 2] = 1.
# # pop[2, 3] = 1.
# # pop[3, 4] = 1.
# # pop[4, 1] = 1.
pop .+= .1
normalize!(pop)
pop

figure() do
    g = complete_digraph(5)
    edge_width = 20 .* [pop[e.src, e.dst] for e in edges(g)]
    gp = graphplot!(g, layout=Shell(), node_size=25, arrow_size=10 .* (edge_width .^ 0.5), arrow_shift=:end; edge_width)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end

# %% --------

env = Environment(S=5, M=8, N=100, hub_discount=0.11)
monte_carlo(100) do
    sim = simulate(env, 30; init=pop)
    visit_rates = sum(sim[end]; dims=2)[:]
    hubbiness, hub = findmax(visit_rates)
    hubbiness /= (1/env.S)
end

# %% --------

env = Environment(S=5, M=8, N=100, hub_discount=0.05)
sim = simulate(env, 100)

pop = sim[end]
figure() do
    g = complete_digraph(5)
    edge_width = 20 .* [pop[e.src, e.dst] for e in edges(g)]
    gp = graphplot!(g, layout=Shell(), node_size=25, arrow_size=10 .* (edge_width .^ 0.5), arrow_shift=:end; edge_width)
    ax = current_axis()
    hidedecorations!(ax); hidespines!(ax)
    ax.aspect = DataAspect()
end

# %% --------
sim = simulate(env, 100)
initial_population(env)

g = complete_digraph(5)
fig, ax, gp = graphplot(g, layout=Shell(), node_size=25, arrow_size=ones(env.S^2-env.S), arrow_shift=:end; edge_width=ones(env.S^2-env.S))
ax = current_axis()
hidedecorations!(ax); hidespines!(ax)
ax.aspect = DataAspect()

# animation settings
framerate = 10

record(fig, "anim.mp4", sim; framerate = framerate) do pop
    edge_width = 20 .* [pop[e.src, e.dst] for e in edges(g)]
    gp.edge_width[] = edge_width
    gp.arrow_size[] = 10 .* (edge_width .^ 0.5)
    gp.arrow_size[]
end
run(`open anim.mp4`)