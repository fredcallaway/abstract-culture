using DataStructures

abstract type AbstractProblem end

action_type(::Type{<:AbstractProblem}) = error("action_type unimplemented")
state_type(::Type{<:AbstractProblem}) = error("state_type unimplemented")
initial_state(::AbstractProblem) = error("initial_state unimplemented")
transition(::AbstractProblem, s, a) = error("transition unimplemented")
actions(::AbstractProblem, s) = error("actions unimplemented")
goal_test(::AbstractProblem, s) = error("goal_test unimplemented")
cost(::AbstractProblem, s, a, s1) = 1  # default action cost
quality(::AbstractProblem, s) = 0.  # all (partial) solutions equally good by default

action_type(p::AbstractProblem) = action_type(typeof(p))
state_type(p::AbstractProblem) = state_type(typeof(p))


Base.Broadcast.broadcastable(x::AbstractProblem) = Ref(x)

function simulate(prob::AbstractProblem, acts::Vector)
    states = state_type(prob)[]
    rewards = Float64[]
    s = initial_state(prob)
    foreach(acts) do a
        push!(states, s)
        s1 = transition(prob, s, a)
        r = reward(prob, s, a, s1)
        push!(rewards, r)
        s = s1
    end
    states, rewards
end

using DataStructures: BinaryMinHeap


struct Node{S,A}
    s::S
    g::Float64  # cost up to this node
    q::Float64  # how good of a partial solution is this
    f::Float64  # search priority
    parent::Union{Missing,Node{S,A}}
    a::Union{Missing,A}
end
Base.isless(n1::Node, n2::Node) = n1.f < n2.f
Base.show(io::IO, n::Node) = print(io, summary(n))


uniqueid(x) = x  # can be overwritten if necesary for certain problems

# f is callable, e.g. a function
function search(f, prob::AbstractProblem; max_iter=Int(1e6), max_cost=Inf, logger=nothing, σ=0.0, track_seen=true, stop_on_goal=true)
    S = state_type(prob)
    A = action_type(prob)

    s0 = initial_state(prob)
    n0 = Node{S,A}(s0, 0., quality(prob, s0), NaN, missing, missing)

    best = n0
    solved = goal_test(prob, n0.s)
    frontier = BinaryMinHeap{Node{S,A}}()

    K = typeof(uniqueid(s0))
    seen = Dict{K,Float64}()  # cost to get to each state we've seen

    push!(frontier, n0)
    steps = 0
    while !isempty(frontier)
        n = pop!(frontier)

        if track_seen
            uid = uniqueid(n.s)
            if uid in keys(seen) && n.g ≥ seen[uid]
                # we've gotten here before with equal or less cost
                continue
            else
                seen[uid] = n.g
            end
        end

        isnothing(logger) || logger(n, steps, seen, best, frontier)

        if goal_test(prob, n.s)
            if !solved || n.q - n.g > best.q - best.g
                best = n
            end
            solved = true
            if stop_on_goal
                break
            else
                continue  # don't expand
            end
        elseif !solved && n.q - n.g > best.q - best.g
            best = n
        end


        for a in actions(prob, n.s)
            s1 = transition(prob, n.s, a)
            isnothing(s1) && continue
            g1 = n.g + cost(prob, n.s, a, s1)
            g1 > max_cost && continue  # don't add to frontier
            q1 = float(quality(prob, s1))
            # q1 == -Inf && continue  # -Inf quality means impossible to win; don't add to frontier
            priority = float(f(prob, s1, g1, q1))
            if σ > 0
                priority += σ * randn()
            end
            n1 = Node(s1, g1, q1, priority, n, a)
            push!(frontier, n1)
        end
        steps += 1
        steps ≥ max_iter && break
    end
    return (;best, solved, steps)
end

function build_plan(n::Node{S,A}) where {S,A}
    plan = A[]
    while !ismissing(n.a)
        push!(plan, n.a)
        n = n.parent
    end
    reverse!(plan)
end

function show_plan(prob, plan)
    s = initial_state(prob)
    show(s)
    for a in plan
        s = transition(prob, s, a)
        show(s)
    end
end

struct RandomSearch <: Function end
(f::RandomSearch)(prob, s, g, q) = rand()

struct BestFirst <: Function end
(f::BestFirst)(prob, s, g, q) = g - q + 1e-10rand()

struct NoisyBestFirst <: Function
    σ::Float64
end
(f::NoisyBestFirst)(prob, s, g, q) = g - q + f.σ * randn()

struct GreedyBestFirst <: Function end
(f::GreedyBestFirst)(prob, s, g, q) = -q + 1e-10rand()

struct BreadthFirst <: Function end
(f::BreadthFirst)(prob, s, g, q) = g + 1e-10rand()

# ASSUME: unit step costs!
struct DepthFirst <: Function end
(f::DepthFirst)(prob, s, g, q) = -g + 1e-10rand()

struct AStar{F<:Function} <: Function
    h::F
end
(f::AStar)(prob, s, g, q) = g + f.h(prob, s)


function expansion_tracked_search(f, prob::AbstractProblem; kws...)
    expansion_log = Node[]
    logger(n, steps, seen, best, frontier) = begin
        push!(expansion_log, n)
    end
    result = search(f, prob; logger, kws...)
    (;result..., expansion_log)
end

function quality_tracked_search(f, prob::AbstractProblem; kws...)
    quality_log = Float64[]
    logger(n, steps, seen, best, frontier) = begin
        push!(quality_log, best.q)
    end
    result = search(f, prob; logger, kws...)
    (;result..., quality_log)
end

plot_state(n::Node) = plot_state(n.s)
