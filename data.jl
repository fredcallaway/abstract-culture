using JSON
using Memoize
using Graphs
include("allsimplepaths.jl")
ematch(e, key::String) =  startswith(e["event"], key)
ematch(e, keys) = any(key -> ematch(e, key), keys)
ematch(key::String) = Base.Fix2(ematch, key)

function workerid(uid)
    v, w = split(uid, "-")
    invert(JSON.parsefile("../machine-task/data/raw/$v/identifiers.json"))[w]
end


findallmatch(uid::String, key) = findallmatch(load_events(uid), key)
function findallmatch(events, key)
    filter(eachindex(events)) do i
        ematch(events[i], key)
    end
end

function findnextmatch(events, start, key)
    i = findnext(e -> ematch(e, key), events, start)
    isnothing(i) && return (nothing, nothing)
    events[i], i
end

function findprevmatch(events, start, key)
    i = findprev(e -> ematch(e, key), events, start)
    isnothing(i) && return (nothing, nothing)
    events[i], i
end

filtermatch(uid::String, key) = filtermatch(load_events(uid), key)
function filtermatch(events, key)
    filter(events) do e
        ematch(e, key)
    end
end

function load_participants(versions...)
    gen_counts = zeros(Int, 100)
    mapreduce(vcat, versions) do version
        @chain CSV.read("../machine-task/data/raw/$version/participants.csv", DataFrame) begin
            @rsubset :mode == "live"
            @rtransform! @astable begin
                :version = version
                :uid = string(version, "-", :wid)
                events = load_events(:uid)

                :complete = !isnothing(findnextmatch(events, 1, "experiment.complete")[1])
                :generation = missing


                :information_type = if :complete
                    params = findnextmatch(events, 1, "experiment.initialize")[1]["PARAMS"]
                    get(params, "information_type", missing)
                else
                    missing
                end



#                 try
#                     init = findnextmatch(events, 1, "experiment.initialize")[1]
#                     :generation = @coalesce begin
#                         get(init["PARAMS"], "generation", missing)
#                         get(init["stimuli"], "generation", missing)
#                     end
#                 catch end
# -
#                 if :complete && !ismissing(:generation)
#                     gen_counts[:generation] += 1
#                     :pid = string("g", :generation, "-", lpad(gen_counts[:generation], 2, "0"))
#                 else
#                     gen_counts[100] += 1
#                     :pid = string("p", lpad(gen_counts[100], 2, "0"))
#                 end

                :total_time = begin
                    events = load_events(:uid)
                    (events[end]["time"] - events[1]["time"]) / 60000
                end
            end
            select(Not([:assignmentId, :hitId, :useragent, :status, :counterbalance, :mode]))
            @rsubset :complete
            @transform _ :pid = 1:nrow(_)
        end
    end
end

@memoize function load_events(uid; normalize_time=true)
    version, wid = rsplit(uid, "-"; limit=2)
    events = JSON.parsefile("../machine-task/data/raw/$version/events/$wid.json")
    if normalize_time
        start = events[1]["time"]
        for e in events
            if haskey(e, "time")
                e["time"] -= start
            end
        end
    end
    events
end

struct Trial
    uid::String
    trial_number::Int
    start::Int
    goal::Int
    knowledge::Vector{Edge{Int}}
    traversed::Vector{Edge{Int}}
    path::Vector{Edge{Int}}
    attempts::Vector
    start_time::Int
    end_time::Int
    # event_indices::Tuple{Int, Int}
end

function Base.show(io::IO, t::Trial)
    print(io, typeof(t), "($(t.uid), $(t.trial_number), ...)")
end

function n_step(t::Trial)
    length(t.path) - 1
end

duration(t::Trial) = t.end_time - t.start_time

@memoize function load_trials(uid)
    events = load_events(uid)
    start_main = findnextmatch(events, 1, "timeline.start.main")[2]
    g = group(e->get(e, "trialId", ""), events[start_main:end])
    delete!(g, "")
    map(enumerate(g)) do (trial_number, trial_events)
        start = trial_events[1]["start"] + 1
        goal = trial_events[1]["goal"] + 1

        knowledge = map(trial_events[1]["recipes"]) do (a, _, b)
            Edge{Int}(a+1, b+1)
        end

        traversed = map(filtermatch(trial_events, "machine.result")) do e
            @require haskey(e, "result") && !isnothing(e["result"])
            Edge{Int}((e["chemical"]+1, e["result"]+1))
        end |> skipmissing |> collect


        attempts = map(findallmatch(trial_events, "machine.result")) do i
            e = trial_events[i]
            e_target = findprevmatch(trial_events, i, "machine.activateTarget")[1]
            @require !isnothing(e_target)
            target = parse(Int, e_target["event"][end])
            result = get(e, "result", nothing)
            if !isnothing(result)
                result += 1
            end
            (;src=e["chemical"]+1, dst=target+1, mode=e["mode"]+1, result)
        end |> skipmissing |> collect

        # attempts = map(filtermatch(trial_events, "machine.result")) do e
        #     (e["chemical"]+1, e["spell"]+1, haskey(e, "result") ? e["result"] + 1 : nothing)
        # end

        stock = Set{Int}(start)
        for a in attempts
            if a.src ∉ stock
                @warn "cheater: $uid"
                return missing
            elseif !isnothing(a.result)
                push!(stock, a.result)
            end
        end


        g = SimpleDiGraphFromIterator(traversed)

        path = @infiltry a_star(g, start, goal)



        start_time = trial_events[1]["time"]
        end_time = trial_events[end]["time"]

        Trial(uid, trial_number, start, goal, knowledge, traversed, path, attempts, start_time, end_time)
    end |> skipmissing |> collect
end
