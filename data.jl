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

findnextmatch(events, key) = findnextmatch(events, 1, key)
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

@memoize function get_params(uid::String)
    e = findnextmatch(load_events(uid), "experiment.initialize")[1]
    params = isnothing(e) ? Dict() : e["PARAMS"]
end

function extract_parameters(uid::String, keys...)
    params = get_params(uid)
    Dict{String,Any}(pick_dict(params, keys))
end

function load_participants(versions...; all_generations=false, keep_incomplete=false, keep_extras=false)
    if all_generations
        versions = mapreduce(vcat, versions) do v
            filter(readdir("../machine-task/data/raw")) do v1
                startswith(v1, v)
            end
        end
        @info "versions: $versions"
    end
    versions = unique(versions)
    gen_counts = zeros(Int, 100)

    df = mapreduce(vcat, versions) do version
        idents = invert(JSON.parsefile("../machine-task/data/raw/$version/identifiers.json"))

        df = @chain CSV.read("../machine-task/data/raw/$version/participants.csv", DataFrame) begin
            @rsubset :mode == "live"
            @rtransform! @astable begin
                :version = version
                :uid = string(version, "-", :wid)
                :workerid = idents[:wid]
                events = load_events(:uid)
                :complete = !isnothing(findnextmatch(events, 1, "experiment.complete")[1])
                :total_time = (events[end]["time"] - events[1]["time"]) / 60000
            end
            select(Not([:assignmentId, :hitId, :useragent, :status, :counterbalance, :mode]))
        end
        if keep_incomplete
            return df
        end
        @subset! df :complete
        if version ≥ "vM5"
            df = @chain df begin
                @rtransform! $AsTable = extract_parameters(:uid, "generation", "pop_name", "pop_id")
                @rtransform! @astable begin
                    pn = :pop_name
                    env = deserialize("envs/$pn")
                    :N = env.N
                    :M = env.M
                    :K = env.K
                    :population = string(:pop_name, "-", :pop_id)
                end
                @orderby :population :generation
                @groupby :population :generation
                @transform :agent_i = 1:length(:generation)
            end
        else
            @rtransform! df begin
                :generation = get_params(:uid)["generation"]
                :M = 20
                :N = 10
                :K = 5
            end
            @transform! df :agent_i = 1:length(:version)
        end
        if startswith(version, "vM4")
            @rtransform! df :population = startswith(version, "vM4.1") ? 2 : 1
        end
        if !keep_extras
            @rsubset! df :agent_i ≤ :N
        end
        df
    end
    @transform df :pid = 1:nrow(df)
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
    g = group(e->get(e, "trialID", ""), events[start_main:end])
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

        if startswith(uid, "vM2g1") && goal == 8 && length(trial_events) == 4
            @warn "H trial"
            return missing
        end


        path = @infiltry a_star(g, start, goal)


        start_time = trial_events[1]["time"]
        end_time = trial_events[end]["time"]

        Trial(uid, trial_number, start, goal, knowledge, traversed, path, attempts, start_time, end_time)
    end |> skipmissing |> collect
end
