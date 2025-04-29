using JSON
using Memoize

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
    isnothing(e) ? Dict() : e["PARAMS"]
end

function extract_parameters(uid::String, keys...)
    params = get_params(uid)
    Dict{String,Any}(pick_dict(params, keys))
end

@memoize function load_participants(versions...; all_generations=false, keep_incomplete=false, keep_failed=false,keep_extras=false)
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
            # @rsubset :mode == "live"
            @rtransform! @astable begin
                :version = version
                :uid = string(version, "-", :wid)
                # :workerid = idents[:wid]
                events = load_events(:uid)
                :complete = !isnothing(findnextmatch(events, 1, "experiment.complete")[1])
                :failed = !isnothing(findnextmatch(events, 1, "experiment.terminate")[1])
                :total_time = (events[end]["time"] - events[1]["time"]) / 60000
            end
        end
        if !keep_incomplete
            @subset! df :complete
        end
        if !keep_failed
            @warn "removing $(sum(df.failed)) failed participants"
            @rsubset! df ! :failed
        end
        # see old-data.jl for cut code
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
    uid::String  # participant unique identifier
    trial_number::Int  # 1 is the first main trial (0 is last instruction trial)
    events::Vector{Dict{String,Any}}
end
trial_id(t::Trial) = t.events[1]["trialID"]
is_practice(t::Trial) = occursin("instruct", trial_id(t))
is_catch(t::Trial) = occursin("catch", trial_id(t))
# is_practice(t::Trial) = t.trial_number < 1

function Base.show(io::IO, t::Trial)
    print(io, typeof(t), "($(t.uid), $(t.trial_number), ...)")
end

# function n_step(t::Trial)
#     length(t.path) - 1
# end

start_time(t::Trial) = t.events[1]["time"]
end_time(t::Trial) = t.events[end]["time"]
duration(t::Trial) = end_time(t) - start_time(t)

@memoize function load_trials(uid)
    events = load_events(uid)
    g = group(e->get(e, "trialID", ""), events)  # maintains order
    delete!(g, "")
    # n_instruct = sum(trial_id -> startswith(trial_id, "instruct"), keys(g))
    n_instruct = 3
    map(enumerate(g)) do (trial_number, trial_events)
        # if trial_number ≤ n_instruct
        #     for t in trial_events
        #         t["trialID"] = "instruct.$trial_number"
        #     end
        # end
        Trial(uid, trial_number - n_instruct, trial_events)
    end    
end

# %% --------