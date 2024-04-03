using RemoteREPL
REMOTE_RESULTS = Any[]

macro both(expr)
    if isnothing(RemoteREPL._repl_client_connection)
        esc(expr)
    else
        quote
            $(esc(expr))
            @remote @eval $expr
        end
    end
    # RemoteREPL.@remote expr
end

macro asyncremote(expr)
    quote
        println("Starting remote task")
        _remote_task = @async @remote begin
            result = $expr
            if !@isdefined LOCAL_RESULTS
                global LOCAL_RESULTS = []
            end
            push!(LOCAL_RESULTS, result)
            result
        end
        @async begin
            try
                result = fetch(_remote_task)
                push!(REMOTE_RESULTS, result)
                run(`say \"task complete\"`)
                println("✅ Remote Task Done!")
            catch err
                println("☠️ REMOTE TASK FAILED!")
                run(`say \"task failed\"`)
                rethrow()
            end
        end
        _remote_task
    end
end
