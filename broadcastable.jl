"Adds arithmetic operations to a struct definition."
macro broadcastable(struct_def)
    # Handle both plain struct and @kwdef transformed struct
    if struct_def isa Expr && struct_def.head == :macrocall && struct_def.args[1] == Symbol("@kwdef")
        # Extract the actual struct definition from @kwdef macro call
        actual_struct = struct_def.args[3]  # @kwdef puts the struct as the 3rd argument
    else
        actual_struct = struct_def
    end
    
    # Extract struct name, handling subtyping
    struct_name_expr = actual_struct.args[2]
    if struct_name_expr isa Expr && struct_name_expr.head == :(<:)
        # Struct has a supertype: struct Name <: SuperType
        struct_name = struct_name_expr.args[1]
    else
        # Plain struct name
        struct_name = struct_name_expr
    end
    
    fields_block = actual_struct.args[3].args
    
    # Filter out LineNumberNodes and get actual field definitions
    fields = [f for f in fields_block if f isa Expr && (f.head == :(::) || f.head == :(=))]
    
    # Extract field names, handling both regular fields and fields with defaults
    fnames = []
    for f in fields
        if f.head == :(::)
            push!(fnames, f.args[1])
        elseif f.head == :(=) && f.args[1] isa Expr && f.args[1].head == :(::)
            push!(fnames, f.args[1].args[1])
        end
    end
    
    ops = [:+, :-, :*, :/, :^]
    
    # Generate all operator methods
    method_exprs = []
    for op in ops
        # Struct vs Struct
        push!(method_exprs, :(
            function Base.$op(x::$struct_name, y::$struct_name)
                $struct_name($((:(Base.$op(x.$f, y.$f)) for f in fnames)...))
            end
        ))
        
        # Struct vs Number
        push!(method_exprs, :(
            function Base.$op(x::$struct_name, y::Number)
                $struct_name($((:(Base.$op(x.$f, y)) for f in fnames)...))
            end
        ))
        
        # Number vs Struct
        push!(method_exprs, :(
            function Base.$op(y::Number, x::$struct_name)
                $struct_name($((:(Base.$op(y, x.$f)) for f in fnames)...))
            end
        ))
    end
    
    esc(quote
        $struct_def
        
        $(method_exprs...)
        
        function Base.isapprox(x::$struct_name, y::$struct_name; kwargs...)
            all(isapprox(getfield(x, f), getfield(y, f); kwargs...) for f in fieldnames($struct_name))
        end
        
        Base.broadcastable(x::$struct_name) = Ref(x)
    end)
end
