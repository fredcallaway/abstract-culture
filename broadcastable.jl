"Adds arithmetic operations to a struct definition."
macro broadcastable(struct_def)
    # Handle both plain struct and @kwdef transformed struct
    if struct_def isa Expr && struct_def.head == :macrocall && struct_def.args[1] == Symbol("@kwdef")
        # Extract the actual struct definition from @kwdef macro call
        actual_struct = struct_def.args[3]  # @kwdef puts the struct as the 3rd argument
    else
        actual_struct = struct_def
    end
    
    # Extract struct name, handling subtyping and parametric types
    struct_name_expr = actual_struct.args[2]
    struct_name = nothing
    type_params = []
    
    if struct_name_expr isa Expr && struct_name_expr.head == :(<:)
        # Struct has a supertype: struct Name <: SuperType or struct Name{T} <: SuperType
        left_side = struct_name_expr.args[1]
        if left_side isa Expr && left_side.head == :curly
            # struct Name{T} <: SuperType
            struct_name = left_side.args[1]  # Name
            type_params = left_side.args[2:end]  # [T, ...]
        else
            # struct Name <: SuperType
            struct_name = left_side
        end
    elseif struct_name_expr isa Expr && struct_name_expr.head == :curly
        # Parametric type: struct Name{T}
        struct_name = struct_name_expr.args[1]  # Name
        type_params = struct_name_expr.args[2:end]  # [T, ...]
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
    
    # Create the type expression for method signatures
    if length(type_params) > 0
        # For parametric types, extract just the type variable names for the type expression
        # and keep the full constraints for the where clause
        type_var_names = []
        for param in type_params
            if param isa Expr && param.head == :(<:)
                # Constrained parameter: T<:Real -> use just T
                push!(type_var_names, param.args[1])
            else
                # Unconstrained parameter: T -> use T
                push!(type_var_names, param)
            end
        end
        
        type_expr = :($struct_name{$(type_var_names...)})
        where_clause = Expr(:where, nothing, type_params...)
    else
        # For non-parametric types
        type_expr = struct_name
        where_clause = nothing
    end
    
    for op in ops
        # Struct vs Struct
        if where_clause !== nothing
            push!(method_exprs, :(
                function Base.$op(x::$type_expr, y::$type_expr) where {$(type_params...)}
                    $type_expr($((:(Base.$op(x.$f, y.$f)) for f in fnames)...))
                end
            ))
        else
            push!(method_exprs, :(
                function Base.$op(x::$type_expr, y::$type_expr)
                    $type_expr($((:(Base.$op(x.$f, y.$f)) for f in fnames)...))
                end
            ))
        end
        
        # Struct vs Number
        if where_clause !== nothing
            push!(method_exprs, :(
                function Base.$op(x::$type_expr, y::Number) where {$(type_params...)}
                    $type_expr($((:(Base.$op(x.$f, y)) for f in fnames)...))
                end
            ))
        else
            push!(method_exprs, :(
                function Base.$op(x::$type_expr, y::Number)
                    $type_expr($((:(Base.$op(x.$f, y)) for f in fnames)...))
                end
            ))
        end
        
        # Number vs Struct
        if where_clause !== nothing
            push!(method_exprs, :(
                function Base.$op(y::Number, x::$type_expr) where {$(type_params...)}
                    $type_expr($((:(Base.$op(y, x.$f)) for f in fnames)...))
                end
            ))
        else
            push!(method_exprs, :(
                function Base.$op(y::Number, x::$type_expr)
                    $type_expr($((:(Base.$op(y, x.$f)) for f in fnames)...))
                end
            ))
        end
    end
    
    # Generate isapprox and broadcastable methods
    if where_clause !== nothing
        isapprox_method = :(
            function Base.isapprox(x::$type_expr, y::$type_expr; kwargs...) where {$(type_params...)}
                all(isapprox(getfield(x, f), getfield(y, f); kwargs...) for f in fieldnames($struct_name))
            end
        )
        broadcastable_method = :(
            Base.broadcastable(x::$type_expr) where {$(type_params...)} = Ref(x)
        )
    else
        isapprox_method = :(
            function Base.isapprox(x::$type_expr, y::$type_expr; kwargs...)
                all(isapprox(getfield(x, f), getfield(y, f); kwargs...) for f in fieldnames($struct_name))
            end
        )
        broadcastable_method = :(
            Base.broadcastable(x::$type_expr) = Ref(x)
        )
    end
    
    esc(quote
        $struct_def
        
        $(method_exprs...)
        
        $isapprox_method
        
        $broadcastable_method
    end)
end
