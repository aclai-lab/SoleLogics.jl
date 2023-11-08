
"""
Some worlds (dimensional worlds) can be interpreted on dimensional data,
that is, n-dimensional arrays. The compatibility of a given world with respect of a 
structure of a given dimensionality must be specified via the following trait:

    goeswithdim(w::AbstractWorld, d) = goeswithdim(typeof(w), d)
    goeswithdim(W::Type{<:AbstractWorld}, d::Integer) = goeswithdim(W, Val(d))
    goeswithdim(::Type{<:AbstractWorld}, ::Val) = false

# Examples
```julia-repl
julia> SoleLogics.goeswithdim(OneWorld, 0)
true

julia> SoleLogics.goeswithdim(OneWorld, 1)
false

julia> SoleLogics.goeswithdim(Interval, 1)
true

julia> SoleLogics.goeswithdim(Interval, 2)
false

julia> all([SoleLogics.goeswithdim.(SoleLogics.Point{N}, N) for N in 1:10])
true

```

See also [`OneWorld`](@ref), [`World`](@ref), [`Interval`](@ref), [`Interval2D`](@ref),
[`GeometricalWorld`](@ref), [`AbstractWorld`](@ref).
"""
goeswithdim(w::AbstractWorld, d) = goeswithdim(typeof(w), d)
goeswithdim(W::Type{<:AbstractWorld}, d::Integer) = goeswithdim(W, Val(d))
goeswithdim(::Type{<:AbstractWorld}, ::Val) = false

############################################################################################
# One unique world (propositional case)
############################################################################################

"""
    struct OneWorld <: AbstractWorld end

A singleton world to be used in modal frames with a single, unique world.
This usage effectively simulates a propositional context.
Note that it is compatible with 0-dimensional datasets.

See also [`Interval`](@ref), [`Interval2D`](@ref),
[`goeswithdim`](@ref), [`AbstractWorld`](@ref).
"""
struct OneWorld <: AbstractWorld end

inlinedisplay(w::OneWorld) = "−"

# A propositional world is compatible with 0-dimensional datasets
goeswithdim(::Type{OneWorld}, ::Val{0}) = true

############################################################################################

worlds_doc = """
    const AbstractWorlds{W} = AbstractVector{W} where {W<:AbstractWorld}
    const Worlds{W} = Vector{W} where {W<:AbstractWorld}

Useful aliases for dealing with worlds sets/arrays.

See also [`accessibles`](@ref), [`AbstractWorld`](@ref).
"""

"""$(worlds_doc)"""
const AbstractWorlds{W} = AbstractVector{W} where {W<:AbstractWorld}

"""$(worlds_doc)"""
const Worlds{W} = Vector{W} where {W<:AbstractWorld}
# Worlds{W}(S::Worlds{W}) where {W<:AbstractWorld} = S

# For convenience, each world type can be instantiated with a tuple of values, one for each field.
(W::Type{<:AbstractWorld})(args::Tuple) = W(args...)

############################################################################################

include("worlds/geometrical-worlds.jl")
