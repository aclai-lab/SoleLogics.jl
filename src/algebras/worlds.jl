
"""
Some worlds (dimensional worlds) can be interpreted on dimensional data,
 that is, n-dimensional arrays. The compatibility
of a given world with respect of a structure of a given dimensionality must be specified
via the following trait:

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
```

See also [`OneWorld`](@ref), [`NamedWorld`](@ref), [`Interval`](@ref), [`Interval2D`](@ref),
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

Base.show(io::IO, w::OneWorld) = print(io, "−")

# A propositional world is compatible with 0-dimensional datasets
goeswithdim(::Type{OneWorld}, ::Val{0}) = true

############################################################################################

"""
    struct NamedWorld{T} <: AbstractWorld
        name::T
    end

A world that is only identified by its `name`.
This can be useful when instantiating the underlying graph of a modal frame
in an explicit way.

See also [`OneWorld`](@ref), [`AbstractWorld`](@ref).
"""
struct NamedWorld{T} <: AbstractWorld
    name::T
end

name(w::NamedWorld) = w.name

Base.show(io::IO, w::NamedWorld) = print(io, name(w))

############################################################################################

# World enumerators generate array/set-like structures
"""
    const AbstractWorldSet{W} = Union{AbstractVector{W},AbstractSet{W}} where {W<:AbstractWorld}
    const WorldSet{W} = Vector{W} where {W<:AbstractWorld}

Useful aliases.

See also [`WorldSet`](@ref), [`AbstractWorld`](@ref).
"""
const AbstractWorldSet{W} = Union{AbstractVector{W},AbstractSet{W}} where {W<:AbstractWorld}
"""
    const AbstractWorldSet{W} = Union{AbstractVector{W},AbstractSet{W}} where {W<:AbstractWorld}
    const WorldSet{W} = Vector{W} where {W<:AbstractWorld}

Useful aliases.

See also [`AbstractWorldSet`](@ref), [`AbstractWorld`](@ref).
"""
const WorldSet{W} = Vector{W} where {W<:AbstractWorld}
WorldSet{W}(S::WorldSet{W}) where {W<:AbstractWorld} = S

# For convenience, each world type can be instantiated with a tuple of values, one for each field.
(W::Type{<:AbstractWorld})(args::Tuple) = W(args...)

############################################################################################

include("worlds/geometrical-worlds.jl")
