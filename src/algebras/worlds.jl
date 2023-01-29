# Abstract type for world
abstract type AbstractWorld end

# These constants is used for specifying different initial world conditions for each world type
#  (e.g. Interval(::EmptyWorld) = Interval(-1,0))
struct EmptyWorld end;
struct CenteredWorld end;

# More specifically, any world type W must provide constructors for:
# `W(::EmptyWorld)` # A dummy world (= no world in particular)
# `W(::CenteredWorld, args...)` # A world that is *central* to the modal frame

# For convenience, each world type can be instantiated with a tuple of values, one for each field.
(W::Type{<:AbstractWorld})(args::Tuple) = W(args...)

# World enumerators generate array/set-like structures
const AbstractWorldSet{W} = Union{AbstractVector{W},AbstractSet{W}} where {W<:AbstractWorld}
# const AbstractWorldOrWorldSet{W} = Union{W,AbstractWorldSet{W}} where {W<:AbstractWorld} # Note: can result in dispatch ambiguities sometimes.
const WorldSet{W} = Vector{W} where {W<:AbstractWorld}
WorldSet{W}(S::WorldSet{W}) where {W<:AbstractWorld} = S

include("worlds/geometrical-worlds.jl")

export AbstractWorld
export EmptyWorld, CenteredWorld
export AbstractWorldSet, WorldSet

export Interval, Interval2D, OneWorld
export dimensionality
export goeswith_dim
