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

include("worlds/geometrical-worlds.jl")
