############################################################################################
# One unique world in a singleton frame (propositional logic)
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

nparameters(::OneWorld) = 0

# A propositional world is compatible with 0-dimensional datasets
goeswithdim(::Type{OneWorld}, ::Val{0}) = true

nworlds(::OneWorld) = 1

############################################################################################

include("worlds/geometrical-worlds.jl")
