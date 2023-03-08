
"""
    abstract type GeometricalWorld <: AbstractRelation end

Abstract type for relations with a geometrical interpretation.

See also [`istopological`](@ref),
[`IntervalRelation`](@ref), [`RectangleRelation`](@ref),
[`RCCRelation`](@ref), [`AbstractRelation`](@ref).
"""
abstract type GeometricalRelation <: AbstractRelation end

"""
    istopological(r::GeometricalRelation)

Returns whether it is known that a given geometrical relation is topological
(i.e., invariant under homeomorphisms,
 see [here](https://en.m.wikipedia.org/wiki/Topological_property))

See also [`GeometricalRelation`](@ref).
"""
istopological(r::GeometricalRelation) = false

# TODO add type for carthesian product of geometrical relations

############################################################################################

# 1D Allen relations
include("IA.jl")

# 2D Allen relations
include("IA2D.jl")

# RCC relations
include("RCC.jl")
