
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

Return whether it is known that a given geometrical relation is topological
(i.e., invariant under homeomorphisms,
 see [here](https://en.wikipedia.org/wiki/Topological_property))

See also [`GeometricalRelation`](@ref).
"""
istopological(r::GeometricalRelation) = false

# TODO add type for carthesian product of geometrical relations

############################################################################################

# Point relations
include("Point.jl")

# 1D Allen relations
include("IntervalAlgebra.jl")

# 2D Allen relations
include("IntervalAlgebra2D.jl")

# RCC relations
include("RCC.jl")
