"""
    abstract type AbstractDimensionalFrame{
        N,
        W<:AbstractWorld,
    } <: AbstractMultiModalFrame{W} end

Abstract type for dimensional frames. Given a `N`-dimensional array of size (X, Y, Z, ...)
the corresponding dimensional frame is a graph where each vertex is an
`N`-hyperrectangle (e.g., an Interval/Interval2D) in the space (1:X, 1:Y, 1:Z, ...).

See also
[`Interval`](@ref),
[`Interval2D`](@ref),
[`IntervalRelation`](@ref),
[`AbstractDimensionalFrame`](@ref), [`AbstractMultiModalFrame`](@ref).
"""
abstract type AbstractDimensionalFrame{N,W<:AbstractWorld} <: AbstractMultiModalFrame{W} end
