"""
    abstract type GeometricalWorld <: AbstractWorld end

Abstract type for worlds with a geometrical interpretation.

See also [`Point`](@ref), [`Interval`](@ref), [`Interval2D`](@ref), [`AbstractWorld`](@ref).
"""
abstract type GeometricalWorld <: AbstractWorld end

############################################################################################
# Point
############################################################################################

"""
    struct Point{N,T} <: GeometricalWorld
        xyz :: NTuple{N,T}
    end

A point in an `N`-dimensional space, with integer coordinates.

# Examples
```julia-repl
julia> SoleLogics.goeswithdim(SoleLogics.Point(1,2,3),3)
true

julia> SoleLogics.goeswithdim(SoleLogics.Point(1,2,3),2)
false

```
See also [`goeswithdim`](@ref), [`Interval`](@ref),
[`Interval2D`](@ref), [`GeometricalWorld`](@ref), [`AbstractWorld`](@ref).
"""
struct Point{N,T} <: GeometricalWorld
    xyz :: NTuple{N,T}
    # TODO check x<=N but only in debug mode
    # Point(x) = x<=N ... ? new(x) : throw_n_log("Cannot instantiate Point(x={$x})")
    
    # TODO needed?
    Point(w::Point) = Point(w.xyz)
    
    Point{N,T}(xyz::NTuple{N,T}) where {N,T} = new{N,T}(xyz)
    Point() = error("Cannot instantiate Point in a 0-dimensional space. " *
        "Please, consider using `OneWorld` instead.")
    Point(xyz::NTuple{N,T}) where {N,T} = Point{N,T}(xyz)
    Point((xyz,)::Tuple{NTuple{N,T}}) where {N,T} = Point{N,T}(xyz)
    Point(xyz::Vararg) = Point(xyz)
end

show(io::IO, r::Point) = print(io, "($(join(r.xyz, ",")))")

goeswithdim(::Type{P}, ::Val{N}) where {N,P<:Point{N}} = true

############################################################################################
# Interval 1D
############################################################################################

"""
    struct Interval{T} <: GeometricalWorld
        x :: T
        y :: T
    end

An interval in a 1-dimensional space, with coordinates of type `T`.

# Examples
```julia-repl
julia> SoleLogics.goeswithdim(SoleLogics.Interval(1,2),1)
true

julia> SoleLogics.goeswithdim(SoleLogics.Interval(1,2),2)
false

julia> collect(accessibles(SoleLogics.FullDimensionalFrame(5), Interval(1,2), SoleLogics.IA_L))
6-element Vector{Interval{Int64}}:
 (3−4)
 (3−5)
 (4−5)
 (3−6)
 (4−6)
 (5−6)


```
See also
[`goeswithdim`](@ref),
[`accessibles`](@ref),
[`FullDimensionalFrame`](@ref),
[`Point`](@ref),
[`Interval2D`](@ref), [`GeometricalWorld`](@ref), [`AbstractWorld`](@ref).
"""
struct Interval{T} <: GeometricalWorld
    x :: T
    y :: T

    # TODO needed?
    Interval(w::Interval) = Interval(w.x,w.y)

    Interval{T}(x::T,y::T) where {T} = new{T}(x,y)
    Interval(x::T,y::T) where {T} = Interval{T}(x,y)
    # TODO: perhaps check x<y (and  x<=N, y<=N ?), but only in debug mode.
    # Interval(x,y) = x>0 && y>0 && x < y ? new(x,y) : throw_n_log("Cannot instantiate Interval(x={$x},y={$y})")
end

Base.show(io::IO, w::Interval) = print(io, "($(w.x)−$(w.y))")

goeswithdim(::Type{<:Interval}, ::Val{1}) = true

############################################################################################
# Interval 2D
############################################################################################

"""
    struct Interval2D{T} <: GeometricalWorld
        x :: Interval{T}
        y :: Interval{T}
    end

A orthogonal rectangle in a 2-dimensional space, with coordinates of type `T`.
This is the 2-dimensional `Interval` counterpart, that is,
the combination of two orthogonal `Interval`s.

# Examples
```julia-repl
julia> SoleLogics.goeswithdim(SoleLogics.Interval2D((1,2),(3,4)),1)
false

julia> SoleLogics.goeswithdim(SoleLogics.Interval2D((1,2),(3,4)),2)
true

julia> collect(accessibles(SoleLogics.FullDimensionalFrame(5,5), Interval2D((2,3),(2,4)), SoleLogics.IA_LL))
3-element Vector{Interval2D{Int64}}:
 ((4−5)×(5−6))
 ((4−6)×(5−6))
 ((5−6)×(5−6))

```
See also
[`goeswithdim`](@ref),
[`accessibles`](@ref),
[`FullDimensionalFrame`](@ref),
[`Point`](@ref),
[`Interval`](@ref), [`GeometricalWorld`](@ref), [`AbstractWorld`](@ref).
"""
struct Interval2D{T} <: GeometricalWorld
    x :: Interval{T}
    y :: Interval{T}

    # TODO needed?
    Interval2D(w::Interval2D) = Interval2D{T}(w.x,w.y)

    Interval2D{T}(x::Interval{T},y::Interval{T}) where {T} = new{T}(x,y)
    Interval2D(x::Interval{T},y::Interval{T}) where {T} = Interval2D{T}(x,y)
    Interval2D{T}(x::Tuple{T,T}, y::Tuple{T,T}) where {T} = Interval2D{T}(Interval(x),Interval(y))
    Interval2D(x::Tuple{T,T}, y::Tuple{T,T}) where {T} = Interval2D{T}(x,y)
end

function Base.show(io::IO, w::Interval2D)
    print(io, "(")
    print(io, w.x)
    print(io, "×")
    print(io, w.y)
    print(io, ")")
end

goeswithdim(::Type{<:Interval2D}, ::Val{2}) = true
