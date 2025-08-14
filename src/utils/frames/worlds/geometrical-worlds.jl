import Base: length
import SoleBase: dimensionality

############################################################################################
# Point
############################################################################################

"""
    struct Point{N,T<:Real} <: GeometricalWorld
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
struct Point{N,T<:Real} <: GeometricalWorld
    xyz :: NTuple{N,T}
    # TODO check x<=N but only in debug mode
    # Point(x) = x<=N ... ? new(x) : error("Cannot instantiate Point(x={$x})")

    # TODO needed?
    Point(w::Point) = Point(w.xyz)

    Point{N,T}(xyz::NTuple{N,T}) where {N,T<:Real} = new{N,T}(xyz)
    Point{N,T}(xyz::Vararg{T,N}) where {N,T<:Real} = Point{N,T}(xyz)
    Point() = error("Cannot instantiate Point in a 0-dimensional space. " *
        "Please, consider using `OneWorld` instead.")
    Point(xyz::NTuple{N,T}) where {N,T<:Real} = Point{N,T}(xyz)
    Point((xyz,)::Tuple{NTuple{N,T}}) where {N,T<:Real} = Point{N,T}(xyz)
    Point(xyz::Vararg) = Point(xyz)
end

# Base.size(w::Point) = (1,) # TODO maybe not
Base.length(w::Point) = 1
Base.size(::Point) = ()

inlinedisplay(w::Point) = "❮$(join(w.xyz, ","))❯"

Base.getindex(w::Point, args...) = Base.getindex(w.xyz, args...)

X(w::Point) = w[1]
Y(w::Point) = w[2]
Z(w::Point) = w[3]

dimensionality(::Point{N}) where {N} = N
dimensionality(::Type{<:Point{N}}) where {N} = N

nparameters(T::Type{<:Point}) = dimensionality(T)

goeswithdim(::Type{P}, ::Val{N}) where {N,P<:Point{N}} = true

# Useful aliases
"""const Point1D = Point{1}

A world representing a point in a one-dimensional space.
See [`Point`](@ref).
"""
const Point1D = Point{1}
"""const Point2D = Point{2}

A world representing a point in a two-dimensional space.
See [`Point`](@ref).
"""
const Point2D = Point{2}
"""const Point3D = Point{3}

A world representing a point in a three-dimensional space.
See [`Point`](@ref).
"""
const Point3D = Point{3}

############################################################################################
# Interval 1D
############################################################################################

"""
    struct Interval{T<:Real} <: GeometricalWorld
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
struct Interval{T<:Real} <: GeometricalWorld
    x :: T
    y :: T

    # TODO needed?
    Interval(w::Interval) = Interval(w.x,w.y)

    Interval{T}(x::T,y::T) where {T} = new{T}(x,y)
    Interval(x::T,y::T) where {T} = Interval{T}(x,y)
    # TODO: perhaps check x<y (and  x<=N, y<=N ?), but only in debug mode.
    # Interval(x,y) = x>0 && y>0 && x < y ? new(x,y) : error("Cannot instantiate Interval(x={$x},y={$y})")
end

# Base.size(w::Interval) = (Base.length(w),)
Base.length(w::Interval) = (w.y - w.x)
Base.size(w::Interval) = (Base.length(w),)

inlinedisplay(w::Interval) = "($(w.x)−$(w.y))"

goeswithdim(::Type{<:Interval}, ::Val{1}) = true

nparameters(T::Type{<:Interval}) = 2

############################################################################################
# Interval 2D
############################################################################################

"""
    struct Interval2D{T<:Real} <: GeometricalWorld
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
struct Interval2D{T<:Real} <: GeometricalWorld
    x :: Interval{T}
    y :: Interval{T}

    # TODO needed?
    Interval2D(w::Interval2D) = Interval2D{T}(w.x,w.y)

    Interval2D{T}(x::Interval{T},y::Interval{T}) where {T} = new{T}(x,y)
    Interval2D(x::Interval{T},y::Interval{T}) where {T} = Interval2D{T}(x,y)
    Interval2D{T}(x::Tuple{T,T}, y::Tuple{T,T}) where {T} = Interval2D{T}(Interval(x),Interval(y))
    Interval2D(x::Tuple{T,T}, y::Tuple{T,T}) where {T} = Interval2D{T}(x,y)
end

Base.length(w::Interval2D) = Base.length(w.x) * Base.length(w.y)
Base.size(w::Interval2D) = (Base.length(w.x), Base.length(w.y))

inlinedisplay(w::Interval2D) = "($(w.x)×$(w.y))"

goeswithdim(::Type{<:Interval2D}, ::Val{2}) = true

nparameters(T::Type{<:Interval2D}) = 4

############################################################################################

struct RelativeGeometricalWorld{W<:GeometricalWorld} <: GeometricalWorld
    w::W
    # TODO assert xyz \in [0,1] ...?
    function RelativeGeometricalWorld{W}(args...; kwargs...) where {W<:GeometricalWorld}
        new{W}(W(args...; kwargs...))
    end
    function RelativeGeometricalWorld{W}(w::W) where {W<:GeometricalWorld}
        new{W}(w)
    end
    function RelativeGeometricalWorld(w::W) where {W<:GeometricalWorld}
        RelativeGeometricalWorld{W}(w)
    end
end

innerworld(w::RelativeGeometricalWorld) = w.w
@forward RelativeGeometricalWorld.w (
    Base.length,
    inlinedisplay,
    goeswithdim,
    nparameters,
    Base.getindex,
    X, Y, Z, dimensionality,
)

const RelativePoint{N,T<:Real} = RelativeGeometricalWorld{Point{N,T}}
const RelativeInterval{T<:Real} = RelativeGeometricalWorld{Interval{T}}
const RelativeInterval2D{T<:Real} = RelativeGeometricalWorld{Interval2D{T}}
