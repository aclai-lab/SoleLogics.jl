############################################################################################
# Dimensonal Worlds
############################################################################################

# Abstract type for worlds with a geometrical interpretation
abstract type GeometricalWorld <: World end

# Some geometrical worlds (dimensional worlds) can be interpreted on dimensional data,
#  that is, n-dimensional arrays.
# The parameter n is referred to as `dimensionality`, and must be specified for each newly 
#  defined dimensional world type via the following trait:
goes_with_dimensionality(W::Type{<:GeometricalWorld}, d::Integer) = goes_with_dimensionality(W, Val(d))
goes_with_dimensionality(::Type{<:GeometricalWorld}, ::Val) = false

############################################################################################
# One unique world (propositional case)
############################################################################################

struct OneWorld <: GeometricalWorld
    OneWorld() = new()

    OneWorld(w::EmptyWorld) = new()
    OneWorld(w::CenteredWorld, args...) = new()
end;

Base.show(io::IO, w::OneWorld) = begin
    print(io, "−")
end

goes_with_dimensionality(::Type{OneWorld}, ::Val{0}) = true


############################################################################################
# Point 1D
############################################################################################

# struct PointWorld <: GeometricalWorld
    # PointWorld(w::PointWorld) = new(w.x,w.y)
#   x :: Integer
#   # TODO check x<=N but only in debug mode
#   # PointWorld(x) = x<=N ... ? new(x) : throw_n_log("Can't instantiate PointWorld(x={$x})")
#   PointWorld(x::Integer) = new(x)
# end

# show(io::IO, r::PointWorld) = print(io, "($(x)×$(y))")

# goes_with_dimensionality(::Type{PointWorld}, ::Val{1}) = true


############################################################################################
# Interval 1D
############################################################################################

# An interval is a pair of natural numbers (x,y) where: i) x > 0; ii) y > 0; iii) x < y.
struct Interval <: GeometricalWorld
    x :: Integer
    y :: Integer

    Interval(x::Integer,y::Integer) = new(x,y)
    Interval(w::Interval) = Interval(w.x,w.y)
    # TODO: perhaps check x<y (and  x<=N, y<=N ?), but only in debug mode.
    # Interval(x,y) = x>0 && y>0 && x < y ? new(x,y) : throw_n_log("Can't instantiate Interval(x={$x},y={$y})")

    Interval(::EmptyWorld) = Interval(-1,0)
    Interval(::CenteredWorld, X::Integer) = Interval(div(X,2)+1,div(X,2)+1+1+(isodd(X) ? 0 : 1))
end

Base.show(io::IO, w::Interval) = begin
    print(io, "(")
    print(io, w.x)
    print(io, "−")
    print(io, w.y)
    print(io, ")")
end

goes_with_dimensionality(::Type{Interval}, ::Val{1}) = true

############################################################################################
# Interval 2D
############################################################################################

# 2-dimensional Interval counterpart: combination of two orthogonal Intervals
struct Interval2D <: GeometricalWorld
    x :: Interval
    y :: Interval

    Interval2D(x::Interval,y::Interval) = new(x,y)
    Interval2D(w::Interval2D) = Interval2D(w.x,w.y)
    Interval2D(x::Tuple{Integer,Integer}, y::Tuple{Integer,Integer}) = Interval2D(Interval(x),Interval(y))

    Interval2D(w::EmptyWorld) = Interval2D(Interval(w),Interval(w))
    Interval2D(w::CenteredWorld, X::Integer, Y::Integer) = Interval2D(Interval(w,X),Interval(w,Y))
end

Base.show(io::IO, w::Interval2D) = begin
    print(io, "(")
    print(io, w.x)
    print(io, "×")
    print(io, w.y)
    print(io, ")")
end

goes_with_dimensionality(::Type{Interval2D}, ::Val{2}) = true
