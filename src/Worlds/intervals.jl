############################################################################################
# Interval 1D
############################################################################################

# An interval is a pair of natural numbers (x,y) where: i) x > 0; ii) y > 0; iii) x < y.
struct Interval <: World
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
    print(io, "( ")
    print(io, w.x)
    print(io, "−")
    print(io, w.y)
    print(io, ")")
end

dimensionality(::Type{Interval}) = 1

############################################################################################
# Interval 2D
############################################################################################

# 2-dimensional Interval counterpart: combination of two orthogonal Intervals
struct Interval2D <: World
    x :: Interval
    y :: Interval
    #
    Interval2D(x::Interval,y::Interval) = new(x,y)
    Interval2D(w::Interval2D) = Interval2D(w.x,w.y)
    Interval2D(x::Tuple{Integer,Integer}, y::Tuple{Integer,Integer}) = Interval2D(Interval(x),Interval(y))
    #
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

dimensionality(::Type{Interval2D}) = 2

############################################################################################
# One unique world (propositional case)
############################################################################################

struct OneWorld <: World
    OneWorld() = new()
    #
    OneWorld(w::EmptyWorld) = new()
    OneWorld(w::CenteredWorld, args...) = new()
end;

Base.show(io::IO, w::OneWorld) = begin
    print(io, "−")
end

dimensionality(::Type{OneWorld}) = 0
