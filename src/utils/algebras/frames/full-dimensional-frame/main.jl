using IterTools
import SoleBase: dimensionality, channelsize

"""
    struct FullDimensionalFrame{N,W<:AbstractWorld} <: AbstractDimensionalFrame{N,W}
        channelsize::NTuple{N,Int}
    end

Abstract type for full dimensional frames.
Given a `N`-dimensional array of size (X, Y, Z, ...)
the corresponding full dimensional frame is a graph where there is exactly one vertex
for each
M-hyperrectangle in the space (1:X, 1:Y, 1:Z, ...),
with `M ≤ N`.

Here, the `M`-hyperrectangle can be either a [`Point`](@ref),
or a `N`-tuple of intervals
(e.g., [`Interval`](@ref) or [`Interval2D`](@ref)), where
each interval is a pair of natural numbers (x,y) where: i) x > 0; ii) y > 0; iii) x < y.

The current implementation can handle N ∈ {0,1,2}.

# Examples
```julia-repl
julia> SoleLogics.allworlds(SoleLogics.FullDimensionalFrame((),))
1-element Vector{OneWorld}:
 −

julia> nworlds(SoleLogics.FullDimensionalFrame((10,), Interval{Int}))
55

julia> nworlds(SoleLogics.FullDimensionalFrame((10,10),))
3025

julia> collect(accessibles(SoleLogics.FullDimensionalFrame(5,5), Interval2D((2,3),(2,4)), SoleLogics.IA_LL))
3-element Vector{Interval2D{Int64}}:
 ((4−5)×(5−6))
 ((4−6)×(5−6))
 ((5−6)×(5−6))

```

See also
[`OneWorld`](@ref),
[`Interval`](@ref),
[`Interval2D`](@ref),
[`IntervalRelation`](@ref),
[`RectangleRelation`](@ref),
[`accessibles`](@ref),
[`AbstractDimensionalFrame`](@ref), [`AbstractMultiModalFrame`](@ref).
"""
struct FullDimensionalFrame{N,W<:AbstractWorld} <: AbstractDimensionalFrame{N,W}

    channelsize::NTuple{N,Int}

    function FullDimensionalFrame{N,W}(channelsize::NTuple{N,Int}; silent = true) where
            {N,W<:AbstractWorld}
        new{N,W}(channelsize)
    end
    function FullDimensionalFrame{N,W}(channelsize::Vararg{Int,N}; silent = true) where
            {N,W<:AbstractWorld}
        FullDimensionalFrame{N,W}(channelsize; silent)
    end

    function FullDimensionalFrame(channelsize::Tuple{}, W::Union{Nothing,Type{<:AbstractWorld}} = nothing; silent = true)
        if !isnothing(W)
            silent || @warn "Ignoring worldtype provided ($(W)) and defaulting to worldtype to OneWorld."
        end
        W = OneWorld
        FullDimensionalFrame{0,W}(channelsize; silent)
    end
    function FullDimensionalFrame(channelsize::Tuple{Int}, W::Union{Nothing,Type{<:AbstractWorld}} = nothing; silent = true)
        if isnothing(W)
            silent || @warn "Unknown world type. Defaulting to Interval{Int}."
            W = Interval{Int}
        end
        FullDimensionalFrame{1,W}(channelsize; silent)
    end
    function FullDimensionalFrame(channelsize::Tuple{Int,Int}, W::Union{Nothing,Type{<:AbstractWorld}} = nothing; silent = true)
        if isnothing(W)
            silent || @warn "Unknown world type. Defaulting to Interval2D{Int}."
            W = Interval2D{Int}
        end
        FullDimensionalFrame{2,W}(channelsize; silent)
    end
    function FullDimensionalFrame(channelsize::Vararg{Int,N}) where {N}
        FullDimensionalFrame(channelsize)
    end
    function FullDimensionalFrame(; silent = true)
        return error("Could not instantiate FullDimensionalFrame with no dimensions")
    end
end

############################################################################################
# Utils
############################################################################################

channelsize(fr::FullDimensionalFrame) = fr.channelsize
Base.getindex(fr::FullDimensionalFrame, i::Integer) = channelsize(fr)[i]

dimensionality(fr::FullDimensionalFrame) = length(channelsize(fr))

# Shorthands
X(fr::FullDimensionalFrame) = fr[1]
Y(fr::FullDimensionalFrame) = fr[2]
Z(fr::FullDimensionalFrame) = fr[3]

# Convenience functions: enumerate all & 1-length intervals in a given range
_intervals_in(a::Integer, b::Integer) = Iterators.filter(((x,y),)->x<y, Iterators.product(a:b-1, a+1:b))
intervals_in(a::Integer, b::Integer) = IterTools.imap(Interval{Int}, _intervals_in(a, b))
short_intervals_in(a::Integer, b::Integer) = IterTools.imap((x)->Interval{Int}(x,x+1), a:b-1)
points_in(a::Integer, b::Integer) = IterTools.imap((x)->Point(x), a:b)
points_in(a1::Integer, b1::Integer, a2::Integer, b2::Integer) = IterTools.imap(((x,y),)->Point(x,y), Iterators.product(a1:b1, a2:b2))

# Convenience function: enumerate all interval2Ds in a given range
intervals2D_in(a1::Integer, a2::Integer, b1::Integer, b2::Integer) = IterTools.imap(Interval2D{Int}, Iterators.product(_intervals_in(a1, a2), _intervals_in(b1, b2)))

const Full0DFrame      = FullDimensionalFrame{0,OneWorld}
const Full1DFrame      = FullDimensionalFrame{1,Interval{Int}}
const Full1DPointFrame = FullDimensionalFrame{1,Point1D{Int}}
const Full2DFrame      = FullDimensionalFrame{2,Interval2D{Int}}
const Full2DPointFrame = FullDimensionalFrame{2,Point2D{Int}}

############################################################################################

allworlds(fr::Full0DFrame)      = [OneWorld()]
allworlds(fr::Full1DFrame)      = intervals_in(1, X(fr)+1)
allworlds(fr::Full1DPointFrame) = points_in(1, X(fr))
allworlds(fr::Full2DFrame)      = intervals2D_in(1, X(fr)+1, 1, Y(fr)+1)
allworlds(fr::Full2DPointFrame) = points_in(1, X(fr), 1, Y(fr))

nworlds(fr::Full0DFrame)      = 1
nworlds(fr::Full1DFrame)      = div(X(fr)*(X(fr)+1), 2)
nworlds(fr::Full1DPointFrame) = X(fr)
nworlds(fr::Full2DFrame)      = div(X(fr)*(X(fr)+1), 2) * div(Y(fr)*(Y(fr)+1), 2)
nworlds(fr::Full2DPointFrame) = X(fr)*Y(fr)

############################################################################################

emptyworld(fr::Full0DFrame)      = OneWorld()
emptyworld(fr::Full1DFrame)      = Interval{Int}(-1,0)
emptyworld(fr::Full1DPointFrame) = Point(-1)
emptyworld(fr::Full2DFrame)      = Interval2D{Int}(Interval{Int}(-1,0),Interval{Int}(-1,0))
emptyworld(fr::Full2DPointFrame) = Point(-1, -1)

# Smallest centered hyperrectangle
_centralint(X::Integer) = Interval{Int}(div(X+1, 2),(div(X+1, 2))+1+(isodd(X) ? 0 : 1))
centralworld(fr::Full0DFrame)      = OneWorld()
centralworld(fr::Full1DFrame)      = _centralint(X(fr))
centralworld(fr::Full1DPointFrame) = Point(div(X(fr)+1, 2))
centralworld(fr::Full2DFrame)      = Interval2D{Int}(_centralint(X(fr)),_centralint(Y(fr)))
centralworld(fr::Full2DPointFrame) = Point(div(X(fr)+1, 2), div(Y(fr)+1, 2))

############################################################################################

include("dimensional-world-filters.jl")

include("Full1DFrame+IA.jl")
include("Full1DFrame+RCC.jl")

include("Full1DPointFrame.jl")

include("Full2DFrame+IA2D.jl")
include("Full2DFrame+RCC.jl")

include("Full2DPointFrame.jl")

include("FullDimensionalFrame-filtered.jl")
