using IterTools
import SoleBase: dimensionality, channelsize

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

"""
    struct FullDimensionalFrame{N,W<:AbstractWorld} <: AbstractDimensionalFrame{N,W}
        channelsize::NTuple{N,Int}
    end

Abstract type for full dimensional frames.
Given a N-dimensional array of size (X, Y, Z, ...)
the corresponding full dimensional frame is a graph where there is exactly one vertex
for each
N-hyperrectangle (e.g., an Interval/Interval2D) in the space (1:X, 1:Y, 1:Z, ...).

Here, a `N`-hyperrectangle is an `N` tuple of intervals, where
each interval is a pair of natural numbers (x,y) where: i) x > 0; ii) y > 0; iii) x < y.

The current implementation can handle N ∈ {0,1,2}.

# Examples
```julia-repl
julia> SoleLogics.allworlds(SoleLogics.FullDimensionalFrame((),))
1-element Vector{OneWorld}:
 −

julia> nworlds(SoleLogics.FullDimensionalFrame((10,),))
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
[`IntervalRelation2D`](@ref),
[`accessibles`](@ref),
[`AbstractDimensionalFrame`](@ref), [`AbstractMultiModalFrame`](@ref).
"""
struct FullDimensionalFrame{N,W<:AbstractWorld} <: AbstractDimensionalFrame{N,W}
    
    channelsize::NTuple{N,Int}
    
    function FullDimensionalFrame{N,W}(channelsize::NTuple{N,Int}) where
            {N,W<:AbstractWorld}
        new{N,W}(channelsize)
    end
    function FullDimensionalFrame{N,W}(channelsize::Vararg{Int,N}) where
            {N,W<:AbstractWorld}
        FullDimensionalFrame{N,W}(channelsize)
    end
    
    function FullDimensionalFrame(channelsize::Tuple{})
        FullDimensionalFrame{0,OneWorld}(channelsize)
    end
    function FullDimensionalFrame(channelsize::Tuple{Int})
        FullDimensionalFrame{1,Interval{Int}}(channelsize)
    end
    function FullDimensionalFrame(channelsize::Tuple{Int,Int})
        FullDimensionalFrame{2,Interval2D{Int}}(channelsize)
    end
    function FullDimensionalFrame(channelsize...)
        FullDimensionalFrame(channelsize)
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

# Convenience function: enumerate all interval2Ds in a given range
intervals2D_in(a1::Integer, a2::Integer, b1::Integer, b2::Integer) = IterTools.imap(Interval2D{Int}, Iterators.product(_intervals_in(a1, a2), _intervals_in(b1, b2)))

# _accessibles(fr::Full0DFrame, ::OneWorld, ::IdentityRel) = [OneWorld()]

const Full0DFrame = FullDimensionalFrame{0,OneWorld}
const Full1DFrame = FullDimensionalFrame{1,Interval{Int}}
const Full2DFrame = FullDimensionalFrame{2,Interval2D{Int}}

############################################################################################

allworlds(fr::FullDimensionalFrame{0}) = [OneWorld()]
allworlds(fr::FullDimensionalFrame{1}) = intervals_in(1, X(fr)+1)
allworlds(fr::FullDimensionalFrame{2}) = intervals2D_in(1, X(fr)+1, 1, Y(fr)+1)

nworlds(fr::FullDimensionalFrame{0}) = 1
nworlds(fr::FullDimensionalFrame{1}) = div(X(fr)*(X(fr)+1), 2)
nworlds(fr::FullDimensionalFrame{2}) = div(X(fr)*(X(fr)+1), 2) * div(Y(fr)*(Y(fr)+1), 2)
nworlds(fr::FullDimensionalFrame{3}) = div(X(fr)*(X(fr)+1), 2) * div(Y(fr)*(Y(fr)+1), 2) * div(Z(fr)*(Z(fr)+1), 2)

############################################################################################

emptyworld(fr::FullDimensionalFrame{0}) = OneWorld()
emptyworld(fr::FullDimensionalFrame{1}) = Interval{Int}(-1,0)
emptyworld(fr::FullDimensionalFrame{2}) = Interval2D{Int}(Interval{Int}(-1,0),Interval{Int}(-1,0))

# Smallest centered hyperrectangle
_centeredworld(X::Integer) = Interval{Int}(div(X+1, 2),(div(X+1, 2))+1+(isodd(X) ? 0 : 1))
centeredworld(fr::FullDimensionalFrame{0}) = OneWorld()
centeredworld(fr::FullDimensionalFrame{1}) = _centeredworld(X(fr))
centeredworld(fr::FullDimensionalFrame{2}) = Interval2D{Int}(_centeredworld(X(fr)),_centeredworld(Y(fr)))

############################################################################################

include("Full1DFrame+IA.jl")
include("Full1DFrame+RCC.jl")

include("Full2DFrame+IA2D.jl")
include("Full2DFrame+RCC.jl")
