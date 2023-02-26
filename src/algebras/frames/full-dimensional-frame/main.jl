using IterTools

"""
    abstract type AbstractDimensionalFrame{
        N,
        W<:AbstractWorld,
        T<:TruthValue
    } <: AbstractMultiModalFrame{W,T} end

Abstract type for dimensional frames. Given a `N`-dimensional array of size (X, Y, Z, ...)
the corresponding dimensional frame is a graph where each vertex is an
`N`-hyperrectangle (e.g., an Interval/Interval2D) in the space (1:X, 1:Y, 1:Z, ...).

See also 
[`Interval`](@ref),
[`Interval2D`](@ref),
[`IntervalRelation`](@ref),
[`AbstractDimensionalFrame`](@ref), [`AbstractMultiModalFrame`](@ref).
"""
abstract type AbstractDimensionalFrame{N,W<:AbstractWorld,T<:TruthValue} <: AbstractMultiModalFrame{W,T} end

"""
    struct FullDimensionalFrame{N,W<:AbstractWorld,T<:TruthValue} <: AbstractDimensionalFrame{N,W,T}
        channel_size::NTuple{N,Int}
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
struct FullDimensionalFrame{N,W<:AbstractWorld,T<:TruthValue} <: AbstractDimensionalFrame{N,W,T}
    
    channel_size::NTuple{N,Int}
    
    function FullDimensionalFrame{N,W,T}(channel_size::NTuple{N,Int}) where
            {N,W<:AbstractWorld,T<:TruthValue}
        new{N,W,T}(channel_size)
    end
    
    function FullDimensionalFrame(channel_size::Tuple{})
        FullDimensionalFrame{0,OneWorld,Bool}(channel_size)
    end
    function FullDimensionalFrame(channel_size::Tuple{Int})
        FullDimensionalFrame{1,Interval{Int},Bool}(channel_size)
    end
    function FullDimensionalFrame(channel_size::Tuple{Int,Int})
        FullDimensionalFrame{2,Interval2D{Int},Bool}(channel_size)
    end
    function FullDimensionalFrame(channel_size...)
        FullDimensionalFrame(channel_size)
    end
end

############################################################################################
# Utils
############################################################################################

channel_size(fr::FullDimensionalFrame) = fr.channel_size
Base.getindex(fr::FullDimensionalFrame, i::Int) = fr.channel_size[i]

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

const Full0DFrame = FullDimensionalFrame{0,OneWorld,Bool}
const Full1DFrame = FullDimensionalFrame{1,Interval{Int},Bool}
const Full2DFrame = FullDimensionalFrame{2,Interval2D{Int},Bool}

############################################################################################

allworlds(fr::FullDimensionalFrame{0}) = [OneWorld()]
allworlds(fr::FullDimensionalFrame{1}) = intervals_in(1, X(fr)+1)
allworlds(fr::FullDimensionalFrame{2}) = intervals2D_in(1,X(fr)+1,1,Y(fr)+1)

nworlds(fr::FullDimensionalFrame{0}) = 1
nworlds(fr::FullDimensionalFrame{1}) = div(X(fr)*(X(fr)+1),2)
nworlds(fr::FullDimensionalFrame{2}) = div(X(fr)*(X(fr)+1),2) * div(Y(fr)*(Y(fr)+1),2)
nworlds(fr::FullDimensionalFrame{3}) = div(X(fr)*(X(fr)+1),2) * div(Y(fr)*(Y(fr)+1),2) * div(Z(fr)*(Z(fr)+1),2)

############################################################################################

emptyworld(fr::FullDimensionalFrame{0}) = OneWorld()
emptyworld(fr::FullDimensionalFrame{1}) = Interval{Int}(-1,0)
emptyworld(fr::FullDimensionalFrame{2}) = Interval2D{Int}(Interval{Int}(w),Interval{Int}(w))

# Smallest centered hyperrectangle
_centeredworld(X::Integer) = Interval{Int}(div(X,2)+1,(div(X,2)+1)+1+(isodd(X) ? 0 : 1))
centeredworld(fr::FullDimensionalFrame{0}) = OneWorld()
centeredworld(fr::FullDimensionalFrame{1}) = _centeredworld(X(fr))
centeredworld(fr::FullDimensionalFrame{2}) = Interval2D{Int}(_centeredworld(X(fr)),_centeredworld(Y(fr)))

############################################################################################

include("Full1DFrame+IA.jl")
include("Full1DFrame+RCC.jl")

include("Full2DFrame+IA2D.jl")
include("Full2DFrame+RCC.jl")
