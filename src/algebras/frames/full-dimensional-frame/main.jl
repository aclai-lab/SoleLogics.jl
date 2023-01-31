using IterTools

abstract type AbstractDimensionalFrame{N,W<:AbstractWorld,T<:TruthValue} <: AbstractMultiModalFrame{W,T} end

struct FullDimensionalFrame{N,W<:AbstractWorld,T<:TruthValue} <: AbstractDimensionalFrame{N,W,T}
    
    channel_size::NTuple{N,Int}
    
    function FullDimensionalFrame{N,W,T}(channel_size::NTuple{N,Int}) where
            {N,W<:AbstractWorld,T<:TruthValue}
        new{N,W,T}(channel_size)
    end
    
    FullDimensionalFrame(channel_size::Tuple{}) = FullDimensionalFrame{0,OneWorld,Bool}(channel_size)
    FullDimensionalFrame(channel_size::Tuple{Int}) = FullDimensionalFrame{1,Interval,Bool}(channel_size)
    FullDimensionalFrame(channel_size::Tuple{Int,Int}) = FullDimensionalFrame{2,Interval2D,Bool}(channel_size)
end

channel_size(fr::FullDimensionalFrame) = fr.channel_size
Base.getindex(fr::FullDimensionalFrame, i::Int) = fr.channel_size[i]

# Shorthands
X(fr::FullDimensionalFrame) = fr[1]
Y(fr::FullDimensionalFrame) = fr[2]
Z(fr::FullDimensionalFrame) = fr[3]

nworlds(fr::FullDimensionalFrame{1}) = div(X(fr)*(X(fr)+1),2)
nworlds(fr::FullDimensionalFrame{2}) = div(X(fr)*(X(fr)+1),2) * div(Y(fr)*(Y(fr)+1),2)
nworlds(fr::FullDimensionalFrame{3}) = div(X(fr)*(X(fr)+1),2) * div(Y(fr)*(Y(fr)+1),2) * div(Z(fr)*(Z(fr)+1),2)

const Full0DFrame = FullDimensionalFrame{0,OneWorld,Bool}
const Full1DFrame = FullDimensionalFrame{1,Interval,Bool}
const Full2DFrame = FullDimensionalFrame{2,Interval2D,Bool}

############################################################################################

accessibles(fr::Full0DFrame, ::_RelationGlob) = [OneWorld()]

# _accessibles(fr::Full0DFrame, ::OneWorld, ::_RelationId) = [OneWorld()]

# Convenience functions: enumerate all & 1-length intervals in a given range
_intervals_in(a::Integer, b::Integer) = Iterators.filter(((x,y),)->x<y, Iterators.product(a:b-1, a+1:b))
intervals_in(a::Integer, b::Integer) = IterTools.imap(Interval, _intervals_in(a, b))
short_intervals_in(a::Integer, b::Integer) = IterTools.imap((x)->Interval(x,x+1), a:b-1)

accessibles(fr::Full1DFrame, ::_RelationGlob) = intervals_in(1, X(fr)+1)

# Convenience function: enumerate all interval2Ds in a given range
intervals2D_in(a1::Integer, a2::Integer, b1::Integer, b2::Integer) = IterTools.imap(Interval2D, Iterators.product(_intervals_in(a1, a2), _intervals_in(b1, b2)))

accessibles(fr::Full2DFrame, ::_RelationGlob) =
    intervals2D_in(1,X(fr)+1,1,Y(fr)+1)

############################################################################################
############################################################################################
############################################################################################


include("Full1DFrame+IA.jl")
include("Full1DFrame+RCC.jl")

include("Full2DFrame+IA2.jl")
include("Full2DFrame+RCC.jl")
