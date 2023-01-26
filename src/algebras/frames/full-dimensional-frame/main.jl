using IterTools

abstract type DimensionalFrame{N,W<:AbstractWorld,T<:TruthValue,NR,Rs<:NTuple{NR,R where R<:AbstractRelation}} <: AbstractMultiModalFrame{W,T,NR,Rs} end

struct FullDimensionalFrame{N,W<:AbstractWorld,T<:TruthValue,NR,Rs<:NTuple{NR,R where R<:AbstractRelation}} <: DimensionalFrame{N,W,T,NR,Rs}
    
    dims::NTuple{N,Int}
    
    function FullDimensionalFrame{N,W,T,NR,Rs}(dims::NTuple{N,Int}) where
            {N,W<:AbstractWorld,T<:TruthValue,NR,Rs<:NTuple{NR,R where R<:AbstractRelation}}
        new{N,W,T,NR,Rs}(dims)
    end
    
    function FullDimensionalFrame{N,W,T}(dims::NTuple{N,Int}) where
            {N,W<:AbstractWorld,T<:TruthValue}
        FullDimensionalFrame{N,W,T,0,Tuple{}}(dims)
    end
    
    FullDimensionalFrame(dims::Tuple{}) = FullDimensionalFrame{0,OneWorld,Bool}(dims)
    FullDimensionalFrame(dims::Tuple{Int}) = FullDimensionalFrame{1,Interval,Bool}(dims)
    FullDimensionalFrame(dims::Tuple{Int,Int}) = FullDimensionalFrame{2,Interval2D,Bool}(dims)
end

Base.getindex(fr::FullDimensionalFrame, i::Int) = fr.dims[i]

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

accessibles(fr::Full0DFrame, ::Union{OneWorld,AbstractWorldSet{OneWorld}}, ::_RelationGlob) = [OneWorld()]

# _accessibles(fr::Full0DFrame, ::OneWorld, ::_RelationId) = [OneWorld()]

# Convenience functions: enumerate all & 1-length intervals in a given range
_intervals_in(a::Integer, b::Integer) = Iterators.filter(((x,y),)->x<y, Iterators.product(a:b-1, a+1:b))
intervals_in(a::Integer, b::Integer) = IterTools.imap(Interval, _intervals_in(a, b))
short_intervals_in(a::Integer, b::Integer) = IterTools.imap((x)->Interval(x,x+1), a:b-1)

accessibles(fr::Full1DFrame, ::Union{Interval,AbstractWorldSet{Interval}}, r::_RelationGlob) = intervals_in(1, X(fr)+1)

# Convenience function: enumerate all interval2Ds in a given range
intervals2D_in(a1::Integer, a2::Integer, b1::Integer, b2::Integer) = IterTools.imap(Interval2D, Iterators.product(_intervals_in(a1, a2), _intervals_in(b1, b2)))

accessibles(fr::Full2DFrame, ::Union{Interval2D,AbstractWorldSet{Interval2D}}, r::_RelationGlob) =
    intervals2D_in(1,X(fr)+1,1,Y(fr)+1)

############################################################################################

# Relations are defined via methods that return iterators to the accessible worlds.
# Each relation R<:AbstractRelation must provide a method for `accessibles`, which returns an iterator
#  to the worlds that are accessible from a given world w:
# `accessibles(fr::DimensionalFrame{N,W}, w::W,           r::R)::AbstractVector{W}`

# Alternatively, one can provide a *bare* definition, that is, method `_accessibles`,
#  returning an iterator of *tuples* which is then fed to a constructor of the same world type, as in:
# `_accessibles(fr::DimensionalFrame{N,W}, w::W,           r::R)::AbstractVector{Tuple}`

# The following fallback ensures that the two definitions are equivalent
accessibles(fr::DimensionalFrame{N,W}, w::W, r::AbstractRelation) where {N,W<:AbstractWorld} = begin
    IterTools.imap(W, _accessibles(fr, w, r))
end

#

# It is convenient to define methods for `accessibles` that take a world set instead of a
#  single world. Generally, this falls back to calling `_accessibles` on each world in
#  the set, and returning a constructor of wolds from the union; however, one may provide
#  improved implementations for special cases (e.g. ⟨L⟩ of a world set in interval algebra).
accessibles(fr::DimensionalFrame{N,W}, S::AbstractWorldSet{W}, r::AbstractRelation) where {N,W<:AbstractWorld} = begin
    IterTools.imap(W,
        IterTools.distinct(
            Iterators.flatten(
                (_accessibles(fr, w, r) for w in S)
            )
        )
    )
end

############################################################################################
# Singletons representing natural relations
############################################################################################

accessibles(fr::DimensionalFrame{N,W}, w::W,           ::_RelationId) where {N,W<:AbstractWorld} = [w] # TODO try IterTools.imap(identity, [w])
accessibles(fr::DimensionalFrame{N,W}, S::AbstractWorldSet{W}, ::_RelationId) where {N,W<:AbstractWorld} = S # TODO try IterTools.imap(identity, S)

############################################################################################

# Note: these methods must be defined for any newly defined world type WT:
# `accessibles(fr::DimensionalFrame{N,W}, w::WT,           ::_RelationGlob)`
# `accessibles(fr::DimensionalFrame{N,W}, S::AbstractWorldSet{WT}, ::_RelationGlob)`

############################################################################################

# Shortcuts using global relation for enumerating all worlds
allworlds(fr::DimensionalFrame{N,W}) where {N,W<:AbstractWorld} = accessibles(fr, W[], RelationGlob)

# Perhaps these help the compiler? TODO figure out if this helps
allworlds(fr::Full0DFrame) = [OneWorld()]

############################################################################################
############################################################################################
############################################################################################


include("Full1DFrame+IA.jl")
include("Full1DFrame+RCC.jl")

include("Full2DFrame+IA2.jl")
include("Full2DFrame+RCC.jl")
