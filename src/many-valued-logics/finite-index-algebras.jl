using ..SoleLogics: AbstractAlgebra
using StaticArrays
import ..SoleLogics: syntaxstring, istop, isbot
import Base: convert

############################################################################################
#### Finite index truth ####################################################################
############################################################################################

struct FiniteIndexTruth <: Truth
    index::UInt8

    function FiniteIndexTruth(index::UInt8)
        return new(index)
    end

    function FiniteIndexTruth(index::T) where {T<:Int}
        return new(convert(UInt8, index))
    end
end

istop(t::FiniteIndexTruth) = t.index == UInt8(1)
isbot(t::FiniteIndexTruth) = t.index == UInt8(2)

function syntaxstring(t::FiniteIndexTruth)
    if t.index < UInt8(3)
        return Char(UInt16(8867) + t.index)
    else
        return Char(UInt16(942) + t.index)
    end
end

Base.show(io::IO, t::FiniteIndexTruth) = print(io, syntaxstring(t))

function Base.convert(::Type{FiniteIndexTruth}, t::BooleanTruth)
    return istop(t) ? FiniteIndexTruth(UInt8(1)) : FiniteIndexTruth(UInt8(2))
end

############################################################################################
#### Binary index operation ################################################################
############################################################################################

struct BinaryIndexOperation{N} <: Operation
    domain::SVector{N, FiniteIndexTruth}
    truthtable::SMatrix{N, N, FiniteIndexTruth}

    function BinaryIndexOperation{N}(
        domain::SVector{N, FiniteIndexTruth},
        truthtable::SMatrix{N, N, FiniteIndexTruth}
    ) where {
        N
    }
        return new{N}(domain, truthtable)
    end
end

Base.show(io::IO, o::BinaryIndexOperation{N}) where {N} = print(io, "$(o.truthtable)")
arity(o::BinaryIndexOperation{N}) where {N} = 2

function (o::BinaryIndexOperation{N})(t1::T1, t2::T2) where {N, T1<:Truth, T2<:Truth}
    if !isa(t1, FiniteIndexTruth) t1 = convert(FiniteIndexTruth, t1) end
    if !isa(t2, FiniteIndexTruth) t2 = convert(FiniteIndexTruth, t2) end
    return o.truthtable[t1.index, t2.index]
end