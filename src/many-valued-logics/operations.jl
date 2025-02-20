using StaticArrays
import SoleLogics: arity

"""
    abstract type Operation end

An operation is a function which takes zero or more operands to a well-defined output value.

See also [`BinaryOperation`](@ref), [`arity`](@ref).
"""
abstract type Operation end

abstract type AbstractBinaryOperation end

"""
    function Base.show(io::IO, o::O) where {O<:Operation}

Write a text representation of an operation `o` to the output stream `io`.

See also [`Operation`](@ref), [`BinaryOperation`](@ref), [`arity`](@ref).
"""
function Base.show(io::IO, o::O) where {O<:Operation}
    print(io, "$(typeof(o)) without a show function")
    @warn "Please, provide a show function for operation $(typeof(o))."
end

"""
    function arity(o::O) where {O<:Operation}

Return the arity of an operation `o`.

See also [`Operation`](@ref), [`BinaryOperation`](@ref), [`arity`](@ref).
"""
function arity(o::O) where {O<:Operation}
    error("Please, provide an arity for operation $o.")
end

"""
    struct BinaryOperation{N,M<:SMatrix{N,N,FiniteTruth}} <: AbstractBinaryOperation
        truthtable::M
    end

A binary operation on a set S is a mapping of the elements of the Cartesian product
S × S → S. The closure property of a binary operation expresses the existence of a result
for the operation given any pair of operands. Binary operations are required to be defined
on all elements of S × S.

See also [`Operation`](@ref), [`arity`](@ref).
"""
struct BinaryOperation{N,M<:SMatrix{N,N,FiniteTruth}} <: AbstractBinaryOperation
    truthtable::M

    function BinaryOperation{N}(truthtable::M) where {N,M<:SMatrix{N,N,FiniteTruth}}
        return new{N,M}(truthtable)
    end

    function BinaryOperation{N}(truthtable::AbstractVector{<:FiniteTruth}) where {N}
        return BinaryOperation{N}(SMatrix{N,N,FiniteTruth}(truthtable))
    end
end

Base.show(io::IO, o::BinaryOperation{N}) where {N} = print(io, "$(o.truthtable)")
arity(o::BinaryOperation{N}) where {N} = 2

"""
    function getdomain(o::BinaryOperation)

Return the domain associated to binary operation `o`.

See also [`BinaryOperation`](@ref).
"""
function getdomain(::BinaryOperation{N}) where {N}
    return SVector{N,FiniteTruth}(FiniteTruth.([1:N]...))
end

"""
Helper allowing to use binary operations with function notation.

See also [`Operation`](@ref), [`BinaryOperation`](@ref), [`arity`](@ref).
"""
@inline function (o::BinaryOperation{N})(t1::UInt8, t2::UInt8) where {N}
    return o.truthtable[t1, t2]
end
@inline function (o::BinaryOperation{N})(t1::FiniteTruth, t2::UInt8) where {N}
    return o.truthtable[t1.index, t2]
end
@inline function (o::BinaryOperation{N})(t1::UInt8, t2::FiniteTruth) where {N}
    return o.truthtable[t1, t2.index]
end
@inline function (o::BinaryOperation{N})(t1::FiniteTruth, t2::FiniteTruth) where {N}
    return o.truthtable[t1.index, t2.index]
end
