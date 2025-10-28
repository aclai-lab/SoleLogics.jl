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
        operation =  BinaryOperation{N}(SMatrix{N,N,FiniteTruth}(truthtable))
        if !checkaxiom(Commutativity, operation)
            @warn "Non commutative operation defined with `AbstractVector`` constructor!\n" *
                  "Please, check that indices are in the intended order.\n" *
                  "If you don't know what you're doing, use the `SMatrix` constructor instead."
        end
        return operation
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
@inline function (o::BinaryOperation{N})(t1::T, t2::UInt8) where {N, T<:Truth}
    if !isa(t1, FiniteTruth) t1 = convert(FiniteTruth, t1)::FiniteTruth end
    return o.truthtable[t1.index, t2]
end
@inline function (o::BinaryOperation{N})(t1::UInt8, t2::T) where {N, T<:Truth}
    if !isa(t2, FiniteTruth) t2 = convert(FiniteTruth, t2)::FiniteTruth end
    return o.truthtable[t1, t2.index]
end
@inline function (o::BinaryOperation{N})(t1::T1, t2::T2) where {N, T1<:Truth, T2<:Truth}
    if !isa(t1, FiniteTruth) t1 = convert(FiniteTruth, t1)::FiniteTruth end
    if !isa(t2, FiniteTruth) t2 = convert(FiniteTruth, t2)::FiniteTruth end
    return o.truthtable[t1.index, t2.index]
end

# TODO: Write documentation
struct ContinuousBinaryOperation{F<:Function} <: AbstractBinaryOperation 
    func::F

    function ContinuousBinaryOperation(func::F) where {F<:Function}
        return new{F}(func)
    end
end

arity(o::ContinuousBinaryOperation) = 2

# Not sure if there's any need of having these definitions for generic Float64's

@inline function (o::ContinuousBinaryOperation)(t1::Float64, t2::Float64)
    return ContinuousTruth(o.func(t1, t2))
end

@inline function (o::ContinuousBinaryOperation)(t1::T, t2::Float64) where {T<:Truth}
    if !isa(t1, ContinuousTruth) t1 = convert(ContinuousTruth, t1)::ContinuousTruth end
    return ContinuousTruth(o.func(t1.value, t2))
end

@inline function (o::ContinuousBinaryOperation)(t1::Float64, t2::T) where {T<:Truth}
    if !isa(t2, ContinuousTruth) t2 = convert(ContinuousTruth, t2)::ContinuousTruth end
    return ContinuousTruth(o.func(t1, t2.value))
end

@inline function (o::ContinuousBinaryOperation)(t1::T, t2::T) where {T<:Truth}
    if !isa(t1, ContinuousTruth) && !isa(t2, ContinuousTruth)
        t1 = convert(ContinuousTruth, t1)::ContinuousTruth
        t2 = convert(ContinuousTruth, t2)::ContinuousTruth
    end
    return ContinuousTruth(o.func(t1.value, t2.value))
end
