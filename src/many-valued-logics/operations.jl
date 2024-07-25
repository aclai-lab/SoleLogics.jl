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
    struct BinaryOperation{T<:Truth,D<:AbstractVector{T},DI<:AbstractDict{Tuple{T,T},T}} <: Operation
        domain::D
        truthtable::DI
    end

A binary operation on a set S is a mapping of the elements of the Cartesian product
S × S → S. The closure property of a binary operation expresses the existence of a result
for the operation given any pair of operands. Binary operations are required to be defined
on all elements of S × S.

See also [`Operation`](@ref), [`arity`](@ref).
"""
struct BinaryOperation{T<:Truth,D<:AbstractVector{T},DI<:AbstractDict{Tuple{T,T},T}} <: AbstractBinaryOperation
    domain::D
    truthtable::DI

    function BinaryOperation(
        domain::D,
        truthtable::DI,
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        DI<:Dict{Tuple{T,T},T}
    }
        for i ∈ domain
            for j ∈ domain
                @assert (i, j) ∈ keys(truthtable) "truthtable[($i, $j)] is not defined."
            end
        end
        @assert length(truthtable) == length(domain)^2 "Found truthtable[(i, j)] where i " *
            "or j ∉ domain."
        return new{T,D,DI}(domain, truthtable)
    end
end

Base.show(io::IO, o::BinaryOperation) = print(io, "$(o.truthtable)")
arity(o::BinaryOperation) = 2

"""
    function getdomain(o::BinaryOperation)

Return the domain associated to binary operation `o`.

See also [`BinaryOperation`](@ref).
"""
getdomain(o::BinaryOperation) = o.domain

"""
    function (o::BinaryOperation{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractVector{T}}

Helper allowing to use binary operations with function notation.

See also [`Operation`](@ref), [`BinaryOperation`](@ref), [`arity`](@ref).
"""
function (o::BinaryOperation{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractVector{T}}
    return o.truthtable[(t1, t2)]
end
