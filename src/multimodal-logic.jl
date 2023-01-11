
export ismultimodal
export AbstractRelationalOperator, DiamondRelationalOperator, BoxRelationalOperator
export relationtype

############################################################################################
############################################################################################
############################################################################################

abstract type AbstractRelation end

abstract type AbstractRelationalOperator{R<:AbstractRelation} end

Base.operator_precedence(::AbstractRelationalOperator) = HIGH_PRIORITY

relationtype(::AbstractRelationalOperator{R}) where {R<:AbstractRelation} = R

struct DiamondRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end

struct BoxRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end

############################################################################################
######################################## BASE ##############################################
############################################################################################

# Named-relation type
struct NamedRelation <: AbstractRelation
    name::Symbol
    # adjacency matrix between NamedWorld's
end

"""
    abstract type AbstractMultiModalFrame{
        W<:AbstractWorld,
        R<:NTuple{N,Type{AbstractRelation}} where {N}
    } end

Abstract type representing a group of `AbstractModalFrame`.

See also [`AbstractModalFrame`](@ref).
"""
abstract type AbstractMultiModalFrame{
    W<:AbstractWorld,
    R<:NTuple{N,Type{AbstractRelation}} where {N} # Why "where" position is correct here? <-- TODO2: ?
    # TODO: you cannot place it outside, when defining an abstract type.
} end

"""
    abstract type AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

Wrapper used to manage many `AbstractRelation`ss using a specific `AbstractModalFrame` for
each of them.

See also [`AbstractRelation`](@ref), [`AbstractModalFrame`](@ref).

TODO2: The following is WrapperMultiModalFrame but the docstring refers to AbstractFrame
# TODO on it.
"""
struct WrapperMultiModalFrame{
    W<:AbstractWorld,
    R<:NTuple{N,Type{AbstractRelation}} where {N}
} <: AbstractMultiModalFrame{W,R}
    frames::OrderedDict{<:AbstractRelation,<:AbstractModalFrame} # Could be done better?
end

struct AdjacencyMultiModalFrame{
    W<:AbstractWorld,
    R<:NTuple{N,Type{AbstractRelation}} where {N}
} <: AbstractMultiModalFrame{W,R}
    adjacents::NamedArray # ...
end

# function enum_accessibles(...)

# TODO2: Here we cannot provide a "basic" multimodal logic, am I right? Because there are many of them, there is not canonical one.
# TODO on it, we'll see.
