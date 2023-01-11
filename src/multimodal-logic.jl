
export ismultimodal
export AbstractRelationalOperator, DiamondRelationalOperator, BoxRelationalOperator
export relationtype

############################################################################################
############################################################################################
############################################################################################

abstract type AbstractRelation end

abstract type AbstractRelationalOperator{R<:AbstractRelation} <: AbstractOperator end
# TODO: why the type parameter?
# TODO-reply: We want to dispatch on it. In this case, because different relations
#  carry different algorithmic behaviors (e.g., Later vs. After are computed in a
#  different way).

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
    R<:NTuple{N,Type{AbstractRelation}} where {N} # Why "where" position is correct here?
} end

"""
    abstract type AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

Wrapper used to manage many `AbstractRelation`'s using a specific `AbstractModalFrame` for
each of them.

See also [`AbstractRelation`](@ref), [`AbstractModalFrame`](@ref).
"""
struct WrapperMultiModalFrame{
    W<:AbstractWorld,
    R<:NTuple{N,Type{AbstractRelation}} where {N}
} <: AbstractMultiModalFrame{W,R}
    frames::OrderedDict{<:AbstractRelation, <:AbstractModalFrame} # Could be done better?
end

struct AdjacencyMultiModalFrame{
    W<:AbstractWorld,
    R<:NTuple{N,Type{AbstractRelation}} where {N}
} <: AbstractMultiModalFrame{W,R}
    adjacents::NamedArray # ...
end

# function enum_accessibles(...)
