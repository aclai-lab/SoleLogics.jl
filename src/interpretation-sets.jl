using SoleBase: AbstractDataset
using SoleBase: ninstances

import Base: getindex

"""
    abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

Abstract type for ordered sets of interpretations.
A set of interpretations, also referred to as a *dataset* in this context,
is a collection of *instances*, each of which is an interpretation, and is
identified by an index i_instance::Integer.
These structures are especially useful when performing
[model checking](https://en.wikipedia.org/wiki/Model_checking).

See also[`truthtype`](@ref),
[`InterpretationVector`](@ref).
"""
abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

# TODO improve general doc.
interpretationtype(::Type{S}) where {M,S<:AbstractInterpretationSet{M}} = M
interpretationtype(s::AbstractInterpretationSet) = interpretationtype(typeof(s))

# TODO improve general doc.
valuetype(::Type{S}) where {M,S<:AbstractInterpretationSet{M}} = valuetype(M)
valuetype(s::AbstractInterpretationSet) = valuetype(typeof(s))

# TODO improve general doc.
truthtype(::Type{S}) where {M,S<:AbstractInterpretationSet{M}} = truthtype(M)
truthtype(s::AbstractInterpretationSet) = truthtype(typeof(s))

"""
    alphabet(s::AbstractInterpretationSet)::Alphabet

Return the propositional alphabet of an interpretation set.

See also [`AbstractAlphabet`](@ref), [`AbstractGrammar`](@ref).
"""
function alphabet(s::AbstractInterpretationSet)::Alphabet
    return error("Please, provide method alphabet(::$(typeof(s))).")
end

# Fallback
function getinstance(s::AbstractInterpretationSet, i_instance::Integer)
    return LogicalInstance(s, i_instance)
end

"""
    struct LogicalInstance{S<:AbstractInterpretationSet}
        s::S
        i_instance::Int64
    end

Object representing the i-th interpretation of an interpretation set.

In general, one may not be able to extract a single logical instance from a
set; thus, this representation, holding the interpretation set + instance id (i_instance),
can come handy in defining `check` and `interpret` methods for newly defined interpretation
set structures.
"""
# TODO: struct LogicalInstance{S<:AbstractInterpretationSet{M}} <: M
struct LogicalInstance{S<:AbstractInterpretationSet} <: AbstractInterpretation

    s::S
    i_instance::Int64

    # function LogicalInstance{M,S}(
    #     s::S,
    #     i_instance::Integer
    # ) where {M<:AbstractInterpretation,S<:AbstractInterpretationSet{M}}
    #     new{M,S}(s, i_instance)
    # end

    function LogicalInstance{S}(
        s::S,
        i_instance::Integer
    ) where {S<:AbstractInterpretationSet}
        new{S}(s, i_instance)
    end

    function LogicalInstance(
        s::AbstractInterpretationSet,
        i_instance::Integer
    )
        # LogicalInstance{interpretationtype(s),typeof(s)}(s, i_instance)
        LogicalInstance{typeof(s)}(s, i_instance)
    end
end

splat(i::LogicalInstance) = (i.s, i.i_instance)

truthtype(i::LogicalInstance) = truthtype(i.s)

function interpret(
    φ::Atom,
    i::LogicalInstance,
    args...;
    kwargs...
)::Formula
    return error("Please, provide method " *
        "interpret(φ::Atom, i::$(typeof(i)), " *
        "args...::$(typeof(args)); " *
        "kwargs...::$(typeof(kwargs))).")
end

function check(
    φ::Formula,
    i::LogicalInstance,
    args...;
    kwargs...
)
    return istop(interpret(φ, i, args...; kwargs...))
    # return check(tree(φ), i, args...; kwargs...)
end

# function check(
#     φ::SyntaxTree,
#     i::LogicalInstance,
#     args...;
#     kwargs...
# )
#     return error("Please, provide method " *
#         "check(φ::SyntaxTree, i::$(typeof(i)), " *
#         "args...::$(typeof(args)); " *
#         "kwargs...::$(typeof(kwargs))).")
# end

function interpret(
    φ::Formula,
    s::AbstractInterpretationSet,
    i_instance::Integer,
    args...;
    kwargs...,
)
    check(φ, getinstance(s, i_instance), args...; kwargs...)
end

function interpret(
    φ::Formula,
    s::AbstractInterpretationSet,
    args...;
    # use_memo::Union{Nothing,AbstractVector} = nothing,
    kwargs...,
)
    # TODO normalize before checking, if it is faster: φ = SoleLogics.normalize()
    map(i_instance->check(
        φ,
        getinstance(s, i_instance),
        args...;
        # use_memo = (isnothing(use_memo) ? nothing : use_memo[[i_instance]]),
        kwargs...
    ), 1:ninstances(s))
end

"""
    check(
        φ::Formula,
        s::AbstractInterpretationSet,
        i_instance::Integer,
        args...;
        kwargs...
    )::Bool

Check a formula on the \$i\$-th instance of an [`AbstractInterpretationSet`](@ref).

See also [`AbstractInterpretationSet`](@ref),
[`Formula`](@ref).
"""
function check(
    φ::Formula,
    s::AbstractInterpretationSet,
    i_instance::Integer,
    args...;
    kwargs...,
)
    check(φ, getinstance(s, i_instance), args...; kwargs...)
end

function check(
    φ::LeftmostConjunctiveForm,
    s::AbstractInterpretationSet,
    args...;
    kwargs...
)
    # TODO normalize before checking, if it is faster: φ = SoleLogics.normalize()
    map(i_instance->check(
        φ,
        getinstance(s, i_instance),
        args...;
        # use_memo = (isnothing(use_memo) ? nothing : use_memo[[i_instance]]),
        kwargs...
    ), 1:ninstances(s))
end

function check(
    φ::LeftmostConjunctiveForm,
    s::AbstractInterpretationSet,
    i_instance::Integer,
    args...;
    kwargs...
)
    return all(ch -> check(ch, s, i_instance, args...; kwargs...), children(φ))
end

function check(
    φ::LeftmostConjunctiveForm,
    i::LogicalInstance,
    args...;
    kwargs...
)
    return all(ch -> check(ch, i, args...; kwargs...), children(φ))
end


"""
    check(
        φ::Formula,
        s::AbstractInterpretationSet,
        args...;
        kwargs...
    )::Vector{Bool}

Check a formula on all instances of an [`AbstractInterpretationSet`](@ref).

See also [`AbstractInterpretationSet`](@ref),
[`Formula`](@ref).
"""
function check(
    φ::Formula,
    s::AbstractInterpretationSet,
    args...;
    # use_memo::Union{Nothing,AbstractVector} = nothing,
    kwargs...,
)
    # TODO normalize before checking, if it is faster: φ = SoleLogics.normalize()
    map(i_instance->check(
        φ,
        getinstance(s, i_instance),
        args...;
        # use_memo = (isnothing(use_memo) ? nothing : use_memo[[i_instance]]),
        kwargs...
    ), 1:ninstances(s))
end

############################################################################################

"""
    struct InterpretationVector{M<:AbstractInterpretation} <: AbstractInterpretationSet{M}
        instances::Vector{M}
    end

A dataset of interpretations instantiated as a vector.

[`AbstractInterpretationSet`](@ref).
"""
struct InterpretationVector{M<:AbstractInterpretation} <: AbstractInterpretationSet{M}
    instances::Vector{M}
end

Base.getindex(s::InterpretationVector, i_instance::Integer) = Base.getindex(s.instances, i_instance)
getinstance(s::InterpretationVector, i_instance::Integer) = Base.getindex(s, i_instance)

############################################################################################

# TODO
# abstract type AbstractFrameSet{FR<:AbstractFrame} end

# function Base.getindex(::AbstractFrameSet{FR}, i_instance::Integer)::FR where {FR<:AbstractFrame}
#     return error("Please, provide ...")
# end

# struct FrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
#     frames::Vector{FR}
# end

# Base.getindex(ks::FrameSet, i_instance::Integer) = Base.getindex(ks.frames, i_instance::Integer)

# struct UniqueFrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
#     frame::FR
# end

# Base.getindex(ks::UniqueFrameSet, i_instance::Integer) = ks.frame

############################################################################################
############################# Helpers for (Multi-)modal logics #############################
############################################################################################

worldtype(::Type{AbstractInterpretationSet{M}}) where {M<:AbstractKripkeStructure} = worldtype(M)
worldtype(s::AbstractInterpretationSet) = worldtype(typeof(s))

frametype(::Type{AbstractInterpretationSet{M}}) where {M<:AbstractKripkeStructure} = frametype(M)
frametype(s::AbstractInterpretationSet) = frametype(typeof(s))

# function relations(s::AbstractInterpretationSet{M}) where {M<:AbstractKripkeStructure}
#     return error("Please, provide method relations(::$(typeof(s))).")
# end

function frame(s::AbstractInterpretationSet, i_instance::Integer)
    return frame(getinstance(s, i_instance))
end

function frame(s::AbstractInterpretationSet{M}, i_instance::Integer) where {M<:AbstractKripkeStructure}
    return error("Please, provide method frame(::$(typeof(s)), ::$(typeof(i_instance))).")
end
accessibles(s::AbstractInterpretationSet, i_instance::Integer, args...) = accessibles(frame(s, i_instance), args...)
allworlds(s::AbstractInterpretationSet, i_instance::Integer, args...) = allworlds(frame(s, i_instance), args...)
nworlds(s::AbstractInterpretationSet, i_instance::Integer) = nworlds(frame(s, i_instance))

function check(
    φ::SyntaxBranch{
        Union{
            DiamondRelationalConnective{typeof(tocenterrel)},
            BoxRelationalConnective{typeof(tocenterrel)},
        }
    },
    i::AbstractInterpretation;
    kwargs...
)
    check(first(children(φ)), i, centralworld(frame(i)); kwargs...)
end

function check(
    φ::SyntaxBranch{
        Union{
            DiamondRelationalConnective{typeof(globalrel)},
            BoxRelationalConnective{typeof(globalrel)},
        }
    },
    i::AbstractInterpretation;
    kwargs...
)
    check(first(children(φ)), i, nothing; kwargs...)
end

# # General grounding
# function check(
#     φ::SyntaxBranch{
#         Union{
#             DiamondRelationalConnective{R},
#             BoxRelationalConnective{R},
#         }
#     },
#     i::AbstractInterpretation;
#     kwargs...
# ) where {R<:AbstractRelation}
#     rel = SoleLogics.relation(SoleLogics.token(φ))
#     check(first(children(φ)), i, accessibles(frame(i), rel); kwargs...)
# end
