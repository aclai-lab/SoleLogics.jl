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

See also [`valuetype`](@ref), [`truthtype`](@ref),
[`InterpretationSet`](@ref).
"""
abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

# TODO improve general doc.
interpretationtype(::Type{AbstractInterpretationSet{M}}) where {M} = M
interpretationtype(s::AbstractInterpretationSet) = interpretationtype(typeof(s))

# TODO improve general doc.
valuetype(::Type{AbstractInterpretationSet{M}}) where {M} = valuetype(M)
valuetype(s::AbstractInterpretationSet) = valuetype(typeof(s))

# TODO improve general doc.
truthtype(::Type{AbstractInterpretationSet{M}}) where {M} = truthtype(M)
truthtype(s::AbstractInterpretationSet) = truthtype(typeof(s))

"""
TODO explain. In general, one cannot extract a single logical instance from a set, thus we represent it as a tuple of dataset + instance id (i_instance)
TODO this struct firm could be better, but unfortunately the following doesn't work:
    LogicalInstance{M<:AbstractInterpretation{A,T<:Truth}, S<:AbstractInterpretationSet{M}} <: AbstractInterpretation{A,T}
"""
struct LogicalInstance{
    A,
    T<:Truth,
    S<:AbstractInterpretationSet{AbstractInterpretation{A,T}} # TODO looks good; let's see if it works! Cuz, I'm not sure, maybe Julia requires you to replace this with {...,M<:AbstractInterpretation{A,T},AbstractInterpretationSet{M}}.
} <: AbstractInterpretation{A,T}

    s::S
    i_instance::Int64

    function LogicalInstance{A,T,S}(
        s::S,
        i_instance::Integer
    ) where {A,T<:Truth,S<:AbstractInterpretationSet{AbstractInterpretation{A,T}}}
        new{A,T,S}(s, i_instance)
    end

    function LogicalInstance(
        s::AbstractInterpretationSet,
        i_instance::Integer
    )
        LogicalInstance{valuetype(s),truthtype(s),typeof(s)}(s, i_instance)
    end
end

splat(i::LogicalInstance) = (i.s, i.i_instance)

function getinstance(s::AbstractInterpretationSet, i_instance::Integer)
    return LogicalInstance(s, i_instance)
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
    struct InterpretationSet{M<:AbstractInterpretation} <: AbstractInterpretationSet{M}
        instances::Vector{M}
    end

A dataset of interpretations instantiated as a vector.

[`AbstractInterpretationSet`](@ref).
"""
struct InterpretationSet{M<:AbstractInterpretation} <: AbstractInterpretationSet{M}
    instances::Vector{M}
end

Base.getindex(s::InterpretationSet, i_instance::Integer) = Base.getindex(s.instances, i_instance)
getinstance(s::InterpretationSet, i_instance::Integer) = Base.getindex(s, i_instance)

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

function alphabet(X::AbstractInterpretationSet{M}) where {M<:AbstractKripkeStructure}
    return error("Please, provide method alphabet(::$(typeof(X))).")
end

# function relations(X::AbstractInterpretationSet{M}) where {M<:AbstractKripkeStructure}
#     return error("Please, provide method relations(::$(typeof(X))).")
# end


function frame(X::AbstractInterpretationSet, i_instance::Integer)
    return frame(getinstance(X, i_instance))
end

function frame(X::AbstractInterpretationSet{M}, i_instance::Integer) where {M<:AbstractKripkeStructure}
    return error("Please, provide method frame(::$(typeof(X)), ::$(typeof(i_instance))).")
end
accessibles(X::AbstractInterpretationSet, i_instance::Integer, args...) = accessibles(frame(X, i_instance), args...)
allworlds(X::AbstractInterpretationSet, i_instance::Integer, args...) = allworlds(frame(X, i_instance), args...)
nworlds(X::AbstractInterpretationSet, i_instance::Integer) = nworlds(frame(X, i_instance))

function check(
    φ::SyntaxBranch{
        Union{
            DiamondRelationalOperator{typeof(tocenterrel)},
            BoxRelationalOperator{typeof(tocenterrel)},
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
            DiamondRelationalOperator{typeof(globalrel)},
            BoxRelationalOperator{typeof(globalrel)},
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
#             DiamondRelationalOperator{R},
#             BoxRelationalOperator{R},
#         }
#     },
#     i::AbstractInterpretation;
#     kwargs...
# ) where {R<:AbstractRelation}
#     rel = SoleLogics.relation(SoleLogics.token(φ))
#     check(first(children(φ)), i, accessibles(frame(i), rel); kwargs...)
# end
