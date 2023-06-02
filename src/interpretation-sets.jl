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

See also [`atomtype`](@ref), [`truthtype`](@ref),
[`InterpretationSet`](@ref).
"""
abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

# TODO improve general doc.
atomtype(::Type{AbstractInterpretationSet{M}}) where {M} = atomtype(M)
atomtype(s::AbstractInterpretationSet) = atomtype(typeof(s))

# TODO improve general doc.
truthtype(::Type{AbstractInterpretationSet{M}}) where {M} = truthtype(M)
truthtype(s::AbstractInterpretationSet) = truthtype(typeof(s))

function check(
    tok::AbstractSyntaxToken,
    d::AbstractInterpretationSet{M},
    i_instance::Integer,
    args...;
    kwargs...,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide method check(::$(typeof(tok)), ::$(typeof(d)), ::Integer, ::$(typeof(args))...; kwargs...).")
end

function check(
    φ::AbstractFormula,
    d::AbstractInterpretationSet{M},
    i_instance::Integer,
    args...;
    kwargs...,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide method check(::$(typeof(φ)), ::$(typeof(d)), ::Integer, ::$(typeof(args))...; kwargs...).")
end

# Check on a dataset = map check on the instances
function check(
    φ::Union{AbstractSyntaxToken,AbstractFormula},
    d::AbstractInterpretationSet{M},
    args...;
    # use_memo::Union{Nothing,AbstractVector} = nothing,
    kwargs...,
)::Vector{truthtype(M)} where {M<:AbstractInterpretation}
    # TODO normalize before checking, if it is faster!
    # φ = SoleLogics.normalize()
    # # TODO use get_instance instead?
    map(i_instance->check(
        φ,
        d,
        i_instance,
        args...;
        # use_memo = (isnothing(use_memo) ? nothing : use_memo[[i_instance]]),
        kwargs...
    ), 1:ninstances(d))
    # map(
    #     i_instance->check(
    #         formula(c),
    #         slice_dataset(d, [i_instance]),
    #         args...;
    #         use_memo = (isnothing(use_memo) ? nothing : @view use_memo[[i_instance]]),
    #         kwargs...,
    #     )[1], 1:ninstances(d)
    # )
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

Base.getindex(ms::InterpretationSet, i_instance) = Base.getindex(ms.instances, i_instance)
function check(
    f::Union{AbstractSyntaxToken,AbstractFormula},
    is::InterpretationSet,
    i_instance::Integer,
    args...
)
    check(f, is[i_instance], args...)
end

############################################################################################

# TODO
# abstract type AbstractFrameSet{FR<:AbstractFrame} end

# function Base.getindex(::AbstractFrameSet{FR}, i_instance)::FR where {FR<:AbstractFrame}
#     error("Please, provide ...")
# end

# struct FrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
#     frames::Vector{FR}
# end

# Base.getindex(ks::FrameSet, i_instance) = Base.getindex(ks.frames, i_instance)

# struct UniqueFrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
#     frame::FR
# end

# Base.getindex(ks::UniqueFrameSet, i_instance) = ks.frame

############################################################################################
############################# Helpers for (Multi-)modal logics #############################
############################################################################################

worldtype(::Type{AbstractInterpretationSet{M}}) where {M<:AbstractKripkeStructure} = worldtype(M)
worldtype(s::AbstractInterpretationSet) = worldtype(typeof(s))

frametype(::Type{AbstractInterpretationSet{M}}) where {M<:AbstractKripkeStructure} = frametype(M)
frametype(s::AbstractInterpretationSet) = frametype(typeof(s))

function alphabet(X::AbstractInterpretationSet{M}) where {M<:AbstractKripkeStructure}
    error("Please, provide method alphabet(::$(typeof(X))).")
end

function relations(X::AbstractInterpretationSet{M}) where {M<:AbstractKripkeStructure}
    error("Please, provide method relations(::$(typeof(X))).")
end


function frame(X::AbstractInterpretationSet{M}, i_instance) where {M<:AbstractKripkeStructure}
    error("Please, provide method frame(::$(typeof(X)), ::$(typeof(i_instance))).")
end
accessibles(X::AbstractInterpretationSet, i_instance, args...) = accessibles(frame(X, i_instance), args...)
allworlds(X::AbstractInterpretationSet, i_instance, args...) = allworlds(frame(X, i_instance), args...)
nworlds(X::AbstractInterpretationSet, i_instance) = nworlds(frame(X, i_instance))

