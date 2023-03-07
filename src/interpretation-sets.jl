using SoleBase: AbstractDataset
using SoleBase: nsamples

import Base: getindex

"""
    abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

Abstract type for ordered sets of interpretations.
A set of interpretations, also referred to as a *dataset* in this context,
is a collection of *instances*, each of which is an interpretation, and is 
identified by an index i_sample::Integer.
These structures are especially useful when performing 
[model checking](https://en.m.wikipedia.org/wiki/Model_checking).

See also [`atomtype`](@ref), [`truthtype`](@ref),
[`InterpretationSet`](@ref).
"""
abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

# TODO improve general doc.
atomtype(::Type{AbstractInterpretationSet{M}}) where {M} = atomtype(M)
atomtype(s::AbstractInterpretationSet) = atomtype(M)

# TODO improve general doc.
truthtype(::Type{AbstractInterpretationSet{M}}) where {M} = atomtype(M)
truthtype(s::AbstractInterpretationSet) = atomtype(M)

function check(
    tok::AbstractSyntaxToken,
    X::AbstractInterpretationSet{M},
    i_sample::Integer,
    args...,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide method check(::$(typeof(tok)), ::$(typeof(X)), ::Integer, ::$(typeof(args))...).")
end

function check(
    φ::AbstractFormula,
    X::AbstractInterpretationSet{M},
    i_sample::Integer,
    args...,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide method check(::$(typeof(φ)), ::$(typeof(X)), ::Integer, ::$(typeof(args))...).")
end

# Check on a dataset = map check on the instances
function check(
    φ::Union{AbstractSyntaxToken,AbstractFormula},
    X::AbstractInterpretationSet{M},
    args...
)::Vector{truthtype(M)} where {M<:AbstractInterpretation}
    # TODO normalize before checking, if it is faster!
    [check(φ, X, i, args...) for i in 1:nsamples(X)]
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

Base.getindex(ms::InterpretationSet, i_sample) = Base.getindex(ms.instances, i_sample)
function check(
    f::Union{AbstractSyntaxToken,AbstractFormula},
    is::InterpretationSet,
    i_sample::Integer,
    args...
)
    check(f, is[i_sample], args...)
end

############################################################################################

# TODO
# abstract type AbstractFrameSet{FR<:AbstractFrame} end

# function Base.getindex(::AbstractFrameSet{FR}, i_sample)::FR where {FR<:AbstractFrame}
#     error("Please, provide ...")
# end

# struct FrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
#     frames::Vector{FR}
# end

# Base.getindex(ks::FrameSet, i_sample) = Base.getindex(ks.frames, i_sample)

# struct UniqueFrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
#     frame::FR
# end

# Base.getindex(ks::UniqueFrameSet, i_sample) = ks.frame
