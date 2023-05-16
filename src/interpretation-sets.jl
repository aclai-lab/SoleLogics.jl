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
[model checking](https://en.wikipedia.org/wiki/Model_checking).

See also [`atomtype`](@ref), [`truthtype`](@ref),
[`InterpretationSet`](@ref).
"""
abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

# TODO improve general doc.
atomtype(::Type{AbstractInterpretationSet{M}}) where {M} = atomtype(M)
atomtype(s::AbstractInterpretationSet) = atomtype(M)

# TODO improve general doc.
truthtype(::Type{AbstractInterpretationSet{M}}) where {M} = truthtype(M)
truthtype(s::AbstractInterpretationSet) = truthtype(M)

function check(
    tok::AbstractSyntaxToken,
    d::AbstractInterpretationSet{M},
    i_sample::Integer,
    args...;
    kwargs...,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide method check(::$(typeof(tok)), ::$(typeof(d)), ::Integer, ::$(typeof(args))...; kwargs...).")
end

function check(
    φ::AbstractFormula,
    d::AbstractInterpretationSet{M},
    i_sample::Integer,
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
    map(i_sample->check(
        φ,
        d,
        i_sample,
        args...;
        # use_memo = (isnothing(use_memo) ? nothing : use_memo[[i_sample]]),
        kwargs...
    ), 1:nsamples(d))
    # map(
    #     i_sample->check(
    #         formula(c),
    #         slice_dataset(d, [i_sample]),
    #         args...;
    #         use_memo = (isnothing(use_memo) ? nothing : @view use_memo[[i_sample]]),
    #         kwargs...,
    #     )[1], 1:nsamples(d)
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
