using SoleBase: AbstractDataset
import SoleBase: nsamples

import Base: getindex

abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

atomtype(::Type{AbstractInterpretationSet{M}}) where {M} = atomtype(M)
atomtype(s::AbstractInterpretationSet) = atomtype(M)

truthtype(::Type{AbstractInterpretationSet{M}}) where {M} = atomtype(M)
truthtype(s::AbstractInterpretationSet) = atomtype(M)

# function Base.getindex(
#     ::AbstractInterpretationSet{M},
#     i_sample,
# )::M where {M<:AbstractInterpretation}
#     error("Please, provide ... TODO")
# end

# TODO maybe args... is not necessary?

function check(
    p::Proposition,
    X::AbstractInterpretationSet{M},
    i_sample,
    args...,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide method check(::$(typeof(p)), ::$(typeof(X)), i_sample).")
end

# function check(
#     φ::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
#     X::AbstractInterpretationSet{M},
#     i_sample,
# )::truthtype(M) where {M<:AbstractInterpretation}
#     error("Please, provide method check(::$(typeof(φ)), ::$(typeof(X)), i_sample).")
# end

function check(
    φ::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
    X::AbstractInterpretationSet{M},
    i_sample,
    args...,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide method check(::$(typeof(φ)), ::$(typeof(X)), i_sample).")
end

function check(
    φ::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
    X::AbstractInterpretationSet{M},
)::Vector{truthtype(M)} where {M<:AbstractInterpretation}
    [check(φ, X, i) for i in 1:nsamples(X)]
end

# ############################################################################################

# struct InterpretationSet{M<:AbstractInterpretation} <: AbstractInterpretationSet{M}
#     instances::Vector{M}
# end

# Base.getindex(ms::InterpretationSet, i_sample) = Base.getindex(ms.instances, i_sample)
# check(f::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula}, is::InterpretationSet, i_sample, args...) = check(f, is[i_sample], args...)


############################################################################################

abstract type AbstractFrameSet{FR<:AbstractFrame} end

function Base.getindex(::AbstractFrameSet{FR}, i_sample)::FR where {FR<:AbstractFrame}
    error("Please, provide ...")
end

struct FrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
    frames::Vector{FR}
end

Base.getindex(ks::FrameSet, i_sample) = Base.getindex(ks.frames, i_sample)

struct UniqueFrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
    frame::FR
end

Base.getindex(ks::UniqueFrameSet, i_sample) = ks.frame
