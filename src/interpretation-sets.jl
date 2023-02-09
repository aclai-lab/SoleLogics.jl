using SoleBase: AbstractDataset

import Base: getindex

abstract type AbstractInterpretationSet{M<:AbstractInterpretation} <: AbstractDataset end

atomtype(::Type{AbstractInterpretationSet{M}}) where {M} = atomtype(M)
atomtype(s::AbstractInterpretationSet) = atomtype(M)

truthtype(::Type{AbstractInterpretationSet{M}}) where {M} = atomtype(M)
truthtype(s::AbstractInterpretationSet) = atomtype(M)

function Base.getindex(
    ::AbstractInterpretationSet{M},
    i_sample,
)::M where {M<:AbstractInterpretation}
    error("Please, provide ...")
end

function check(
    ::AbstractInterpretation{M},
    i_sample,
    ::Proposition,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide ...")
end

function check(
    ::AbstractInterpretation{M},
    i_sample,
    ::AbstractFormula,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide ...")
end

struct InterpretationSet{M<:AbstractInterpretation} <: AbstractInterpretationSet{M}
    instances::Vector{M}
end

Base.getindex(ms::InterpretationSet, i_sample) = Base.getindex(ms.instances, i_sample)
check(is::InterpretationSet, i_sample, args...) = check(is[i_sample], args...)


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
