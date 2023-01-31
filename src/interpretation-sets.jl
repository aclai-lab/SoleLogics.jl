

abstract type AbstractInterpretationSet{M<:AbstractInterpretation} end

function Base.getindex(
    ::AbstractInterpretationSet{M},
    instance_id,
)::M where {M<:AbstractInterpretation}
    error("Please, provide ...")
end

function check(
    ::AbstractInterpretation{M},
    instance_id,
    ::Proposition,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide ...")
end

function check(
    ::AbstractInterpretation{M},
    instance_id,
    ::AbstractFormula,
)::truthtype(M) where {M<:AbstractInterpretation}
    error("Please, provide ...")
end

struct InterpretationSet{M<:AbstractInterpretation} <: AbstractInterpretationSet{M}
    instances::Vector{M}
end

Base.getindex(ms::InterpretationSet, instance_id) = Base.getindex(ms.instances, instance_id)
check(is::InterpretationSet, instance_id, args...) = check(is[instance_id], args...)


############################################################################################

abstract type AbstractFrameSet{FR<:AbstractFrame} end

function Base.getindex(::AbstractFrameSet{FR}, instance_id)::FR where {FR<:AbstractFrame}
    error("Please, provide ...")
end

struct FrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
    frames::Vector{FR}
end

Base.getindex(ks::FrameSet, instance_id) = Base.getindex(ks.frames, instance_id)

struct UniqueFrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
    frame::FR
end

Base.getindex(ks::UniqueFrameSet, instance_id) = ks.frame
