

abstract type AbstractInterpretationSet{M<:AbstractKripkeStructure} where {M}

function Base.getindex(::AbstractInterpretationSet{M}, i_instance::Int)::M where {M<:AbstractKripkeStructure}
    error("Please, provide ...")
end

struct InterpretationSet{M<:AbstractKripkeStructure} <: AbstractInterpretationSet{M}
    instances::Vector{M}
end

Base.getindex(ms::InterpretationSet, i_instance::Int) = Base.getindex(ms.instances, i_instance)

abstract type AbstractFrameSet{FR<:AbstractFrame}

function Base.getindex(::AbstractFrameSet{FR}, i_instance::Int)::FR where {FR<:AbstractFrame}
    error("Please, provide ...")
end

struct FrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
    frames::Vector{FR}
end

Base.getindex(ks::FrameSet, i_instance::Int) = Base.getindex(ks.frames, i_instance)

struct UniqueFrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
    frame::FR
end

Base.getindex(ks::UniqueFrameSet, i_instance::Int) = ks.frame
