

abstract type AbstractInterpretationSet{M<:AbstractKripkeStructure} where {M}

function Base.getindex(::AbstractInterpretationSet{M}, i_instance::Int)::M where {M<:AbstractKripkeStructure}
    error("Please, provide ...")
end

struct InterpretationSet{M<:AbstractKripkeStructure} <: AbstractInterpretationSet{M}
    instances::Vector{M}
end

Base.getindex(ms::InterpretationSet, i_instance::Int) = Base.getindex(ms.instances, i_instance)

abstract type AbstractFrameSet{KF<:AbstractFrame}

function Base.getindex(::AbstractFrameSet{KF}, i_instance::Int)::KF where {KF<:AbstractFrame}
    error("Please, provide ...")
end

struct FrameSet{KF<:AbstractFrame} <: AbstractFrameSet{KF}
    frames::Vector{KF}
end

Base.getindex(ks::FrameSet, i_instance::Int) = Base.getindex(ks.frames, i_instance)

struct UniqueFrameSet{KF<:AbstractFrame} <: AbstractFrameSet{KF}
    frame::KF
end

Base.getindex(ks::UniqueFrameSet, i_instance::Int) = ks.frame
