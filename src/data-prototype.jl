

abstract type AbstractLogicalModelSet{M<:AbstractKripkeModel} where {M}

function Base.getindex(::AbstractLogicalModelSet{M}, i_instance::Int)::M where {M<:AbstractKripkeModel}
    error("Please, provide ...")
end

struct LogicalModelSet{M<:AbstractKripkeModel} <: AbstractLogicalModelSet{M}
    models::Vector{M}
end

Base.getindex(ms::LogicalModelSet, i_instance::Int) = Base.getindex(ms.models, i_instance)

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
