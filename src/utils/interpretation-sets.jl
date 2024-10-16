# Implementations and utilities

interpretationtype(s::AbstractInterpretationSet) = interpretationtype(typeof(s))

valuetype(S::Type{<:AbstractInterpretationSet}) = valuetype(interpretationtype(S))
valuetype(s::AbstractInterpretationSet) = valuetype(typeof(s))

truthtype(S::Type{<:AbstractInterpretationSet}) = truthtype(interpretationtype(S))
truthtype(s::AbstractInterpretationSet) = truthtype(typeof(s))

# Fallback
function getinstance(s::AbstractInterpretationSet, i_instance::Integer)
    return LogicalInstance(s, i_instance)
end

function eachinstance(s::AbstractInterpretationSet)
    return (getinstance(s, i_instance) for i_instance in 1:ninstances(s))
end

struct LogicalInstance{S<:AbstractInterpretationSet} <: AbstractInterpretation
    s::S
    i_instance::Int64

    function LogicalInstance{S}(
        s::S,
        i_instance::Integer
    ) where {S<:AbstractInterpretationSet}
        new{S}(s, i_instance)
    end

    function LogicalInstance(
        s::AbstractInterpretationSet,
        i_instance::Integer
    )
        LogicalInstance{typeof(s)}(s, i_instance)
    end
end

splat(i::LogicalInstance) = (i.s, i.i_instance)

truthtype(i::LogicalInstance) = truthtype(i.s)

function check(
    φ::SyntaxTree,
    i::LogicalInstance,
    args...;
    kwargs...
)
    return istop(interpret(φ, i, args...; kwargs...))
end

function interpret(
    φ::Formula,
    s::AbstractInterpretationSet,
    i_instance::Integer,
    args...;
    kwargs...,
)
    check(φ, getinstance(s, i_instance), args...; kwargs...)
end

function interpret(
    φ::Formula,
    s::AbstractInterpretationSet,
    args...;
    kwargs...,
)
    map(i_instance->check(
        φ,
        getinstance(s, i_instance),
        args...;
        kwargs...
    ), 1:ninstances(s))
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

function check(
    φ::LeftmostConjunctiveForm,
    s::AbstractInterpretationSet,
    args...;
    kwargs...
)
    map(i_instance->check(
        φ,
        getinstance(s, i_instance),
        args...;
        kwargs...
    ), 1:ninstances(s))
end

function check(
    φ::LeftmostConjunctiveForm,
    s::AbstractInterpretationSet,
    i_instance::Integer,
    args...;
    kwargs...
)
    return all(ch -> check(ch, s, i_instance, args...; kwargs...), children(φ))
end

function check(
    φ::LeftmostConjunctiveForm,
    i::LogicalInstance,
    args...;
    kwargs...
)
    return all(ch -> check(ch, i, args...; kwargs...), children(φ))
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
    kwargs...,
)
    map(i_instance->check(
        φ,
        getinstance(s, i_instance),
        args...;
        kwargs...
    ), 1:ninstances(s))
end

"""
    struct InterpretationVector{M<:AbstractInterpretation} <: AbstractInterpretationSet
        instances::Vector{M}
    end

A dataset of interpretations instantiated as a vector.

[`AbstractInterpretationSet`](@ref).
"""
struct InterpretationVector{M<:AbstractInterpretation} <: AbstractInterpretationSet
    instances::Vector{M}
end

function interpretationtype(::Type{S}) where {M<:AbstractInterpretation,S<:InterpretationVector{M}}
    return error("Please, provide method interpretationtype(::$(typeof(S))).")
end

Base.getindex(s::InterpretationVector, i_instance::Integer) = Base.getindex(s.instances, i_instance)
getinstance(s::InterpretationVector, i_instance::Integer) = Base.getindex(s, i_instance)

struct FrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
    frames::Vector{FR}
end

Base.getindex(ks::FrameSet, i_instance::Integer) = Base.getindex(ks.frames, i_instance::Integer)

struct UniqueFrameSet{FR<:AbstractFrame} <: AbstractFrameSet{FR}
    frame::FR
end

Base.getindex(ks::UniqueFrameSet, i_instance::Integer) = ks.frame

# Helpers for (Multi-)modal logics

worldtype(S::Type{AbstractInterpretationSet}) = worldtype(interpretationtype(S))
worldtype(s::AbstractInterpretationSet) = worldtype(typeof(s))

frametype(S::Type{AbstractInterpretationSet}) = frametype(interpretationtype(S))
frametype(s::AbstractInterpretationSet) = frametype(typeof(s))

accessibles(s::AbstractInterpretationSet, i_instance::Integer, args...) = accessibles(frame(s, i_instance), args...)
allworlds(s::AbstractInterpretationSet, i_instance::Integer, args...) = allworlds(frame(s, i_instance), args...)
nworlds(s::AbstractInterpretationSet, i_instance::Integer) = nworlds(frame(s, i_instance))