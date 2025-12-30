import SoleBase: ninstances

"""
    struct LogicalInstance{S<:InterpretationSet}
        s::S
        i_instance::Int
    end

Object representing the i-th interpretation of an interpretation set.

In general, one may not be able to extract a single logical instance from a
set; thus, this representation, holding the interpretation set + instance id (i_instance),
can come handy in defining `check` and `interpret` methods for newly defined interpretation
set structures.
"""
struct LogicalInstance{S<:InterpretationSet} <: Interpretation

    s::S
    i_instance::Int

    function LogicalInstance{S}(
        s::S,
        i_instance::Integer
    ) where {S<:InterpretationSet}
        new{S}(s, i_instance)
    end

    function LogicalInstance(
        s::InterpretationSet,
        i_instance::Integer
    )
        # LogicalInstance{interpretationtype(s),typeof(s)}(s, i_instance)
        LogicalInstance{typeof(s)}(s, i_instance)
    end
end

splat(i::LogicalInstance) = (i.s, i.i_instance)

truthtype(i::LogicalInstance) = truthtype(i.s)

function interpret(
    φ::Atom,
    i::LogicalInstance,
    args...;
    kwargs...
)::Formula
    return error("Please, provide method " *
        "interpret(φ::Atom, i::$(typeof(i)), " *
        join(map(t->"::$(t)", typeof.(args)), ", ") * "; " *
        join(map(p->"$(p.first)::$(p.second)", kwargs), ", ") * ").")
end

function check(
    algo::CheckAlgorithm,
    φ::Formula,
    i::LogicalInstance,
    args...;
    kwargs...
)
    return error("Please, provide method " *
        "check(algo::$(typeof(algo)), φ::Formula, i::$(typeof(i)), " *
        join(map(t->"::$(t)", typeof.(args)), ", ") * "; " *
        join(map(p->"$(p.first)::$(p.second)", kwargs), ", ") * ").")
end

# # General grounding
# function check(
#     φ::SyntaxTree,
#     i::LogicalInstance{InterpretationSet};
#     kwargs...
# )
#     if token(φ) isa Union{DiamondRelationalConnective,BoxRelationalConnective}
#         rel = SoleLogics.relation(SoleLogics.token(φ))
#         if rel == tocenterrel
#             check(first(children(φ)), i, centralworld(frame(i)); kwargs...)
#         elseif rel == globalrel
#             check(first(children(φ)), i, AnyWorld(); kwargs...)
#         else
#             check(first(children(φ)), i, accessibles(frame(i), rel); kwargs...)
#         end
#     else
#         error("Unexpected formula: $φ!")
#     end
# end

function interpret(
    φ::Formula,
    s::InterpretationSet,
    i_instance::Integer,
    args...;
    kwargs...,
)
    interpret(φ, getinstance(s, i_instance), args...; kwargs...)
end

function interpret(
    φ::Formula,
    s::InterpretationSet,
    args...;
    # use_memo::Union{Nothing,AbstractVector} = nothing,
    kwargs...,
)
    # TODO normalize before checking, if it is faster: φ = SoleLogics.normalize()
    map(i_instance->check(
        φ,
        getinstance(s, i_instance),
        args...;
        # use_memo = (isnothing(use_memo) ? nothing : use_memo[[i_instance]]),
        kwargs...
    ), 1:ninstances(s))
end

"""
    check(
        [algo::CheckAlgorithm,]
        φ::Formula,
        s::InterpretationSet,
        i_instance::Integer,
        args...;
        kwargs...
    )::Bool

Check a formula on the \$i\$-th instance of an [`InterpretationSet`](@ref).

See also [`InterpretationSet`](@ref),
[`Formula`](@ref).
"""
function check(
    algo::CheckAlgorithm, 
    φ::Formula,
    s::InterpretationSet,
    i_instance::Integer,
    args...;
    kwargs...,
)
    check(algo, φ, getinstance(s, i_instance), args...; kwargs...)
end

"""
    check(
        [algo::CheckAlgorithm,]
        φ::Formula,
        s::InterpretationSet,
        args...;
        kwargs...
    )::Vector{Bool}

Check a formula on all instances of an [`InterpretationSet`](@ref).

See also [`InterpretationSet`](@ref),
[`Formula`](@ref).
"""
function check(
    algo::CheckAlgorithm,
    φ::Formula,
    s::InterpretationSet,
    args...;
    # use_memo::Union{Nothing,AbstractVector} = nothing,
    kwargs...,
)
    # TODO normalize before checking, if it is faster: φ = SoleLogics.normalize()
    map(i_instance->check(
        algo,
        φ,
        getinstance(s, i_instance),
        args...;
        # use_memo = (isnothing(use_memo) ? nothing : use_memo[[i_instance]]),
        kwargs...
    ), 1:ninstances(s))
end

function check(
    algo::CheckAlgorithm,
    φ::SyntaxBranch,
    i::LogicalInstance,
    args...;
    kwargs...
)
    return istop(interpret(φ, i, args...; kwargs...))
end

# Fallback
function getinstance(s::InterpretationSet, i_instance::Integer)
    return LogicalInstance(s, i_instance)
end

############################################################################################

"""
    struct InterpretationVector{M<:Interpretation} <: InterpretationSet
        instances::Vector{M}
    end

A dataset of interpretations, instantiated as a vector.

See also [`InterpretationSet`](@ref).
"""
struct InterpretationVector{M<:Interpretation} <: InterpretationSet
    instances::Vector{M}
end

function interpretationtype(::Type{S}) where {M<:Interpretation,S<:InterpretationVector{M}}
    return error("Please, provide method interpretationtype(::$(typeof(S))).")
end

Base.getindex(s::InterpretationVector, i_instance::Integer) = Base.getindex(s.instances, i_instance)
getinstance(s::InterpretationVector, i_instance::Integer) = Base.getindex(s, i_instance)
SoleBase.ninstances(s::InterpretationVector) = Base.length(s.instances)
