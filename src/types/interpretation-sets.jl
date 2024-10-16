import SoleBase: AbstractDataset, ninstances, eachinstance
import Base: getindex

"""
    abstract type AbstractInterpretationSet <: AbstractDataset end

Abstract type for ordered sets of interpretations.
A set of interpretations, also referred to as a *dataset* in this context,
is a collection of *instances*, each of which is an interpretation, and is
identified by an index i_instance::Integer.
These structures are especially useful when performing
[model checking](https://en.wikipedia.org/wiki/Model_checking).

See also[`truthtype`](@ref),
[`InterpretationVector`](@ref).
"""
abstract type AbstractInterpretationSet <: AbstractDataset end

# TODO improve general doc.
function interpretationtype(S::Type{<:AbstractInterpretationSet})
    return error("Please, provide method interpretationtype(::$(typeof(S))).")
end

# TODO improve general doc.
function valuetype(S::Type{<:AbstractInterpretationSet})
    return error("Please, provide method valuetype(::$(typeof(S))).")
end

# TODO improve general doc.
function truthtype(S::Type{<:AbstractInterpretationSet})
    return error("Please, provide method truthtype(::$(typeof(S))).")
end

"""
    alphabet(s::AbstractInterpretationSet)::Alphabet

Return the propositional alphabet of an interpretation set.

See also [`AbstractAlphabet`](@ref), [`AbstractGrammar`](@ref).
"""
function alphabet(s::AbstractInterpretationSet)::Alphabet
    return error("Please, provide method alphabet(::$(typeof(s))).")
end

"""
    struct LogicalInstance{S<:AbstractInterpretationSet}
        s::S
        i_instance::Int64
    end

Object representing the i-th interpretation of an interpretation set.

In general, one may not be able to extract a single logical instance from a
set; thus, this representation, holding the interpretation set + instance id (i_instance),
can come handy in defining `check` and `interpret` methods for newly defined interpretation
set structures.
"""
abstract type LogicalInstance{S<:AbstractInterpretationSet} <: AbstractInterpretation end

function interpret(
    φ::Atom,
    i::LogicalInstance,
    args...;
    kwargs...
)::Formula
    return error("Please, provide method " *
        "interpret(φ::Atom, i::$(typeof(i)), " *
        "args...::$(typeof(args)); " *
        "kwargs...::$(typeof(kwargs))).")
end

function check(
    φ::Formula,
    i::LogicalInstance,
    args...;
    kwargs...
)
    return error("Please, provide method " *
        "check(φ::SyntaxTree, i::$(typeof(i)), " *
        "args...::$(typeof(args)); " *
        "kwargs...::$(typeof(kwargs))).")
end

# Abstract type for frame sets
abstract type AbstractFrameSet{FR<:AbstractFrame} end

function Base.getindex(::AbstractFrameSet{FR}, i_instance::Integer)::FR where {FR<:AbstractFrame}
    return error("Please, provide ...")
end

function frame(s::AbstractInterpretationSet, i_instance::Integer)
    return error("Please, provide method frame(::$(typeof(s)), ::$(typeof(i_instance))).")
end