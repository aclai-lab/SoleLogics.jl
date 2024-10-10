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

# Interface
- `interpretationtype(s)`
- `alphabet(s)`
- See also [`AbstractDataset`](@ref)

# Utility Functions
- `valuetype(s)`
- `truthtype(s)`

See also[`truthtype`](@ref),
[`InterpretationVector`](@ref).
"""
abstract type AbstractInterpretationSet <: AbstractDataset end

# TODO improve general doc.
function interpretationtype(S::Type{<:AbstractInterpretationSet})
    return error("Please, provide method interpretationtype(::$(typeof(S))).")
end
interpretationtype(s::AbstractInterpretationSet) = interpretationtype(typeof(s))

# TODO improve general doc.
valuetype(S::Type{<:AbstractInterpretationSet}) = valuetype(interpretationtype(S))
valuetype(s::AbstractInterpretationSet) = valuetype(typeof(s))

# TODO improve general doc.
truthtype(S::Type{<:AbstractInterpretationSet}) = truthtype(interpretationtype(S))
truthtype(s::AbstractInterpretationSet) = truthtype(typeof(s))

"""
    alphabet(s::AbstractInterpretationSet)::Alphabet

Return the propositional alphabet of an interpretation set.

See also [`AbstractAlphabet`](@ref), [`AbstractGrammar`](@ref).
"""
function alphabet(s::AbstractInterpretationSet)::Alphabet
    return error("Please, provide method alphabet(::$(typeof(s))).")
end

function eachinstance(s::AbstractInterpretationSet)
    return (getinstance(s, i_instance) for i_instance in 1:ninstances(s))
end
