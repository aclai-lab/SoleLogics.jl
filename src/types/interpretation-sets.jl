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
- [`interpretationtype(S)`](@ref)
- [`alphabet(s)`](@ref)
- [`getinstance(s)`](@ref)
- [`concatdatasets(ss...)`](@ref)
- [`instances(s, idxs, return_view; kwargs...)`](@ref)
- [`ninstances(s)`](@ref)

# Utility Functions
- [`valuetype(s)`](@ref)
- [`truthtype(s)`](@ref)
- [`slicedataset(s, idxs; kwargs...)`](@ref)
- [`eachinstance(s)`](@ref)

# Utility Functions (with more-than-propositional logics)
- [`worldtype(s)`](@ref)
- [`frametype(s)`](@ref)
- [`frame(s, i_instance)`](@ref)
- [`accessibles(s, i_instance, args...)`](@ref)
- [`allworlds(s, i_instance, args...)`](@ref)
- [`nworlds(s, i_instance)`](@ref)

See also [`InterpretationVector`](@ref).
"""
abstract type AbstractInterpretationSet <: AbstractDataset end

"""
    interpretationtype(S::Type{<:AbstractInterpretationSet})
    interpretationtype(s::AbstractInterpretationSet)

Return a supertype for the interpretations of a given (type of)
    interpretation set.

See also[`truthtype`](@ref), [`InterpretationSet`](@ref).
"""
function interpretationtype(S::Type{<:AbstractInterpretationSet})
    return error("Please, provide method interpretationtype(::$(typeof(S))).")
end
interpretationtype(s::AbstractInterpretationSet) = interpretationtype(typeof(s))

valuetype(S::Type{<:AbstractInterpretationSet}) = valuetype(interpretationtype(S))
valuetype(s::AbstractInterpretationSet) = valuetype(typeof(s))

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

# function getinstance(s::AbstractInterpretationSet, i_instance::Integer)
#     return error("Please, provide method getinstance(::$(typeof(s)), i_instance::Integer).")
# end

function eachinstance(s::AbstractInterpretationSet)
    return (getinstance(s, i_instance) for i_instance in 1:ninstances(s))
end

############################################################################################
############################# Helpers for (Multi-)modal logics #############################
############################################################################################

worldtype(S::Type{<:AbstractInterpretationSet}) = worldtype(interpretationtype(S))
worldtype(s::AbstractInterpretationSet) = worldtype(typeof(s))

frametype(S::Type{<:AbstractInterpretationSet}) = frametype(interpretationtype(S))
frametype(s::AbstractInterpretationSet) = frametype(typeof(s))

function frame(s::AbstractInterpretationSet, i_instance::Integer)
    return error("Please, provide method frame(::$(typeof(s)), ::$(typeof(i_instance))).")
end
function accessibles(s::AbstractInterpretationSet, i_instance::Integer, args...)
    accessibles(frame(s, i_instance), args...)
end
function allworlds(s::AbstractInterpretationSet, i_instance::Integer, args...)
    allworlds(frame(s, i_instance), args...)
end

function nworlds(s::AbstractInterpretationSet, i_instance::Integer)
    nworlds(frame(s, i_instance))
end
