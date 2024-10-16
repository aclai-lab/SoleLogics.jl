# interfaces.jl

import SoleBase: initrng
import Base: convert, promote_rule, _promote
import Base: eltype, in, getindex, isiterable, iterate, IteratorSize, length, isequal, hash

############################################################################################
#### AbstractAlphabet ######################################################################
############################################################################################

"""
    abstract type AbstractAlphabet{V} end

Abstract type for representing an alphabet of atoms with values of type `V`.
An alphabet (or *propositional alphabet*) is a set of atoms
(assumed to be [countable](https://en.wikipedia.org/wiki/Countable_set)).

See also [`Atom`](@ref), [`ExplicitAlphabet`](@ref), [`AlphabetOfAny`](@ref).
"""
abstract type AbstractAlphabet{V} end

"""
    atoms(a::AbstractAlphabet)::AbstractVector{atomstype(a)}

List the atoms of a *finite* alphabet.

See also [`AbstractAlphabet`](@ref).
"""
function atoms(a::AbstractAlphabet)::AbstractVector{atomstype(a)}
    return error("Please, provide method atoms(::$(typeof(a))).")
end

"""
    Base.in(p::Atom, a::AbstractAlphabet)::Bool

Return whether an atom belongs to an alphabet.

See also [`AbstractAlphabet`](@ref), [`Atom`](@ref).
"""
function Base.in(p::Atom, a::AbstractAlphabet)::Bool
    return error("Please, provide method Base.in(::Atom, ::$(typeof(a))).")
end

"""
    natoms(a::AbstractAlphabet)::Integer

Return the number of atoms of a *finite* alphabet.

See also [`randatom`](@ref), [`AbstractAlphabet`](@ref).
"""
function natoms(a::AbstractAlphabet)::Integer
    return error("Please, provide method natoms(::$(typeof(a))).")
end

############################################################################################
#### AbstractGrammar #######################################################################
############################################################################################

"""
    abstract type AbstractGrammar{V<:AbstractAlphabet,O<:Operator} end

Abstract type for representing a
[context-free grammar](https://en.wikipedia.org/wiki/Context-free_grammar)
based on a *single* alphabet of type `V`, and a set of operators
that consists of all the (singleton) child types of `O`.
A context-free grammar is a simple structure for defining formulas inductively.

See also [`alphabet`](@ref), [`AbstractAlphabet`](@ref), [`Operator`](@ref).
"""
abstract type AbstractGrammar{V<:AbstractAlphabet,O<:Operator} end

"""
    alphabet(g::AbstractGrammar{V} where {V})::V

Return the propositional alphabet of a grammar.

See also [`AbstractAlphabet`](@ref), [`AbstractGrammar`](@ref).
"""
function alphabet(g::AbstractGrammar{V} where {V})::V
    return error("Please, provide method alphabet(::$(typeof(g))).")
end

"""
    Base.in(φ::SyntaxTree, g::AbstractGrammar)::Bool

Return whether a `SyntaxTree`, belongs to a grammar.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
function Base.in(::SyntaxTree, g::AbstractGrammar)::Bool
    return error("Please, provide method Base.in(::SyntaxTree, ::$(typeof(g))).")
end

"""
    formulas(
        g::AbstractGrammar;
        maxdepth::Integer,
        nformulas::Union{Nothing,Integer} = nothing,
        args...
    )::Vector{<:SyntaxBranch}

Enumerate the formulas produced by a given grammar with a finite and iterable alphabet.

See also [`AbstractGrammar`](@ref), [`SyntaxBranch`](@ref).
"""
function formulas(
    g::AbstractGrammar;
    maxdepth::Integer,
    nformulas::Union{Nothing,Integer} = nothing,
    args...
)::Vector{<:SyntaxBranch}
    return error("Please, provide method formulas(::$(typeof(g)), args...; kwargs...).")
end

############################################################################################
#### AbstractAlgebra #######################################################################
############################################################################################

"""
    abstract type AbstractAlgebra{T<:Truth} end

Abstract type for representing algebras. Algebras are used for grounding the
truth of atoms and the semantics of operators.

See also [`bot`](@ref), [`BooleanAlgebra`](@ref), [`Operator`](@ref), [`TOP`](@ref),
[`collatetruth`](@ref), [`domain`](@ref), [`iscrisp`](@ref), [`truthtype`](@ref).
"""
abstract type AbstractAlgebra{T<:Truth} end

"""
    domain(a::AbstractAlgebra)

Return an iterator to the values in the `domain` of a given algebra.

See also [`AbstractAlgebra`](@ref).
"""
function domain(a::AbstractAlgebra{T} where {T<:Truth})::AbstractVector{T}
    return error("Please, provide method domain(::$(typeof(a))).")
end

"""
    top(a::AbstractAlgebra)

Return the top of a given algebra.

See also [`bot`](@ref), [`AbstractAlgebra`](@ref).
"""
function top(a::AbstractAlgebra{T} where {T})::T
    return error("Please, provide method top(::$(typeof(a))).")
end

"""
    bot(a::AbstractAlgebra)

Return the bottom of a given algebra.

See also [`top`](@ref), [`AbstractAlgebra`](@ref).
"""
function bot(a::AbstractAlgebra{T} where {T})::T
    return error("Please, provide method bot(::$(typeof(a))).")
end

############################################################################################
#### AbstractLogic #########################################################################
############################################################################################

"""
    abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

Abstract type of a logic, which comprehends a context-free grammar (*syntax*) and
an algebra (*semantics*).

See also [`AbstractAlgebra`](@ref), [`AbstractGrammar`](@ref).
"""
abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

"""
    grammar(l::AbstractLogic{G})::G where {G<:AbstractGrammar}

Return the `grammar` of a given logic.

See also [`AbstractGrammar`](@ref), [`AbstractLogic`](@ref), [`algebra`](@ref).
"""
function grammar(l::AbstractLogic{G})::G where {G}
    return error("Please, provide method grammar(::$(typeof(l))).")
end

"""
    algebra(l::AbstractLogic{G,V})::V where {G,V}

Return the `algebra` of a given logic.

See also [`AbstractAlgebra`](@ref), [`AbstractLogic`](@ref).
"""
function algebra(l::AbstractLogic{G,V})::V where {G,V}
    return error("Please, provide method algebra(::$(typeof(l))).")
end

# Utility functions
atomstype(V::Type{<:AbstractAlphabet}) = eltype(V)
atomstype(a::AbstractAlphabet) = atomstype(typeof(a))
valuetype(a::Type{<:AbstractAlphabet}) = valuetype(atomstype(a))
valuetype(a::AbstractAlphabet) = valuetype(atomstype(a))
operatorstype(::AbstractGrammar{V,O}) where {V,O} = O
alphabettype(::AbstractGrammar{V,O}) where {V,O} = V
atomstype(g::AbstractGrammar) = eltype(alphabet(g))
tokenstype(g::AbstractGrammar) = Union{operatorstype(g),atomstype(g)}
operatorstype(l::AbstractLogic) = operatorstype(grammar(l))
alphabettype(l::AbstractLogic) = alphabettype(grammar(l))
atomstype(l::AbstractLogic) = atomstype(alphabet(l))
tokenstype(l::AbstractLogic) = tokenstype(grammar(l))