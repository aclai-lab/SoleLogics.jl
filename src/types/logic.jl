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

# Examples

```julia-repl
julia> Atom(1) in ExplicitAlphabet(Atom.(1:10))
true

julia> Atom(1) in ExplicitAlphabet(1:10)
true

julia> Atom(1) in AlphabetOfAny{String}()
false

julia> Atom("mystring") in AlphabetOfAny{String}()
true

julia> "mystring" in AlphabetOfAny{String}()
┌ Warning: Please, use Base.in(Atom(mystring), alphabet::AlphabetOfAny{String}) instead of Base.in(mystring, alphabet::AlphabetOfAny{String})
└ @ SoleLogics ...
true
```

# Interface
- `atoms(a::AbstractAlphabet)::Bool`
- `Base.isfinite(::Type{<:AbstractAlphabet})::Bool`
- `randatom(rng::Union{Random.AbstractRNG, Integer}, a::AbstractAlphabet, args...; kwargs...)::AbstractAtom`

# Utility functions
- `natoms(a::AbstractAlphabet)::Bool`
- `Base.in(p::AbstractAtom, a::AbstractAlphabet)::Bool`
- `Base.eltype(a::AbstractAlphabet)`
- `randatom(a::AbstractAlphabet, args...; kwargs...)::AbstractAtom`
- `atomstype(a::AbstractAlphabet)`
- `valuetype(a::AbstractAlphabet)`

# Implementation

When implementing a new alphabet type `MyAlphabet`, you should provide a method for
establishing whether an atom belongs to it or not;
while, in general, this method should be:

    function Base.in(p::AbstractAtom, a::MyAlphabet)::Bool

in the case of *finite* alphabets, it suffices to define a method:

    function atoms(a::AbstractAlphabet)::AbstractVector{atomstype(a)}

By default, an alphabet is considered finite:

    Base.isfinite(::Type{<:AbstractAlphabet}) = true
    Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))
    Base.in(p::AbstractAtom, a::AbstractAlphabet) = Base.isfinite(a) ? Base.in(p, atoms(a)) : error(...)

See also [`AbstractGrammar`](@ref), [`AlphabetOfAny`](@ref), [`AbstractAtom`](@ref),
[`ExplicitAlphabet`](@ref).
"""
abstract type AbstractAlphabet{V} end

Base.eltype(::Type{<:AbstractAlphabet{V}}) where {V} = Atom{V}
atomstype(V::Type{<:AbstractAlphabet}) = eltype(V)
atomstype(a::AbstractAlphabet) = atomstype(typeof(a))
valuetype(a::Type{<:AbstractAlphabet}) = valuetype(atomstype(a))
valuetype(a::AbstractAlphabet) = valuetype(atomstype(a))

"""
An alphabet of `valuetype` `V` can be used for instantiating atoms of valuetype `V`.
"""
(::AbstractAlphabet{V})(a) where {V} = Atom{V}(a)

# Default behavior
"""
    Base.isfinite(a::AbstractAlphabet)

Return `true` if the alphabet is finite, `false` otherwise.

See [`AbstractAlphabet`](@ref).
"""
Base.isfinite(::Type{<:AbstractAlphabet}) = true
Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))

"""
    atoms(a::AbstractAlphabet)::AbstractVector{atomstype(a)}

List the atoms of a *finite* alphabet.

See also [`AbstractAlphabet`](@ref).
"""
function atoms(a::AbstractAlphabet)::AbstractVector{atomstype(a)}
    if Base.isfinite(a)
        return error("Please, provide method atoms(::$(typeof(a))).")
    else
        return error("Cannot list atoms of (infinite) alphabet of type $(typeof(a)).")
    end
end

"""
    Base.in(p::AbstractAtom, a::AbstractAlphabet)::Bool

Return whether an atom belongs to an alphabet.

See also [`AbstractAlphabet`](@ref), [`AbstractAtom`](@ref).
"""
function Base.in(p::AbstractAtom, a::AbstractAlphabet)::Bool
    if Base.isfinite(a)
        Base.in(p, atoms(a))
    else
        return error("Cannot establish whether an atom belongs to " *
            "(infinite) alphabet of type $(typeof(a)).")
    end
end

# Helper
function Base.in(value::Union{AbstractString,Number,AbstractChar}, a::AbstractAlphabet)
    @warn "Please, use Base.in(Atom($(value)), alphabet::$(typeof(a))) instead of " *
        "Base.in($(value), alphabet::$(typeof(a)))"
    Base.in(Atom(value), a)
end

"""
    natoms(a::AbstractAlphabet)::Integer

Return the number of atoms of a *finite* alphabet.

See also [`randatom`](@ref), [`AbstractAlphabet`](@ref).
"""
function natoms(a::AbstractAlphabet)::Integer
    if Base.isfinite(a)
        return error("Please, provide method natoms(::$(typeof(a))).")
    else
        return error("Cannot compute natoms of (infinite) alphabet of type $(typeof(a)).")
    end
end

"""
    randatom(a::AbstractAlphabet, args...; kwargs...)
    randatom(rng::Union{Random.AbstractRNG, Integer}, a::AbstractAlphabet, args...; kwargs...)

Return a random atom from a *finite* alphabet.

See also [`natoms`](@ref), [`AbstractAlphabet`](@ref).
"""
function randatom(a::AbstractAlphabet, args...; kwargs...)
    randatom(Random.GLOBAL_RNG, a, args...; kwargs...)
end

function randatom(rng::Union{Random.AbstractRNG, Integer}, a::AbstractAlphabet, args...; kwargs...)
    if isfinite(a)
        # TODO: note that `atoms(a)` can lead to brutal reduction in performance,
        #  if one forgets to implement specific methods for `randatom` for custom alphabets!
        return Base.rand(rng, atoms(a), args...; kwargs...)
    else
        error("Please provide method randatom(rng::$(typeof(rng)), " *
            "alphabet::$(typeof(a)), args...; kwargs...)")
    end
end

# Helper
function Base.length(a::AbstractAlphabet)
    @warn "Please use `natoms` instead of `Base.length` with alphabets."
    return natoms(a)
end

"""
    Base.iterate(a::AbstractAlphabet)
    Base.iterate(a::AbstractAlphabet, state)

Return an iterator to the next element in an alhabet.

See also [`AbstractAlphabet`](@ref), [`AbstractSyntaxBranch`](@ref).
"""
function Base.iterate(a::AbstractAlphabet)
    if isfinite(a)
        return Base.iterate(atoms(a))
    else
        return error("Cannot iterate (infinite) alphabet of type $(typeof(a)).")
    end
end
function Base.iterate(a::AbstractAlphabet, state)
    if isfinite(a)
        return Base.iterate(atoms(a), state)
    else
        return error("Cannot iterate (infinite) alphabet of type $(typeof(a)).")
    end
end

# [Iteration interface](https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration) util.
function Base.IteratorSize(::Type{V}) where {V<:AbstractAlphabet}
    return Base.isfinite(V) ? Base.HasLength() : Base.IsInfinite()
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
V context-free grammar is a simple structure for defining formulas inductively.

# Interface
- `alphabet(g::AbstractGrammar)::AbstractAlphabet`
- `Base.in(::SyntaxTree, g::AbstractGrammar)::Bool`
- `formulas(g::AbstractGrammar; kwargs...)::Vector{<:SyntaxTree}`

# Utility functions
- `Base.in(a::AbstractAtom, g::AbstractGrammar)`
- `atomstype(g::AbstractGrammar)`
- `tokenstype(g::AbstractGrammar)`
- `operatorstype(g::AbstractGrammar)`
- `alphabettype(g::AbstractGrammar)`

See also [`alphabet`](@ref),
[`AbstractAlphabet`](@ref), [`Operator`](@ref).
"""
abstract type AbstractGrammar{V<:AbstractAlphabet,O<:Operator} end

operatorstype(::AbstractGrammar{V,O}) where {V,O} = O
alphabettype(::AbstractGrammar{V,O}) where {V,O} = V

"""
    alphabet(g::AbstractGrammar{V} where {V})::V

Return the propositional alphabet of a grammar.

See also [`AbstractAlphabet`](@ref), [`AbstractGrammar`](@ref).
"""
function alphabet(g::AbstractGrammar{V} where {V})::V
    return error("Please, provide method alphabet(::$(typeof(g))).")
end
atomstype(g::AbstractGrammar) = eltype(alphabet(g))
tokenstype(g::AbstractGrammar) = Union{operatorstype(g),atomstype(g)}

"""
    Base.in(φ::SyntaxTree, g::AbstractGrammar)::Bool

Return whether a `SyntaxTree`, belongs to a grammar.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
function Base.in(::SyntaxTree, g::AbstractGrammar)::Bool
    return error("Please, provide method Base.in(::SyntaxTree, ::$(typeof(g))).")
end

function Base.in(φ::Formula, g::AbstractGrammar)::Bool
    return Base.in(tree(φ), g)
end

# TODO actually differentiate Connective's and SyntaxLeaves, and define+use leaves(g)
# Note: when using this file's syntax tokens, these methods suffice:
Base.in(a::AbstractAtom, g::AbstractGrammar) = Base.in(a, alphabet(g))
Base.in(op::Truth, g::AbstractGrammar) = (op <: operatorstype(g))
Base.in(op::Connective, g::AbstractGrammar) = (op <: operatorstype(g))

"""
    formulas(
        g::AbstractGrammar;
        maxdepth::Integer,
        nformulas::Union{Nothing,Integer} = nothing,
        args...
    )::Vector{<:SyntaxTree}

Enumerate the formulas produced by a given grammar with a finite and iterable alphabet.

# Implementation

Additional `args` can be used to model the function's behavior.
At least these two arguments should be covered:
- a `nformulas` argument can be used to limit the size of the returned `Vector`;
- a `maxdepth` argument can be used to limit the syntactic component, represented as a syntax tree,
to a given maximum depth;

See also [`AbstractGrammar`](@ref), [`AbstractSyntaxBranch`](@ref).
"""
function formulas(
    g::AbstractGrammar{V,O} where {V,O};
    maxdepth::Integer,
    nformulas::Union{Nothing,Integer} = nothing,
    args...
)::Vector{<:SyntaxTreeo}
    @assert maxdepth >= 0
    @assert nformulas > 0
    if isfinite(alphabet(g))
        return error("Please, provide method formulas(::$(typeof(g)), maxdepth, " *
                     "nformulas, args...).")
    else
        return error("Cannot enumerate formulas of (infinite) " *
            "alphabet of type $(typeof(alphabet(g))).")
    end
end

function Base.isequal(a::AbstractGrammar, b::AbstractGrammar)
    Base.isequal(alphabet(a), alphabet(b)) &&
    Base.isequal(operatorstype(a), operatorstype(b))
end
Base.hash(a::AbstractGrammar) = Base.hash(alphabet(a), Base.hash(operatorstype(a)))

############################################################################################
#### AbstractAlgebra, semantics ############################################################
############################################################################################

"""
    abstract type AbstractAlgebra{T<:Truth} end

Abstract type for representing algebras. Algebras are used for grounding the
truth of atoms and the semantics of operators. They typically encode a
[lattice structure](https://en.wikipedia.org/wiki/Lattice_(order)) where two
elements(or nodes) *⊤* and *⊥* are referred to as *TOP* (or maximum)
and *bot* (or minimum). Each node in the lattice represents a truth value
that an atom or a formula can have on an interpretation, and the
semantics of operators is given in terms of operations between truth values.

# Interface

- `truthtype(a::AbstractAlgebra)`
- `domain(a::AbstractAlgebra)`
- `top(a::AbstractAlgebra)`
- `bot(a::AbstractAlgebra)`

# Utility functions
- `iscrisp(a::AbstractAlgebra)`

# Implementation

When implementing a new algebra type, the methods `domain`,
`TOP`, and `bot` should be implemented.

See also [`bot`](@ref), [`BooleanAlgebra`](@ref), [`Operator`](@ref), [`TOP`](@ref),
[`collatetruth`](@ref), [`domain`](@ref), [`iscrisp`](@ref), [`truthtype`](@ref).
"""
abstract type AbstractAlgebra{T<:Truth} end

"""
    truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:Truth} = T
    truthtype(a::AbstractAlgebra) = truthtype(typeof(a))

The Julia type for representing truth values of the algebra.

See also [`AbstractAlgebra`](@ref).
"""
truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:Truth} = T
truthtype(a::AbstractAlgebra) = truthtype(typeof(a))

"""
    domain(a::AbstractAlgebra)

Return an iterator to the values in the `domain` of a given algebra.

See also [`AbstractAlgebra`](@ref).
"""
function domain(a::AbstractAlgebra{T} where {T<:Truth})::AbstractVector{T}
    return error("Please, provide method domain(::$(typeof(a))).")
end

# Note: maybe one day this will have a use?
# Base.in(t::Truth, a::AbstractAlgebra) = Base.in(t, domain(a))

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

"""
    iscrisp(a::AbstractAlgebra) = iscrisp(typeof(a))

An algebra is crisp (or *boolean*) when its domain only has two values, namely,
the top and the bottom. The antonym of crisp is *fuzzy*.

See also [`AbstractAlgebra`](@ref).
"""
iscrisp(a::AbstractAlgebra) = (length(domain(a)) == 2)

############################################################################################
#### AbstractLogic #########################################################################
############################################################################################

"""
    abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

Abstract type of a logic, which comprehends a context-free grammar (*syntax*) and
an algebra (*semantics*).

# Interface

- `grammar(l::AbstractLogic)::AbstractGrammar`
- `algebra(l::AbstractLogic)::AbstractAlgebra`

# Utility functions
- See also [`AbstractGrammar`](@ref)
- See also [`AbstractAlgebra`](@ref)

# Implementation

When implementing a new logic type,
the methods `grammar` and `algebra` should be implemented.

See also [`AbstractAlgebra`](@ref), [`AbstractGrammar`](@ref).
"""
abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

"""
    grammar(l::AbstractLogic{G})::G where {G<:AbstractGrammar}

Return the `grammar` of a given logic.

See also [`AbstractGrammar`](@ref), [`AbstractLogic`](@ref), [`algebra`](@ref),
[`alphabet`](@ref), [`formulas`](@ref), [`grammar`](@ref), [`operators`](@ref),
[`truthtype`](@ref).
"""
function grammar(l::AbstractLogic{G})::G where {G}
    return error("Please, provide method grammar(::$(typeof(l))).")
end

operatorstype(l::AbstractLogic) = operatorstype(grammar(l))
alphabettype(l::AbstractLogic) = alphabettype(grammar(l))
operators(l::AbstractLogic) = operators(grammar(l))
alphabet(l::AbstractLogic) = alphabet(grammar(l))
atomstype(l::AbstractLogic) = atomstype(alphabet(l))
tokenstype(l::AbstractLogic) = tokenstype(grammar(l))
formulas(l::AbstractLogic, args...; kwargs...) = formulas(grammar(l), args...; kwargs...)

Base.in(op::Operator, l::AbstractLogic) = Base.in(op, grammar(l))
Base.in(φ::AbstractSyntaxBranch, l::AbstractLogic) = Base.in(φ, grammar(l))
Base.in(p::AbstractAtom, l::AbstractLogic) = Base.in(p, alphabet(l))

"""
    algebra(l::AbstractLogic{G,V})::V where {G,V}

Return the `algebra` of a given logic.

See also [`AbstractAlgebra`](@ref), [`AbstractLogic`](@ref).
"""
function algebra(l::AbstractLogic{G,V})::V where {G,V}
    return error("Please, provide method algebra(::$(typeof(l))).")
end

truthtype(l::AbstractLogic) = truthtype(algebra(l))
top(l::AbstractLogic) = top(algebra(l))
bot(l::AbstractLogic) = bot(algebra(l))
iscrisp(l::AbstractLogic) = iscrisp(algebra(l))

function Base.isequal(a::AbstractLogic, b::AbstractLogic)
    Base.isequal(grammar(a), grammar(b)) &&
    Base.isequal(algebra(a), algebra(b))
end
Base.hash(a::AbstractLogic) = Base.hash(grammar(a), Base.hash(algebra(a)))
