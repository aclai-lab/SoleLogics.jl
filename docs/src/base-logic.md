```@meta
CurrentModule = SoleLogics
```

```@contents
Pages = ["base-logic.md"]
```

# [Introduction](@id base-logic-introduction)
At the end of this chapter, you are going to understand how [`Atom`](@ref)s and [`Truth`](@ref) values are organized in *alphabets*, and how *grammars* are defined. 

You will also get an in-depth view of how boolean [`Truth`](@ref) values and boolean [`Connective`](@ref)'s are defined from both a syntax and a syntactical standpoint of view.

Finally, you will get a clearer idea about how to represent and manipulate *interpretations* and their outcomes.

Recalling the type hierarchy presented in [man-core](@ref), it is here enriched with the following new types and structures.


- [`Truth`](@ref)
    - [`BooleanTruth`](@ref)
---

- [`Connective`](@ref)
    - [`NamedConnective`](@ref)
        - [`NEGATION`](@ref)
        - [`CONJUNCTION`](@ref) 
        - [`DISJUNCTION`](@ref)
        - [`IMPLICATION`](@ref)
---

- [`AbstractAlphabet{V}`](@ref)
    - [`ExplicitAlphabet{V}`](@ref)
    - [`AlphabetOfAny{V}`](@ref)
---

- [`AbstractGrammar{V<:AbstractAlphabet,O<:Operator}`](@ref)
    - [`CompleteFlatGrammar{V<:AbstractAlphabet,O<:Operator}`](@ref)
---

- [`AbstractAlgebra{T<:Truth}`](@ref)
    - [`BooleanAlgebra`](@ref)
---

- [`AbstractLogic{G<:AbstractGrammar,V<:AbstractAlgebra}`](@ref)
    - [`BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra}`](@ref)
---

- [`AbstractInterpretation`](@ref)
    - [`AbstractAssignment`](@ref)
        - [`TruthDict{D<:AbstractDict{A where A<:Atom,T where T<:Truth}}`](@ref)
        - [`DefaultedTruthDict{D<:AbstractDict{A where A<:Atom,T where T<:Truth}, T<:Truth}`](@ref)
    - [`AbstractInterpretationSet{M<:AbstractInterpretation}`](@ref) **(see [SoleBase.jl](https://github.com/aclai-lab/SoleBase.jl))**
        - [`InterpretationVector{M<:AbstractInterpretation}`](@ref)
    - [`LogicalInstance{S<:AbstractInterpretationSet}`](@ref) 

- [`TruthTable{A,T<:Truth}`](@ref)
- [`LogicalInstance{S<:AbstractInterpretationSet}`](@ref)

## [Alphabet](@id alphabets)
```@docs
AbstractAlphabet{V}
Base.isfinite(::Type{<:AbstractAlphabet})
atoms(a::AbstractAlphabet)
Base.in(p::Atom, a::AbstractAlphabet)
Base.length(a::AbstractAlphabet)
Base.iterate(a::AbstractAlphabet)

ExplicitAlphabet{V}
AlphabetOfAny{V}
```

## [Grammar](@id grammars)
```@docs
AbstractGrammar{V<:AbstractAlphabet,O<:Operator} 
alphabet(g::AbstractGrammar{V} where {V})
Base.in(φ::SyntaxTree, g::AbstractGrammar)
formulas(g::AbstractGrammar; maxdepth::Integer, nformulas::Union{Nothing,Integer} = nothing, args...)

CompleteFlatGrammar{V<:AbstractAlphabet,O<:Operator}
connectives(g::AbstractGrammar)
leaves(g::AbstractGrammar)

formulas(g::CompleteFlatGrammar{V,O} where {V,O}; maxdepth::Integer, nformulas::Union{Nothing,Integer} = nothing)
```

## [Algebra](@id algebra)

```@docs
AbstractAlgebra{T<:Truth}
truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:Truth}
domain(a::AbstractAlgebra)
top(a::AbstractAlgebra{T} where {T})
bot(a::AbstractAlgebra)
iscrisp(a::AbstractAlgebra)
```

## [Logic](@id logic)
```@docs
AbstractLogic{G<:AbstractGrammar,V<:AbstractAlgebra}
grammar(l::AbstractLogic{G}) where {G<:AbstractGrammar}
algebra(l::AbstractLogic{G,V}) where {G,V}
```

## [More about Connectives](@id more-about-connectives)
```@docs
NamedConnective{Symbol}
collatetruth(c::Connective, ts::NTuple{N,T where T<:Truth}) where {N}
simplify(c::Connective, ts::NTuple{N,F where F<:Formula}) where {N}
```

## [Propositional boolean logic](@id boolean-algebra)

```@docs
NEGATION
CONJUNCTION
DISJUNCTION
IMPLICATION
```

Boolean logic [`Connective`](@ref)s are regrouped in a single collection.
```@docs
BASE_CONNECTIVES
```

```@docs
BooleanTruth
```

```@docs
BooleanAlgebra
BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra}
```

A method is provided to simply access a propositional logic.

```@docs
    propositionallogic(; alphabet = AlphabetOfAny{String}(), operators = $(BASE_PROPOSITIONAL_CONNECTIVES), grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), $(BASE_PROPOSITIONAL_CONNECTIVES)), algebra = BooleanAlgebra())
```

# Interpretations

Interpretations are nothing but dictionaries working with [`Truth`](@ref) values, or other types that can be ultimately converted to [`Truth`](@ref).

```@docs
AbstractAssignment
Base.haskey(i::AbstractAssignment, ::Atom)
TruthDict{D<:AbstractDict{A where A<:Atom,T where T<:Truth}}
DefaultedTruthDict{D<:AbstractDict{A where A<:Atom,T where T<:Truth}, T<:Truth}
```

To associate interpretations with their assignment, we can simply build a [truth table](https://en.wikipedia.org/wiki/Truth_table).
```@docs
TruthTable{A,T<:Truth}
```

```
AbstractInterpretationSet{M<:AbstractInterpretation}

LogicalInstance{S<:AbstractInterpretationSet}

check(φ::Formula, s::AbstractInterpretationSet, i_instance::Integer, args...; kwargs...)
check(φ::Formula, s::AbstractInterpretationSet, args...; kwargs...)

InterpretationVector{M<:AbstractInterpretation}
```
