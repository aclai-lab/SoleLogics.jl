```@meta
CurrentModule = SoleLogics
```

```@contents
Pages = ["base-logic.md"]
```

# [Introduction](@id base-logic-introduction)
At the end of this chapter, you are going to understand how [`Atom`](@ref)s and [`Truth`](@ref) values are organized in alphabets, and how grammars are defined. 

You will also get an in-depth view of how boolean truth values and boolean [`Connective`](@ref)'s are defined from both a syntax and a syntactical standpoint of view.

The end of this chapter is dedicated to modal logic, which is one of the most hot topics covered by SoleLogics.

Recalling the type hierarchy presented in [man-core](@ref), it is here enriched with the following new types and structures.


- [`Truth`](@ref)
    - [`BooleanTruth`](@ref) (new)
        - [`Top`] (⊤)
        - [`Bot`] (⊥)
---

- [`Connective`](@ref)
    - [`NamedConnective`](@ref) (new)
        - [`NEGATION`](@ref)
        - [`CONJUNCTION`](@ref) 
        - [`DISJUNCTION`](@ref)
        - [`IMPLICATION`](@ref)
---

- [`AbstractAlphabet{V}`](@ref) (new)
    - [`ExplicitAlphabet{V}`](@ref)
    - [`AlphabetOfAny{V}`](@ref)
---

- [`AbstractGrammar{V<:AbstractAlphabet,O<:Operator}`](@ref) (new)
    - [`CompleteFlatGrammar{V<:AbstractAlphabet,O<:Operator}`](@ref)
---

- [`AbstractAlgebra{T<:Truth}`](@ref) (new)
    - [`BooleanAlgebra`](@ref)
---

- [`AbstractLogic{G<:AbstractGrammar,V<:AbstractAlgebra}`](@ref) (new)
    - [`BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra}`]

# [Alphabet](@id alphabets)
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

# [Grammar](@id grammars)
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

# [Algebra](@id algebra)

```@docs
AbstractAlgebra{T<:Truth}
truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:Truth}
domain(a::AbstractAlgebra)
top(a::AbstractAlgebra{T} where {T})
bot(a::AbstractAlgebra)
iscrisp(a::AbstractAlgebra)
```

# [Logic](@id logic)
```@docs
AbstractLogic{G<:AbstractGrammar,V<:AbstractAlgebra}
grammar(l::AbstractLogic{G}) where {G<:AbstractGrammar}
algebra(l::AbstractLogic{G,V}) where {G,V}
```

# [More about Connectives](@id more-about-connectives)
```@docs
NamedConnective{Symbol}
collatetruth(c::Connective, ts::NTuple{N,T where T<:Truth}) where {N}
simplify(c::Connective, ts::NTuple{N,F where F<:Formula}) where {N}
```

## [Boolean logic](@id boolean-algebra)

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
Top
Bot
```

```@docs
BooleanAlgebra
BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra}
```