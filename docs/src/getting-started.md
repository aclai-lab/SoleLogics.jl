```@meta
CurrentModule = SoleLogics
```

# [Getting started](@id man-core)

## [Syntax Basics](@id syntactical-base-definitions)

- Logical formulas are most commonly represented as syntax trees
TODO

```@docs
AbstractSyntaxStructure
AbstractSyntaxToken
SyntaxTree
SoleLogics.arity(::Type{<:AbstractSyntaxToken})
Atom
```

Let's recall the last concepts with a simple example.

```julia-repl
julia> p = Atom("p")
Atom{String}("p")

julia> q = Atom("q")
Atom{String}("q")

# Operators are syntax tokens too
julia> CONJUNCTION
∧

# SyntaxTree's are arity-compliant
julia> st = SyntaxTree(CONJUNCTION, p)
ERROR: AssertionError: Cannot instantiate SyntaxTree{SoleLogics.NamedOperator{:∧}} 
with token ∧ of arity 2 and 1 children.

# In fact, the conjunction operator in a syntax tree must have exactly 2 children 
julia> arity(CONJUNCTION)
2

julia> stree = SyntaxTree(CONJUNCTION, (p,q))
SyntaxTree: p ∧ q

# Get the token of the root node
julia> token(st)
∧

# Get the first subtree, containing only an atom
julia> leftree = children(st)[1]; 
SyntaxTree: p

julia> typeof(leftree)
SyntaxTree{Atom{String}}

# Atoms are necessarily at the leaves; in fact their arity is 0
julia> leftree |> token |> arity
0
```

## Operators

```@docs
SoleLogics.AbstractOperator
```
!!! note
    SoleLogics.jl offers the possibility to implement custom [`NamedOperator`](@ref)s. To see an in-depth example, please refer to section [Customization](@ref customization-section).

```@docs
SoleLogics.NamedOperator
Base.operator_precedence(op::AbstractOperator)
SoleLogics.isrightassociative
SoleLogics.iscommutative
```

To learn more about operators, refer to [Propositional Logic](@ref) and [Modal Logic](@ref) chapters.

## Formulas

TODO: the following definition might be unclear, since "evaluation" and "logic" are not concepts already red from the user. When the Logic structure will be removed from SoleLogics, this section will be expanded with semantics-related concepts.

```@docs
AbstractFormula
```

!!! note
    SoleLogics.jl offers the possibility to implement a custom [`Formula`](@ref) type. To see an in-depth example, please refer to section [Customization](@ref customization-section).

```@docs
joinformulas
tokens(f::AbstractFormula)
SoleLogics.height(t::SyntaxTree)
SoleLogics.tree(f::AbstractFormula)
```

## Parsing & Printing
SoleLogics.jl allows you to: 

- Extract the string representation of a formula (via `syntaxstring`);
- Parse formulas from strings (via `parsetree`).

These features are highly customizable, and leverage three properties of syntactical tokens: [`arity`](@ref), operator precedence ([`operator_precedence`](@ref)) and operator associativity ([`isrightassociative`](@ref)).

```@docs
syntaxstring
parsetree
```

## Grammar

```@docs
AbstractAlphabet
```

!!! note
    SoleLogics.jl offers the possibility to implement a custom [`AbstractAlphabet`](@ref) concrete type. To see an in-depth example, please refer to section [Customization](@ref customization-section).

```@docs
atoms(a::AbstractAlphabet)

Base.in(p::Atom, a::AbstractAlphabet)
Base.length(a::AbstractAlphabet)
Base.iterate(a::AbstractAlphabet)
ExplicitAlphabet
AlphabetOfAny

AbstractGrammar
alphabet(g::AbstractGrammar{A} where {A})
Base.in(t::SyntaxTree, g::AbstractGrammar)
formulas(g::AbstractGrammar; maxdepth::Integer, nformulas::Union{Nothing,Integer} = nothing, args...)

CompleteFlatGrammar{A<:AbstractAlphabet,O<:AbstractOperator}
formulas( g::CompleteFlatGrammar{A,O} where {A,O}; maxdepth::Integer, nformulas::Union{Nothing,Integer} = nothing)
```

## Semantics

## [Customization](@id customization-section)
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc justo justo, finibus ac odio in, tempor fermentum augue. Vivamus ullamcorper lacus eget enim imperdiet, ac lobortis turpis elementum. Fusce non auctor eros. Duis scelerisque auctor volutpat. Morbi non luctus est. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Pellentesque porttitor a est sit amet ornare. Aliquam faucibus fringilla imperdiet.
