```@meta
CurrentModule = SoleLogics
```

# [Getting started](@id man-core)

In this chapter, Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc justo justo, finibus ac odio in, tempor fermentum augue. Vivamus ullamcorper lacus eget enim imperdiet, ac lobortis turpis elementum. Fusce non auctor eros. Duis scelerisque auctor volutpat. Morbi non luctus est. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Pellentesque porttitor a est sit amet ornare. Aliquam faucibus fringilla imperdiet.

## [Syntax Basics](@id syntactical-base-definitions)

```@docs
AbstractSyntaxStructure
AbstractSyntaxToken
SyntaxTree
SoleLogics.arity(::Type{<:AbstractSyntaxToken})
Proposition
```

Let's recall the last concepts with a simple script.

```julia
julia> p = Proposition("p")
Proposition{String}("p")

julia> q = Proposition("q")
Proposition{String}("q")

# As we shall see in the next section, operators are syntax tokens too
julia> CONJUNCTION
∧

julia> st = SyntaxTree(CONJUNCTION, p)
ERROR: AssertionError: Cannot instantiate SyntaxTree{SoleLogics.NamedOperator{:∧}} 
with token ∧ of arity 2 and 1 children.

# In fact, the conjunction operator in a syntax tree must have exactly 2 children 
julia> arity(CONJUNCTION)
2

julia> stree = SyntaxTree(CONJUNCTION, (p,q))
SyntaxTree: p ∧ q

# Get the content of the root token
julia> token(st)
∧

# Get the right subtree, containing only a proposition
julia> leftree = children(st)[1]; 
SyntaxTree: p

julia> typeof(leftree)
SyntaxTree{Proposition{String}}

# Propositions are necessarily leaf nodes, in fact their arity is 0
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
SoleLogics.jl parsing and printing systems allows you to 

- easily convert formulas represented as strings into syntactical structures;
- print complex formulas in a more clear manner, avoiding placing parentheses where it's not needed. 

 Those features are both highly customizable and work entirely three properties typical of syntactical tokens: [`arity`](@ref), operator precedence ([`Base.operator_precedence(op::AbstractOperator)`](@ref)) and operator associativity ([`SoleLogics.isrightassociative`](@ref)).

```@docs
parsetree
syntaxstring
```

## Grammar

```@docs
AbstractAlphabet
```

!!! note
    SoleLogics.jl offers the possibility to implement a custom [`AbstractAlphabet`](@ref) concrete type. To see an in-depth example, please refer to section [Customization](@ref customization-section).

```@docs
propositions(a::AbstractAlphabet)

Base.in(p::Proposition, a::AbstractAlphabet)
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