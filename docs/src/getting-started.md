```@meta
CurrentModule = SoleLogics
```

# [Getting started](@id man-core)

## [Syntax Basics](@id syntactical-base-definitions)

Logical formulas are syntactical objects representing statements which level of truth can be assessed.
Formulas arise from formal grammars (e.g., context-free grammars), and are most commonly represented as syntax trees.
At the leaf nodes of a syntax tree are atoms (simple, atomic statements) or truth values (e.g., ⊤, representing truth),
while at the internal nodes are logical connectives that allow for the composition of formulas to represent complex concepts.

In SoleLogics, an Atom is a wrapper for any value (accessible via the `value` method).
TODO
arity

```@docs
Atom
Truth
Connective
SyntaxBranch
```

Let's review these concepts with a simple example.

```julia-repl
julia> p = Atom("p")
Atom{String}("p")

julia> q = Atom("q")
Atom{String}("q")

julia> value(p)
"p"

# Operators are syntax tokens too
julia> CONJUNCTION
∧

# SyntaxBranch's are arity-compliant
julia> st = SyntaxBranch(CONJUNCTION, p)
ERROR: AssertionError: Cannot instantiate SyntaxBranch{SoleLogics.NamedOperator{:∧}} 
with token ∧ of arity 2 and 1 children.

# In fact, the conjunction operator in a syntax tree must have exactly 2 children 
julia> arity(CONJUNCTION)
2

julia> stree = SyntaxBranch(CONJUNCTION, (p,q))
SyntaxBranch: p ∧ q

# Get the token of the root node
julia> token(st)
∧

# Get the first subtree, containing only an atom
julia> leftree = children(st)[1]; 
SyntaxBranch: p

julia> typeof(leftree)
SyntaxBranch{Atom{String}}

# Atoms are necessarily at the leaves; in fact their arity is 0
julia> leftree |> token |> arity
0
```


```@docs
AbstractSyntaxStructure
AbstractSyntaxToken
SoleLogics.arity(::AbstractSyntaxToken)
```

### Connectives

TODO

```@docs
SoleLogics.Connective
```
!!! note
    SoleLogics.jl offers the possibility to implement custom operators. To see an in-depth example, please refer to section [Customization](@ref customization-section).

```@docs
SoleLogics.NamedOperator
SoleLogics.precedence
SoleLogics.associativity
SoleLogics.iscommutative
```

To learn more about operators, refer to [Propositional Logic](@ref) and [Modal Logic](@ref) chapters.

### Formulas

TODO: the following definition might be unclear, since "evaluation" and "logic" are not concepts already read by the user. When the Logic structure will be removed from SoleLogics, this section will be expanded with semantics-related concepts.

```@docs
Formula
```

!!! note
    SoleLogics.jl offers the possibility to implement a custom [`Formula`](@ref) subtype. To see an in-depth example, please refer to section [Customization](@ref customization-section).

```@docs
joinformulas
tokens(::Formula)
SoleLogics.height(::SyntaxBranch)
SoleLogics.tree(::Formula)
```

### Parsing & Printing
SoleLogics.jl allows you to: 

- Extract the string representation of a formula (via `syntaxstring`);
- Parse formulas from strings (via `parsetree`).

These features are highly customizable, and leverage three properties of syntactical tokens: [`arity`](@ref), connective [`precedence`](@ref) and connective [`associativity`](@ref).

```@docs
syntaxstring
parsetree
```

### Grammar

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
Base.in(φ::SyntaxBranch, g::AbstractGrammar)
formulas(g::AbstractGrammar; maxdepth::Integer, nformulas::Union{Nothing,Integer} = nothing, args...)

CompleteFlatGrammar{A<:AbstractAlphabet,O<:Operator}
formulas(g::CompleteFlatGrammar{A,O} where {A,O}; maxdepth::Integer, nformulas::Union{Nothing,Integer} = nothing)
```

## Semantics

## [Customization](@id customization-section)
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc justo justo, finibus ac odio in, tempor fermentum augue. Vivamus ullamcorper lacus eget enim imperdiet, ac lobortis turpis elementum. Fusce non auctor eros. Duis scelerisque auctor volutpat. Morbi non luctus est. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Pellentesque porttitor a est sit amet ornare. Aliquam faucibus fringilla imperdiet.
