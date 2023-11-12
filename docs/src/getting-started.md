```@meta
CurrentModule = SoleLogics
```

# [Getting started](@id man-core)

In this introductory section you will learn about the main building blocks of SoleLogics. Their definition, usage examples and how to customize them to your own needings. 

In a few words, you can consider this package as divided into two halves. 

The syntactical part deals with defining data structures to represent logical constructs such as assertions, logical constants, alphabets, grammars, crisp and fuzzy algebras, formulas etc. A consistent part of SoleLogics is devoted to randomly generate such structures, as well as parse and minimize formulas.

The semantic part deals with defining which rules must be applied when interpreting a logical formulas. The "semantic heart" of SoleLogics is its finite model checking algorithm, whose purpose is to efficiently check whether a formula is satisfied by an interpretation or not.

Please, feel free to use the following tree structures to orient yourself in the reading of this section. More pieces will be added to this type-hierarchy tree in the following sections.

[`Syntactical`](@ref)\
    ├── [`Formula`](@ref)\
    │   ├── [`AbstractSyntaxStructure`](@ref)\
    │   │   ├── [`SyntaxTree`](@ref)\
    │   │   │   ├── [`SyntaxLeaf`](@ref)\
    │   │   │   │   ├── [`Atom`](@ref)\
    │   │   │   │   └── [`Truth`](@ref)\
    │   │   │   │       └── ...\
    │   │   │   └── [`SyntaxBranch`](@ref)\
    │   │   └── ...\
    │   └── ...\
    └── [`Connective`](@ref)\
        ├── [`NamedConnective`](@ref) (e.g., ∧, ∨, →, ¬, □, ◊)\
        └── ...\

## [Syntax Basics](@id syntactical-base-definitions)

```@docs
Syntactical

Connective
arity

Formula
tree(φ::Formula)
height(φ::Formula)
tokens(φ::Formula)
composeformulas(c::Connective, φs::NTuple{N,F})::F where {N,F<:Formula}

AbstractSyntaxStructure
SyntaxTree
children(φ::SyntaxTree)
token(φ::SyntaxTree)
arity(φ::SyntaxTree)

SyntaxLeaf
SyntaxToken
dual(t::SyntaxToken)
Base.in(tok::SyntaxToken, φ::SyntaxTree)

Atom

Truth
istop(t::Truth)
isbot(t::Truth)
truthsupertype(T::Type{<:Truth})
```

```@docs
Operator
```
An [`Operator`](@ref) can be used to compose syntax tokens (e.g., [`Atom`](@ref)s), [`SyntaxTree`](@ref)s and/or [`Formula`](@ref)s.

```julia-repl
    ¬(Atom(1)) ∨ Atom(1) ∧ ⊤
    ∧(⊤,⊤)
    ⊤()
```

```@docs
SyntaxBranch
children(φ::SyntaxBranch)
token(φ::SyntaxBranch)
```

## [Semantics Basics](@id semantics-base-definitions)
```@docs
AbstractInterpretation
interpret(φ::Formula, i::AbstractInterpretation, args...; kwargs...)
check(φ::Formula, i::AbstractInterpretation, args...; kwargs...)
```