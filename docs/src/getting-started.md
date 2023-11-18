```@meta
CurrentModule = SoleLogics
```

# [Getting started](@id man-core)

In this introductory section you will learn about the main building blocks of SoleLogics. Their definition, usage examples and how to customize them to your own needings. 

In a few words, you can consider this package as divided into two halves. 

The [syntactical](https://en.wikipedia.org/wiki/Syntax) half deals with defining data structures to represent logical constructs such as assertions, logical constants, alphabets, grammars, crisp and fuzzy algebras, formulas etc. A consistent part of SoleLogics is devoted to randomly generate such structures, as well as parse and minimize formulas.

The [semantic](https://en.wikipedia.org/wiki/Semantics) half deals with defining which rules must be applied when interpreting a logical formulas. The "semantic heart" of SoleLogics is its finite [model checking](https://en.wikipedia.org/wiki/Model_checking) algorithm, whose purpose is to efficiently check whether a formula is satisfied by an interpretation or not.

Please, feel free to use the following tree structures to orient yourself in the reading of this section. More pieces will be added to this type-hierarchy tree in the following sections.

## Syntax and Semantics tree type hierarchies
- [`Syntactical`](@ref)
    - [`Connective`](@ref)                      (e.g., ∧, ∨, ¬, →)
    - [`Formula`](@ref)
        - [`AbstractSyntaxStructure`](@ref)
            - [`SyntaxTree`](@ref)              (e.g., ¬p ∧ q → s)
                - [`SyntaxLeaf`](@ref)
                    - [`Atom`](@ref)            (e.g., p, q)
                    - [`Literal`](@ref)         (an [`Atom`](@ref) p, or its negation ¬p)
                    - [`Truth`](@ref)           (e.g., ⊤, ⊥)
                - [`SyntaxBranch`](@ref)        (e.g., p ∧ q)
---

- [`AbstractInterpretation`](@ref) (e.g., p is ⊤, equivalent to p is true in boolean logic)

---

Also, two union types are defined:
- [`Operator`](@ref), that is, `Union{Connective,Truth}`, 
- [`SyntaxToken`](@ref), that is, `Union{Atom,Connective}`.  
        
## [Syntax Basics](@id syntactical-base-definitions)

```@docs
Syntactical
```

To print out a generic [`Syntactical`](@ref) element, we must define how it is converted into a string. To do this, we can implement a custom [`syntaxstring`](@ref).

```@docs
syntaxstring(s::Syntactical; kwargs...)
```

```@docs
Connective
```

If the definition above overwhelms you, don't worry: it will be clearer later. For now we are simply interested in understanding that [`Connective`](@ref)s are simply symbols used to concatenate other logical constructs with each other. 

Later, we will see some interesting example about how to equip these symbols with semantics, that is, what rules should be applied when interpreting connectives in a generic [`Formula`](@ref). We will also understand how to define our own custom connectives.

```@docs
arity(φ::SyntaxTree)
```

The vast majority of data structures involved in encoding a logical formula, are children of the [`Formula`](@ref) abstract type. When such data structures purely represents tree-shaped data structures (or single nodes in them), then they are also children of the [`AbstractSyntaxStructure`](@ref) abstract type.

```@docs
Formula
```

The following methods define [`Formula`](@ref) interface.

```@docs
tree(φ::Formula)
height(φ::Formula)
tokens(φ::Formula)
```

Now, let us see how to *compose* syntax elements, to express more complex concepts.

```@docs
composeformulas(c::Connective, φs::NTuple{N,F}) where {N,F<:Formula}
```

We are ready to see how logical formulas are represented using syntax trees

```@docs
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
Literal{T<:SyntaxToken}
```

```@docs
Truth
```

Similarly to the [`Connective`](@ref)s case, [`Truth`](@ref) explanation could be unfamiliar at first sight. At the moment, what is of our interest is that SoleLogics provides us a simple interface to create custom, complex at will, algebras without worrying about adapting all the underlying algorithms (e.g., formulas generation, parsing, model checking etc.).

```@docs
istop(t::Truth)
isbot(t::Truth)
truthsupertype(T::Type{<:Truth})
```

The union of [`Connective`](@ref)s and [`Truth`](@ref) values are exactly what is called *logical operators*, or simply [`Operator`](@ref). In SoleLogics, logical operators are splitted in two parts to highlight some differences that always holds (e.g., [`Truth`](@ref) values arity is always 0, while [`Connective`](@ref)s arity is always greater than 0); apart from this technical decision, many dispatches are defined using the more general union type [`Operator`](@ref).

```@docs
Operator
```
An [`Operator`](@ref) can be used to compose syntax tokens (e.g., [`Atom`](@ref)s), [`SyntaxTree`](@ref)s and/or [`Formula`](@ref)s.

```julia-repl
    ¬(Atom(1)) ∨ Atom(1) ∧ ⊤
    ∧(⊤,⊤)
    ⊤()
```

The internal nodes in a [`SyntaxTree`](@ref) definitely have [`ariety`](@ref) greater than `0`, and thus, cannot wrap [`Atom`](@ref)s nor [`Truth`](@ref) values. To clearly distinguish internal nodes and leaves, the [`SyntaxBranch`](@ref) type is defined, making each [`SyntaxTree`](@ref) arity-complaint.

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
