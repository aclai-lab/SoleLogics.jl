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
