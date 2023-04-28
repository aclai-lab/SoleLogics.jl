# Syntax

```@docs
SoleLogics.AbstractSyntaxToken

arity(::Type{<:SoleLogics.AbstractSyntaxToken})

syntaxstring(φ::SoleLogics.AbstractFormula; kwargs...)

Proposition

negation(p::Proposition)

SoleLogics.AbstractSyntaxStructure

SyntaxTree

Base.in(tok::SoleLogics.AbstractSyntaxToken, f::SoleLogics.AbstractFormula)

ntokens

tree
```