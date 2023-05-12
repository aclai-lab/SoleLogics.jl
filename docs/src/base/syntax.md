# Syntax

```@docs
SoleLogics.AbstractSyntaxToken

arity(::Type{<:SoleLogics.AbstractSyntaxToken})

syntaxstring(tok::SoleLogics.AbstractSyntaxToken; kwargs...)

Proposition

negation(p::Proposition)

SoleLogics.AbstractSyntaxStructure

SyntaxTree

Base.in(tok::SoleLogics.AbstractSyntaxToken, f::SoleLogics.AbstractFormula)

ntokens

tree
```