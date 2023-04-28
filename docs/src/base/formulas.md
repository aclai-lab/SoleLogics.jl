# Formulas

```@docs
SoleLogics.AbstractFormula

SoleLogics.joinformulas(op::SoleLogics.AbstractOperator, ::NTuple{N,F}) where {N,F<:SoleLogics.AbstractFormula}

tokens(f::SoleLogics.AbstractFormula)::AbstractVector{<:SoleLogics.AbstractSyntaxToken}

operators(f::SoleLogics.AbstractFormula)

propositions(f::SoleLogics.AbstractFormula)

ntokens(f::SoleLogics.AbstractFormula)

npropositions(f::SoleLogics.AbstractFormula)

height
```