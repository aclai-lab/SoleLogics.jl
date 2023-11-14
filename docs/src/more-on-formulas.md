```@meta
CurrentModule = SoleLogics
```

```@contents
Pages = ["more-on-formulas.md"]
```

# [More on Formulas](@id more-on-formulas-section)
In this chapter, you are initially going to learn more about [`Formula`](@ref) subtypes. For example, explicitly declare that a [`SyntaxTree`](@ref) (or some of its subtrees) represents a [normal form](https://en.wikipedia.org/wiki/Canonical_normal_form) can lead to great benefits, considering both computational and memory load.

We proceed by presenting the random formulae generation engine, parsing and some utility function.

Recalling the type hierarchy presented in [man-core](@ref), it is here enriched with the following new types and structures.

- [`Formula`](@ref)
    - [`AnchoredFormula`](@ref) **(new)**
    - [`AbstractSyntaxStructure`](@ref)
        - [`LeftmostLinearForm{C<:Connective,SS<:AbstractSyntaxStructure}`](@ref) **(new)**
---

# Linear Forms
```@docs
LeftmostLinearForm{C<:Connective,SS<:AbstractSyntaxStructure}

LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure}
LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure}

CNF{SS<:AbstractSyntaxStructure}
DNF{SS<:AbstractSyntaxStructure}
```

```@docs
AnchoredFormula
logic(φ::AnchoredFormula)
synstruct(φ::AnchoredFormula)

baseformula(φ::Formula; infer_logic = true, additional_operators::Union{Nothing,Vector{<:Operator}} = nothing, kwargs...)

parsebaseformula(expr::String, additional_operators::Union{Nothing,Vector{<:Operator}} = nothing; operators::Union{Nothing,Vector{<:Operator}}, grammar::Union{Nothing,AbstractGrammar} = nothing, algebra::Union{Nothing,AbstractAlgebra} = nothing, kwargs...)
```

# Random generation

TODO: complete this section

# Parsing

TODO: complete this section

# Utilities

```@docs
treewalk(st::SyntaxTree, args...; rng::AbstractRNG = Random.GLOBAL_RNG, criterion::Function = ntokens, toleaf::Bool = true, returnnode::Bool = false, transformnode::Function = nothing)

subformulas(f::Formula; sorted=true)

normalize(f::Formula; remove_boxes = true, reduce_negations = true, allow_atom_flipping = true)

isgrounded(f::Formula)
```
