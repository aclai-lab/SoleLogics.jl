```@meta
CurrentModule = SoleLogics
```

```@contents
Pages = ["more-on-formulas.md"]
```

# [More on Formulas](@id more-on-formulas-section)
In this chapter, you are going to learn more on [`Formula`](@ref) representations that are alternative to syntax trees. As you will see, for example, formulas with specific structure (e.g., [normal forms](https://en.wikipedia.org/wiki/Canonical_normal_form)) can be represented in ways that make them more easy to handle, and can lead to great benefits in terms of both computational and memory load.

We proceed by presenting the random formulae generation engine, parsing and some utility function.

Recalling the type hierarchy presented in [man-core](@ref), it is here enriched with the following new types and structures.

- [`Formula`](@ref)
    - [`AnchoredFormula`](@ref)
    - [`AbstractSyntaxStructure`](@ref)
        - [`Literal`](@ref)
        - [`LeftmostLinearForm{C<:Connective,SS<:AbstractSyntaxStructure}`](@ref)
---

## Literals
```@docs
Literal
```

## Linear Forms
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

## Random sampling and generation

```@docs
Base.rand(alphabet::AbstractAlphabet, args...; kwargs...)
SoleLogics.randatom
StatsBase.sample(alphabet::AbstractAlphabet, weights::AbstractWeights, args...; kwargs...)

randformula(height::Integer, alphabet, operators::AbstractVector; rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG)
```

## Parsing

```@docs
BASE_PARSABLE_CONNECTIVES

parseformula(F::Type{<:SyntaxTree}, expr::String, additional_operators::Union{Nothing,AbstractVector} = nothing; function_notation::Bool = false, atom_parser::Base.Callable = Atom{String}, additional_whitespaces::Vector{Char} = Char[], opening_parenthesis::String = DEFAULT_OPENING_PARENTHESIS, closing_parenthesis::String = DEFAULT_CLOSING_PARENTHESIS, arg_delim::String = DEFAULT_ARG_DELIM)
```

## Utilities

```@docs
treewalk(st::SyntaxTree, args...; rng::AbstractRNG = Random.GLOBAL_RNG, criterion::Function = ntokens, toleaf::Bool = true, returnnode::Bool = false, transformnode::Function = nothing)

subformulas(f::Formula; sorted=true)

normalize(f::Formula; remove_boxes = true, reduce_negations = true, allow_atom_flipping = true)

isgrounded(f::Formula)
```
