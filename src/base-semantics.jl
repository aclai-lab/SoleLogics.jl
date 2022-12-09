"""
    struct NamedOperator{Symbol} <: AbstractOperator end

A singleton type for representing operators defined by a name or a symbol.
For example, the AND operator (logical conjuction) can be defined as the subtype:

    const CONJUNCTION = NamedOperator{:∧}()
    const ∧ = CONJUNCTION
    arity(::typeof(∧)) = 2

See also [`NEGATION`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref), [`IMPLICATION`](@ref), [`AbstractOperator`](@ref).
"""
struct NamedOperator{Symbol} <: AbstractOperator end

doc_NEGATION = """
    const NEGATION = TruthOperator{:¬}()
    const ¬ = NEGATION
    arity(::typeof(¬)) = 1

Logical negation.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_NEGATION)
"""
const NEGATION = NamedOperator{:¬}()
"""
$(doc_NEGATION)
"""
const ¬ = NEGATION
arity(::typeof(¬)) = 1

doc_CONJUNCTION = """
    const CONJUNCTION = NamedOperator{:∧}()
    const ∧ = CONJUNCTION
    arity(::typeof(∧)) = 2

Logical conjunction.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_CONJUNCTION)
"""
const CONJUNCTION = NamedOperator{:∧}()
"""
$(doc_CONJUNCTION)
"""
const ∧ = CONJUNCTION
arity(::typeof(∧)) = 2

doc_DISJUNCTION = """
    const DISJUNCTION = NamedOperator{:∨}()
    const ∨ = DISJUNCTION
    arity(::typeof(∨)) = 2

Logical disjunction.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_DISJUNCTION)
"""
const DISJUNCTION = NamedOperator{:∨}()
"""
$(doc_DISJUNCTION)
"""
const ∨ = DISJUNCTION
arity(::typeof(∨)) = 2

doc_IMPLICATION = """
    const IMPLICATION = NamedOperator{:⟹}()
    const ⟹ = IMPLICATION
    arity(::typeof(⟹)) = 2

Logical implication.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_IMPLICATION)
"""
const IMPLICATION = NamedOperator{:⟹}()
"""
$(doc_IMPLICATION)
"""
const ⟹ = IMPLICATION
arity(::typeof(⟹)) = 2

"""
    const base_operators = [⊤, ⊥, ¬, ∧, ∨, ⟹]

Basic logical operators.

See also [`TOP`](@ref), [`BOTTOM`](@ref), [`NEGATION`](@ref), [`CONJUCTION`](@ref), [`AbstractOperator`](@ref).
"""
const base_operators = [⊤, ⊥, ¬, ∧, ∨, ⟹]
const BaseOperators = Union{typeof.(base_operators)...}

# This can be useful for standard phrasing of propositional formulas with string propositions.
const base_grammar = CompleteGrammar(AlphabetOfAny{String}(), base_operators)

"""
    struct BooleanAlgebra <: AbstractAlgebra end

[https://en.m.wikipedia.org/wiki/Boolean_algebra](Boolean algebra) is defined on the values
`true` (top) and `false` (bottom). For this algebra, the basic operators negation,
conjunction and disjunction (stylized as ¬, ∧, ∨) can be defined as the complement, minimum
and maximum, respectively.

See also [`Truth`](@ref).
"""
struct BooleanAlgebra <: AbstractAlgebra end

domain(a::BooleanAlgebra) = [true,false]
top(a::BooleanAlgebra) = true
bottom(a::BooleanAlgebra) = false

# Standard semantics for NOT, AND, OR, IMPLIES
collate_truth(a::AbstractAlgebra, o::typeof(¬), (t,)::NTuple{1}) = (!t)
collate_truth(a::AbstractAlgebra, o::typeof(∧), (t1, t2)::NTuple{2}) = min(t1, t2)
collate_truth(a::AbstractAlgebra, o::typeof(∨), (t1, t2)::NTuple{2}) = max(t1, t2)
collate_truth(a::AbstractAlgebra, o::typeof(⟹), (t1, t2)::NTuple{2}) =
    collate_truth(a, ∨, (!(t1), t2))

"""
    struct BaseLogic{G<:AbstractGrammar, A<:AbstractAlgebra} <: AbstractLogic{G, A}
        grammar::G
        algebra::A
    end

Basic logic type based on a grammar and an algebra, where both grammar and algebra
are instantiated.
"""
struct BaseLogic{G<:AbstractGrammar, A<:AbstractAlgebra} <: AbstractLogic{G, A}
    grammar::G
    algebra::A

    function BaseLogic{G, A}(
        grammar::G = base_grammar,
        algebra::A = BooleanAlgebra(),
    ) where {G<:AbstractGrammar, A<:AbstractAlgebra}
        # @assert all([goeswith(op, algebra) for op in operators(grammar)]) "Cannot instantiate BaseLogic{$(G), $(A)}: operators $(operators(grammar)[[goeswith(op, algebra) for op in operators(grammar)]]) cannot be interpreted on $(algebra)." # requires `goeswith` trait
        new{G, A}(grammar, algebra)
    end

    function BaseLogic{G}(
        grammar::G = base_grammar,
        algebra::A = BooleanAlgebra(),
    ) where {G<:AbstractGrammar, A<:AbstractAlgebra}
        BaseLogic{G, A}(grammar, algebra)
    end

    function BaseLogic(
        grammar::G = base_grammar,
        algebra::A = BooleanAlgebra(),
    ) where {G<:AbstractGrammar, A<:AbstractAlgebra}
        BaseLogic{G, A}(grammar, algebra)
    end
end

grammar(l::BaseLogic) = l.grammar
algebra(l::BaseLogic) = l.algebra

const base_logic = BaseLogic(base_grammar, BooleanAlgebra())

# TODO...
abstract type AbstractLogicalModel end
