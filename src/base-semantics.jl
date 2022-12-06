"""
    struct NamedOperator{T<:Symbol} <: AbstractOperator end

A singleton type for representing operators defined by a name or a symbol.
For example, the AND operator (logical conjuction) can be defined as the subtype:

    CONJUNCTION = NamedOperator{:∧}

See also [`AbstractOperator`](@ref).
"""
struct NamedOperator{Symbol} <: AbstractOperator end

const NEGATION = NamedOperator{:¬}()
const ¬ = NEGATION
arity(::typeof(¬)) = 1

const CONJUNCTION = NamedOperator{:∧}()
const ∧ = CONJUNCTION
arity(::typeof(∧)) = 2

const DISJUNCTION = NamedOperator{:∨}()
const ∨ = DISJUNCTION
arity(::typeof(∨)) = 2

const IMPLICATION = NamedOperator{:⟹}()
const ⟹ = IMPLICATION
arity(::typeof(⟹)) = 2


const base_operators = [⊤, ⊥, ¬, ∧, ∨, ⟹]
const BaseOperators = Union{typeof.(base_operators)...}

const base_grammar = CompleteGrammar(AlphabetOfAny{String}(), base_operators)

"""
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

collate_truth(a::AbstractAlgebra, o::typeof(¬), t::NTuple{1}) = (!t)
collate_truth(a::AbstractAlgebra, o::typeof(∧), (t1, t2)::NTuple{2}) = min(t1, t2)
collate_truth(a::AbstractAlgebra, o::typeof(∨), (t1, t2)::NTuple{2}) = max(t1, t2)
collate_truth(a::AbstractAlgebra, o::typeof(⟹), (t1, t2)::NTuple{2}) =
    collate_truth(a, ∨, (!(t1), t2))

# TODO complete description
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
