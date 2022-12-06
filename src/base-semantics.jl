############################################################################################
########################################## SYNTAX ##########################################
############################################################################################

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
BaseOperators = Union{typeof.(base_operators)...}

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


struct BaseLogic{G<:AbstractGrammar, A<:AbstractAlgebra} <: AbstractLogic{G, A}
    grammar::G
    algebra::A
end
