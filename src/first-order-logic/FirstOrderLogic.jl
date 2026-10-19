"""
    module FirstOrderLogics

First-order logic (FOL) on top of the SoleLogics syntax and semantics machinery.

Atomic formulas are [`PredicateAtom`](@ref)s (which are `AbstractAtom`s), and the
quantifiers [`ForAll`](@ref) and [`Exists`](@ref) are unary `Connective`s carrying the
variable they bind; therefore, first-order formulas are ordinary `SyntaxTree`s and can be
freely combined with the propositional connectives `¬`, `∧`, `∨` and `→`.
Formulas are interpreted on finite first-order structures ([`FOStructure`](@ref)).

See also [`Term`](@ref), [`Predicate`](@ref), [`FOStructure`](@ref).
"""
module FirstOrderLogics

using ..SoleLogics
using ..SoleLogics: AbstractSyntaxBranch, SyntaxLeaf, simplify

import ..SoleLogics: syntaxstring, arity, precedence, associativity, name
import ..SoleLogics: hasdual, dual, interpret, domain

export Term, Variable, Constant, FunctionSymbol, FunctionTerm
export Predicate, PredicateAtom, EQUALITY, ≐
export predicate, arguments, functionsymbol, variable

export AbstractQuantifier, ForAll, Exists, forall, exists, ∀, ∃
export isquantifier

export variables, freevariables, boundvariables, issentence
export predicates, constants, functionsymbols
export substitute

export @variables, @constants

include("syntax.jl")

export FOStructure, domain, evaluate

include("semantics.jl")

end
