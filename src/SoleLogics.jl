module SoleLogics

import Base: show
using DataStructures
using Dictionaries
using Reexport

# Abstract types
export AbstractLogic, CrispLogic, FuzzyLogic

export AbstractOperator
# deprecated: AbstractUnaryOperator, AbstractBinaryOperator. Use ariety(op) instead
export AbstractModalOperator
export AbstractExistentialModalOperator, AbstractUniversalModalOperator

# Concrete types, collections, wrappers, utilities related to operators
export UNOP, BINOP
export NEGATION, CONJUNCTION, DISJUNCTION, IMPLICATION
export Operators, reltype, ariety, precedence

# Modal operators
export EXMODOP, UNIVMODOP
export DIAMOND, BOX
export @modaloperators

# Modal logic extensions
export HSRELATIONS, HS₃RELATIONS, HS₇RELATIONS

# Defined logics
export Logic, alphabet, operators
export PROPOSITIONAL_LOGIC, MODAL_LOGIC, DEFAULT_LOGIC

# Formula tree definitions exports
export FNode, Formula
export token, formula, logic, fhash, leftchild, rightchild, parent, size, tree
export formula!, leftchild!, rightchild!, parent!, size!
export isleaf, height, modal_depth, inorder
export subformulas, fnormalize!

# Formula tree input and construction
export shunting_yard, build_tree

# Formula tree generation
export gen_formula

@reexport using SoleAlphabets
@reexport using SoleWorlds
@reexport using SoleTraits

include("operators.jl")
include("logics.jl")
include("formulas.jl")

end
