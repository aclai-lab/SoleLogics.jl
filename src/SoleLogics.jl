module SoleLogics

import Base: show
using DataStructures
using Dictionaries
using Reexport
@reexport using SoleBase

# Abstract types
export AbstractLogic, CrispLogic, FuzzyLogic

export AbstractOperator
export AbstractModalOperator
export AbstractExistentialModalOperator, AbstractUniversalModalOperator

# Concrete types, collections, wrappers, utilities related to operators
export UNOP, BINOP
export NEGATION, CONJUNCTION, DISJUNCTION, IMPLICATION
export EQUAL, GREATER, GREATER_EQUAL, LOWER, LOWER_EQUAL
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

# Model checking exports
#=
export Worlds, Adjacents
export KripkeModel, worlds, worlds!, adjacents, adjacents!, evaluations, evaluations!
export memo, contains, push!
export check
export gen_kmodel, dispense_alphabet
=#

# The following includes and reexports are needed in this exact order
include("Worlds/Worlds.jl")
@reexport using .Worlds

include("Relations/Relations.jl")
@reexport using .Relations

include("operators.jl")
include("logics.jl")

include("Alphabets/Alphabets.jl")
@reexport using .Alphabets

include("formulas.jl")

include("ModelChecking/checker.jl")
include("ModelChecking/op_behaviour.jl")
include("ModelChecking/generator.jl")

# include modelchecking files here

end
