module SoleLogics

import Base: show
using DataStructures
using Dictionaries
using Reexport
@reexport using SoleTraits

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

# Previous dependecies within SoleAlphabet:
# NOTE: will be deprecated
const Letter = String
const LetterAlphabet = Vector{Letter}
SoleTraits.is_proposition(::Letter) = true

include("operators.jl")
include("logics.jl")
include("formulas.jl")

# Submodules reexporting
include("Relations/Relations.jl")
@reexport using .Relations

include("Worlds/Worlds.jl")
@reexport using .Worlds

include("Alphabets/Alphabets.jl")
@reexport using .Alphabets

end
