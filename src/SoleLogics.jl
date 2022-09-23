module SoleLogics

"""
GENERAL TODOs:
    - Define the `Operators`` interface, that is subtype of AbstractArray?
    - Define the logic with:
        1) an `Alphabet``
        2) a set of `Operators` (based on the previous TODO)
        3) a name
    - Define a generator function for operators that uses @eval at runtime to create such
    operators
    - Define a generator for formulas
    - Pretty print a formula syntax tree
"""

import Base: show
using Reexport
using DataStructures

# Abstract types
export AbstractLogic, CrispLogic, FuzzyLogic

export AbstractOperator, AbstractUnaryOperator, AbstractBinaryOperator
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
export Logic
export MODAL_LOGIC, alphabet, operators

# Formula tree definitions exports
export Node, Formula
export token, formula, fhash, leftchild, rightchild, parent, size
export formula!, leftchild!, rightchild!, parent!, size!
export isleaf, height, modal_depth
export inorder
export subformulas, fnormalize!

# Formula tree input and construction
export shunting_yard, tree

# Formula tree generation
export gen_formula

@reexport using SoleAlphabets
@reexport using SoleWorlds
@reexport using SoleTraits

include("operators.jl")
include("logics.jl")
include("formulas.jl")

end
