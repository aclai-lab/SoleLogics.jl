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

# Formula tree related exports
export Node, Formula
export token, formula, leftchild, rightchild, parent, size
export formula!, leftchild!, rightchild!, parent!, size!
export isleaf, height
export inorder

# Abstract types
export AbstractOperator, AbstractUnaryOperator, AbstractBinaryOperator
export AbstractModalOperator
export AbstractExistentialModalOperator, AbstractUniversalModalOperator

# Concrete types, collections, wrappers, utilities
export UNOP, BINOP
export NEGATION, CONJUNCTION, DISJUNCTION, IMPLICATION
export Operators, reltype
export unary_operators, binary_operators
export isunaryoperator, isbinaryoperator

# Modal operators
export MODOP, EXMODOP, UNIVMODOP
export DIAMOND, BOX
export HSRELATIONS, HS₃RELATIONS, HS₇RELATIONS
export @modaloperators

@reexport using SoleAlphabets
@reexport using SoleWorlds

include("operators.jl")
include("formulas.jl")

end
