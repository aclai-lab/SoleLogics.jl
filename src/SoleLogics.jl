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

export Node, Formula
export token, formula, leftchild, rightchild, parent, height
export formula!, leftchild!, rightchild!, parent!, height!
export size, isleaf

export AbstractOperator, AbstractUnaryOperator, AbstractBinaryOperator
export AbstractModalOperator
export AbstractExistentialModalOperator, AbstractUniversalModalOperator

export NEGATION, DIAMOND, BOX
export CONJUNCTION, DISJUNCTION, IMPLICATION

export HSRELATIONS, HS₃RELATIONS, HS₇RELATIONS
export EXMODOP, UNIVMODOP
export Operators, unary_operators, binary_operators, @modaloperators
export isunaryoperator, isbinaryoperator
export operators_precedence, operator
export reltype

@reexport using SoleAlphabets
@reexport using SoleWorlds

include("operators.jl")
include("formulas.jl")

end
