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
export token, formula, leftchild, rightchild, parent
export formula!, leftchild!, rightchild!, parent!
export size, isleaf, height

export HSRELATIONS, HS₃RELATIONS, HS₇RELATIONS
export EXMODOP, UNIVMODOP
export Operators, @modaloperators
export reltype

@reexport using SoleAlphabets
@reexport using SoleWorlds

include("operators.jl")
include("formulas.jl")

end
