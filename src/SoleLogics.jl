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

include("operators.jl")
include("formulas.jl")

end
