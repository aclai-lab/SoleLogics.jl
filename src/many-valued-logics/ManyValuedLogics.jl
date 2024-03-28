module ManyValuedLogics

using ..SoleLogics

export BinaryOperation

include("operations.jl")

include("axioms.jl")

export FiniteTruth
export Monoid, CommutativeMonoid
export FiniteLattice, FiniteBoundedLattice, FiniteResiduatedLattice
export FiniteFLewAlgebra, FiniteHeytingAlgebra
export getdomain

include("finite-algebras.jl")

export precedeq, precedes, succeedeq, succeedes

include("order-utilities.jl")

export ⋅

include("many-valued-formulas.jl")

end
