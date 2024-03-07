module ManyValuedLogics

using ..SoleLogics

export BinaryOperation
export FiniteTruth
export Monoid, CommutativeMonoid
export FiniteLattice, FiniteBoundedLattice, FiniteResiduatedLattice
export FiniteFLewAlgebra, FiniteHeytingAlgebra
export precedeq, precedes, succeedeq, succeedes

include("core.jl")

end
