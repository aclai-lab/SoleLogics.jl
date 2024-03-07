module ManyValuedLogics

using ..SoleLogics

export BinaryOperation, Monoid, CommutativeMonoid
export FiniteLattice, FiniteBoundedLattice, FiniteResiduatedLattice
export FiniteFLewAlgebra, FiniteHeytingAlgebra
export precedeq, precedes, succeedeq, succeedes

include("core.jl")

end
