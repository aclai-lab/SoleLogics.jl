module ManyValuedLogics

using ..SoleLogics

export FiniteTruth

include("finitetruth.jl")

export BinaryOperation

include("operations.jl")

include("axioms.jl")

export Monoid, CommutativeMonoid
export FiniteLattice, FiniteBoundedLattice, FiniteResiduatedLattice
export FiniteFLewAlgebra, FiniteHeytingAlgebra
export getdomain

include("finite-algebras.jl")

export precedeq, precedes, succeedeq, succeedes

include("order-utilities.jl")

export BASE_MANY_VALUED_CONNECTIVES

include("many-valued-formulas.jl")

include("algebras/algebras.jl")

export alphacheck

include("check.jl")

end
