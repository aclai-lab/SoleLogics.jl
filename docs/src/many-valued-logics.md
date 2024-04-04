```@meta
CurrentModule = SoleLogics.ManyValuedLogics
```

```@contents
Pages = ["many-valued-logics.md"]
```

# [Introduction](@id many-valued-logics-introduction)
SoleLogics also provides tools to work with [many-valued logics](https://en.wikipedia.org/wiki/Many-valued_logic) (e.g., fuzzy logics), that is, logics with more truth values other than the classical Boolean ones `⊤` and `⊥`. With many-valued logics, the truth values are elements of a bounded lattice, providing a partial order between them, which encodes a *truer than* relation.

Most of the tools for dealing with these logics can be accessed by importing the ManyValuedLogics submodule:
```julia
using SoleLogics.ManyValuedLogics
```

# [Operation](@id many-valued-logics-operation)
```@docs
Operation
BinaryOperation
```

# [Axiom](@id many-valued-logics-axiom)
```@docs
Axiom
checkaxiom
```

## [Common axioms](@id many-valued-logics-common-axioms)
```@docs
Commutativity
Associativity
AbsorptionLaw
LeftIdentity
RightIdentity
IdentityElement
RightResidual
LeftResidual
ResiduationProperty
Implication1
Implication2
Implication3
DistributiveLaw
```

# [Finite algebra](@id many-valued-logics-finite-algebra)
```@docs
FiniteAlgebra
```

## [Monoid](@id many-valued-logics-monoid)
```@docs
Monoid
CommutativeMonoid
```

## [Finite lattice](@id many-valued-logics-finite-lattice)
```@docs
FiniteLattice
FiniteBoundedLattice
FiniteResiduatedLattice
```

## [Finite algebra varieties](@id many-valued-logics-finite-algebra-varieties)
```@docs
FiniteFLewAlgebra
FiniteHeytingAlgebra
```

# [Order utilities](@id many-valued-logics-order-utilities)
```@docs
precedeq
precedes
succeedeq
succeedes
```
