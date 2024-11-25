using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: G3, α, FiniteIndexTruth, FiniteIndexFLewAlgebra
@atoms p q
@test_nowarn check(parseformula("p∧q→q"), TruthDict([p => α, q => ⊥]), G3)
@test_nowarn check(parseformula("p∧q→q"), TruthDict([p => convert(FiniteIndexTruth, α), q => ⊥]), convert(FiniteIndexFLewAlgebra, G3))
