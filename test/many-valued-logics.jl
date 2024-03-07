using SoleLogics
using SoleLogics.ManyValuedLogics

domain = Set{BooleanTruth}([⊥, ⊤])

jointable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, ⊤) => ⊤
)

meettable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊥,
    (⊤, ⊥) => ⊥, (⊤, ⊤) => ⊤
)

join = BinaryOperation(domain, jointable)

meet = BinaryOperation(domain, meettable)

monoid = CommutativeMonoid(meet, ⊤)

ffa = FiniteFLewAlgebra(join, meet, monoid, ⊥, ⊤)

@test ffa.implication(⊥, ⊥) == ⊤
@test ffa.implication(⊥, ⊤) == ⊤
@test ffa.implication(⊤, ⊥) == ⊥
@test ffa.implication(⊤, ⊤) == ⊤

implicationtable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊤,
    (⊥, ⊤) => ⊤,
    (⊤, ⊥) => ⊥,
    (⊤, ⊤) => ⊤
)

implication = BinaryOperation(domain, implicationtable)

fha = FiniteHeytingAlgebra(join, meet, implication, ⊥, ⊤)
