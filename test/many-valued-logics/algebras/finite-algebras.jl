using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: BinaryOperation

domain = Set{BooleanAlgebra}([⊥, ⊤])

meettable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊥,
    (⊤, ⊥) => ⊥, (⊤, ⊤) => ⊤
)

meet = BinaryOperation(domain, meettable)
