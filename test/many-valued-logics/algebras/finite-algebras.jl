using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: BinaryOperation, Monoid

domain = Set{BooleanTruth}([⊥, ⊤])

meettable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊥,
    (⊤, ⊥) => ⊥, (⊤, ⊤) => ⊤
)

meet = BinaryOperation(domain, meettable)

monoid = Monoid(meet, ⊤)
