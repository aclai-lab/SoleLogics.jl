d2 = Vector{BooleanTruth}([⊥, ⊤])

jt2 = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, ⊤) => ⊤
)

mt2 = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊥,
    (⊤, ⊥) => ⊥, (⊤, ⊤) => ⊤
)

j2 = BinaryOperation(d2, jt2)

m2 = BinaryOperation(d2, mt2)

booleanalgebra = FiniteFLewAlgebra(j2, m2, m2, ⊥, ⊤)
