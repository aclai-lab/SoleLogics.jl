α = FiniteTruth("α")

d3 = Vector{FiniteTruth}([⊥, α, ⊤])

# a ∨ b = max{a, b}
jointable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, ⊤) => ⊤,
    (α, ⊥) => α, (α, α) => α, (α, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, ⊤) => ⊤
)

# a ∧ b = min{a, b}
meettable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => α, (α, ⊤) => α,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, ⊤) => ⊤
)

join = BinaryOperation(d3, jointable)
meet = BinaryOperation(d3, meettable)

G3 = FiniteFLewAlgebra(join, meet, meet, ⊥, ⊤)
