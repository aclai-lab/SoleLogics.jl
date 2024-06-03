α = FiniteTruth("α")
β = FiniteTruth("β")

d4 = Vector{FiniteTruth}([⊥, α, β, ⊤])

# a ∨ b = max{a, b}
jointable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, β) => β, (⊥, ⊤) => ⊤,
    (α, ⊥) => α, (α, α) => α, (α, β) => β, (α, ⊤) => ⊤,
    (β, ⊥) => β, (β, α) => β, (β, β) => β, (β, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, β) => ⊤, (⊤, ⊤) => ⊤
)

# a ∧ b = min{a, b}
meettable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, β) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => α, (α, β) => α, (α, ⊤) => α,
    (β, ⊥) => ⊥, (β, α) => α, (β, β) => β, (β, ⊤) => β,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, β) => β, (⊤, ⊤) => ⊤
)

join = BinaryOperation(d4, jointable)
meet = BinaryOperation(d4, meettable)

G4 = FiniteFLewAlgebra(join, meet, meet, ⊥, ⊤)
