α = FiniteTruth("α")
β = FiniteTruth("β")
γ = FiniteTruth("γ")

d5 = Vector{FiniteTruth}([⊥, α, β, γ, ⊤])

# a ∨ b = max{a, b}
jointable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, β) => β, (⊥, γ) => γ, (⊥, ⊤) => ⊤,
    (α, ⊥) => α, (α, α) => α, (α, β) => β, (α, γ) => γ, (α, ⊤) => ⊤,
    (β, ⊥) => β, (β, α) => β, (β, β) => β, (β, γ) => γ, (β, ⊤) => ⊤,
    (γ, ⊥) => γ, (γ, α) => γ, (γ, β) => γ, (γ, γ) => γ, (γ, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, β) => ⊤, (⊤, γ) => ⊤, (⊤, ⊤) => ⊤
)

# a ∧ b = min{a, b}
meettable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, β) => ⊥, (⊥, γ) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => α, (α, β) => α, (α, γ) => α, (α, ⊤) => α,
    (β, ⊥) => ⊥, (β, α) => α, (β, β) => β, (β, γ) => β, (β, ⊤) => β,
    (γ, ⊥) => ⊥, (γ, α) => α, (γ, β) => β, (γ, γ) => γ, (γ, ⊤) => γ,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, β) => β, (⊤, γ) => γ, (⊤, ⊤) => ⊤
)

join = BinaryOperation(d5, jointable)
meet = BinaryOperation(d5, meettable)

G5 = FiniteFLewAlgebra(join, meet, meet, ⊥, ⊤)
