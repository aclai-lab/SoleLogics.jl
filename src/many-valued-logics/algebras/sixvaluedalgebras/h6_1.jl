α = FiniteTruth("α")
β = FiniteTruth("β")
γ = FiniteTruth("γ")
δ = FiniteTruth("δ")

d6 = Vector{FiniteTruth}([⊥, α, β, γ, δ, ⊤])

# a ∨ b = max{a, b}
jointable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, β) => β, (⊥, γ) => γ, (⊥, δ) => δ, (⊥, ⊤) => ⊤,
    (α, ⊥) => α, (α, α) => α, (α, β) => γ, (α, γ) => γ, (α, δ) => δ, (α, ⊤) => ⊤,
    (β, ⊥) => β, (β, α) => γ, (β, β) => β, (β, γ) => γ, (β, δ) => δ, (β, ⊤) => ⊤,
    (γ, ⊥) => γ, (γ, α) => γ, (γ, β) => γ, (γ, γ) => γ, (γ, δ) => δ, (γ, ⊤) => ⊤,
    (δ, ⊥) => δ, (δ, α) => δ, (δ, β) => δ, (δ, γ) => δ, (δ, δ) => δ, (δ, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, β) => ⊤, (⊤, γ) => ⊤, (⊤, δ) => ⊤, (⊤, ⊤) => ⊤
)

# a ∧ b = min{a, b}
meettable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, β) => ⊥, (⊥, γ) => ⊥, (⊥, δ) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => α, (α, β) => ⊥, (α, γ) => α, (α, δ) => α, (α, ⊤) => α,
    (β, ⊥) => ⊥, (β, α) => ⊥, (β, β) => β, (β, γ) => β, (β, δ) => β, (β, ⊤) => β,
    (γ, ⊥) => ⊥, (γ, α) => α, (γ, β) => β, (γ, γ) => γ, (γ, δ) => γ, (γ, ⊤) => γ,
    (δ, ⊥) => ⊥, (δ, α) => α, (δ, β) => β, (δ, γ) => γ, (δ, δ) => δ, (δ, ⊤) => δ,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, β) => β, (⊤, γ) => γ, (⊤, δ) => δ, (⊤, ⊤) => ⊤
)

join = BinaryOperation(d6, jointable)
meet = BinaryOperation(d6, meettable)

H6_1 = FiniteFLewAlgebra(join, meet, meet, ⊥, ⊤)