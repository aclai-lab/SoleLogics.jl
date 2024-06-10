α = FiniteTruth("α")
β = FiniteTruth("β")
γ = FiniteTruth("γ")
δ = FiniteTruth("δ")

d6 = Vector{FiniteTruth}([⊥, α, β, γ, δ, ⊤])

jointable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, β) => β, (⊥, γ) => γ, (⊥, δ) => δ, (⊥, ⊤) => ⊤,
    (α, ⊥) => α, (α, α) => α, (α, β) => δ, (α, γ) => γ, (α, δ) => δ, (α, ⊤) => ⊤,
    (β, ⊥) => β, (β, α) => δ, (β, β) => β, (β, γ) => ⊤, (β, δ) => δ, (β, ⊤) => ⊤,
    (γ, ⊥) => γ, (γ, α) => γ, (γ, β) => ⊤, (γ, γ) => γ, (γ, δ) => ⊤, (γ, ⊤) => ⊤,
    (δ, ⊥) => δ, (δ, α) => δ, (δ, β) => δ, (δ, γ) => ⊤, (δ, δ) => δ, (δ, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, β) => ⊤, (⊤, γ) => ⊤, (⊤, δ) => ⊤, (⊤, ⊤) => ⊤
)

meettable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, β) => ⊥, (⊥, γ) => ⊥, (⊥, δ) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => α, (α, β) => ⊥, (α, γ) => α, (α, δ) => α, (α, ⊤) => α,
    (β, ⊥) => ⊥, (β, α) => ⊥, (β, β) => β, (β, γ) => ⊥, (β, δ) => β, (β, ⊤) => β,
    (γ, ⊥) => ⊥, (γ, α) => α, (γ, β) => ⊥, (γ, γ) => γ, (γ, δ) => α, (γ, ⊤) => γ,
    (δ, ⊥) => ⊥, (δ, α) => α, (δ, β) => β, (δ, γ) => α, (δ, δ) => δ, (δ, ⊤) => δ,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, β) => β, (⊤, γ) => γ, (⊤, δ) => δ, (⊤, ⊤) => ⊤
)

join = BinaryOperation(d6, jointable)

meet = BinaryOperation(d6, meettable)

H6 = FiniteFLewAlgebra(join, meet, meet, ⊥, ⊤)