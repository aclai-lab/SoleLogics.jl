α = FiniteTruth("α")
β = FiniteTruth("β")

d4 = Vector{FiniteTruth}([⊥, α, β, ⊤])

djointable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, β) => β, (⊥, ⊤) => ⊤,
    (α, ⊥) => α, (α, α) => α, (α, β) => ⊤, (α, ⊤) => ⊤,
    (β, ⊥) => β, (β, α) => ⊤, (β, β) => β, (β, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, β) => ⊤, (⊤, ⊤) => ⊤
)

dmeettable = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, β) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => α, (α, β) => ⊥, (α, ⊤) => α,
    (β, ⊥) => ⊥, (β, α) => ⊥, (β, β) => β, (β, ⊤) => β,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, β) => β, (⊤, ⊤) => ⊤
)

djoin = BinaryOperation(d4, djointable)
dmeet = BinaryOperation(d4, dmeettable)

H4 = FiniteFLewAlgebra(djoin, dmeet, dmeet, ⊥, ⊤)
