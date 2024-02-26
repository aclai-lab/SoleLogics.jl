using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: FLewTruth, FLewAlgebra, meet, join, monoid

⊥, α, β, γ, ⊤ = FLewTruth.(["⊥", "α", "β", "γ", "⊤"])
elements = Set{FLewTruth}([⊥, α, β, ⊤])

jointable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => α, (⊥,β) => β, (⊥,⊤) => ⊤,
    (α,⊥) => α, (α,α) => α, (α,β) => ⊤, (α,⊤) => ⊤,
    (β,⊥) => β, (β,α) => ⊤, (β,β) => β, (β,⊤) => ⊤,
    (⊤,⊥) => ⊤, (⊤,α) => ⊤, (⊤,β) => ⊤, (⊤,⊤) => ⊤,
)
meettable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => ⊥, (⊥,β) => ⊥, (⊥,⊤) => ⊥,
    (α,⊥) => ⊥, (α,α) => α, (α,β) => ⊥, (α,⊤) => α,
    (β,⊥) => ⊥, (β,α) => ⊥, (β,β) => β, (β,⊤) => β,
    (⊤,⊥) => ⊥, (⊤,α) => α, (⊤,β) => β, (⊤,⊤) => ⊤,
)
monoidtable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => ⊥, (⊥,β) => ⊥, (⊥,⊤) => ⊥,
    (α,⊥) => ⊥, (α,α) => α, (α,β) => ⊥, (α,⊤) => α,
    (β,⊥) => ⊥, (β,α) => ⊥, (β,β) => β, (β,⊤) => β,
    (⊤,⊥) => ⊥, (⊤,α) => α, (⊤,β) => β, (⊤,⊤) => ⊤,
)

incompletejointable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => α, (⊥,β) => β, (⊥,⊤) => ⊤,
    (α,⊥) => α,             (α,β) => ⊤, (α,⊤) => ⊤,
    (β,⊥) => β, (β,α) => ⊤, (β,β) => β, (β,⊤) => ⊤,
    (⊤,⊥) => ⊤, (⊤,α) => ⊤, (⊤,β) => ⊤, (⊤,⊤) => ⊤,
)
incompletemeettable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => ⊥, (⊥,β) => ⊥, (⊥,⊤) => ⊥,
    (α,⊥) => ⊥,             (α,β) => ⊥, (α,⊤) => α,
    (β,⊥) => ⊥, (β,α) => ⊥, (β,β) => β, (β,⊤) => β,
    (⊤,⊥) => ⊥, (⊤,α) => α, (⊤,β) => β, (⊤,⊤) => ⊤,
)
incompletemonoidtable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => ⊥, (⊥,β) => ⊥, (⊥,⊤) => ⊥,
    (α,⊥) => ⊥,             (α,β) => ⊥, (α,⊤) => α,
    (β,⊥) => ⊥, (β,α) => ⊥, (β,β) => β, (β,⊤) => β,
    (⊤,⊥) => ⊥, (⊤,α) => α, (⊤,β) => β, (⊤,⊤) => ⊤,
)

overflowingjointable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => α, (⊥,β) => β, (⊥,⊤) => ⊤,
    (α,⊥) => α, (α,α) => α, (α,β) => ⊤, (α,⊤) => ⊤,
    (β,⊥) => β, (β,α) => ⊤, (β,β) => β, (β,⊤) => ⊤,
    (γ,⊥) => γ,
    (⊤,⊥) => ⊤, (⊤,α) => ⊤, (⊤,β) => ⊤, (⊤,⊤) => ⊤,
)
overflowingmeettable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => ⊥, (⊥,β) => ⊥, (⊥,⊤) => ⊥,
    (α,⊥) => ⊥, (α,α) => α, (α,β) => ⊥, (α,⊤) => α,
    (β,⊥) => ⊥, (β,α) => ⊥, (β,β) => β, (β,⊤) => β,
    (γ,⊥) => γ,
    (⊤,⊥) => ⊥, (⊤,α) => α, (⊤,β) => β, (⊤,⊤) => ⊤,
)
overflowingmonoidtable = Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}(
    (⊥,⊥) => ⊥, (⊥,α) => ⊥, (⊥,β) => ⊥, (⊥,⊤) => ⊥,
    (α,⊥) => ⊥, (α,α) => α, (α,β) => ⊥, (α,⊤) => α,
    (β,⊥) => ⊥, (β,α) => ⊥, (β,β) => β, (β,⊤) => β,
    (γ,⊥) => γ,
    (⊤,⊥) => ⊥, (⊤,α) => α, (⊤,β) => β, (⊤,⊤) => ⊤,
)

myalgebra = FLewAlgebra(elements, jointable, meettable, monoidtable)
