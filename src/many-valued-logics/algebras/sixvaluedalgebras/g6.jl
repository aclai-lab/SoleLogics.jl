# Domain ⊤, ⊥, α, β, γ, δ
t, b, α, β, γ = FiniteTruth.([1:6]...)

# α ∨ β = max{α, β}
jointruthtable = [
#   ⊤  ⊥  α  β  γ  δ
    t, t, t, t, t, t,   # ⊤
    t, b, α, β, γ, δ,   # ⊥
    t, α, α, β, γ, δ,   # α
    t, β, β, β, γ, δ,   # β
    t, γ, γ, γ, γ, δ,   # γ
    t, δ, δ, δ, δ, δ    # δ
]

# α ∧ β = min{α, β}
meettruthtable = [
#   ⊤  ⊥  α  β
    t, b, α, β, γ, δ,   # ⊤
    b, b, b, b, b, b,   # ⊥
    α, b, α, α, α, α,   # α
    β, b, α, β, β, β,   # β
    γ, b, α, β, γ, γ,   # γ  
    δ, b, α, β, γ, δ    # δ
]

join = BinaryOperation{6}(jointruthtable)
meet = BinaryOperation{6}(meettruthtable)
# In G6, the t-norm ⋅ is ∧

G6 = FiniteFLewAlgebra{6}(join, meet, meet, b, t)
