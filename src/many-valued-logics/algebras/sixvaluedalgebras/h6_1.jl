# Domain ⊤, ⊥, α, β, γ, δ
t, b, α, β, γ = FiniteTruth.([1:6]...)

# α ∨ β = max{α, β}
jointruthtable = [
#   ⊤  ⊥  α  β  γ  δ
    t, t, t, t, t, t,   # ⊤
    t, b, α, β, γ, δ,   # ⊥
    t, α, α, γ, γ, δ,   # α
    t, β, γ, β, γ, δ,   # β
    t, γ, γ, γ, γ, δ,   # γ
    t, δ, δ, δ, δ, δ    # δ
]

# α ∧ β = min{α, β}
meettruthtable = [
#   ⊤  ⊥  α  β
    t, b, α, β, γ, δ,   # ⊤
    b, b, b, b, b, b,   # ⊥
    α, b, α, b, α, α,   # α
    β, b, b, β, β, β,   # β
    γ, b, α, β, γ, γ,   # γ  
    δ, b, α, β, γ, δ    # δ
]

join = BinaryOperation{6}(jointruthtable)
meet = BinaryOperation{6}(meettruthtable)
# In H6_1, the t-norm ⋅ is ∧

H6_1 = FiniteFLewAlgebra{6}(join, meet, meet, b, t)
