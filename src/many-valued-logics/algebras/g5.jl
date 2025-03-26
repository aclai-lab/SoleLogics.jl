# Domain ⊤, ⊥, α, β, γ
t, b, α, β, γ = FiniteTruth.([1:5]...)

# α ∨ β = max{α, β}
jointruthtable = [
#   ⊤  ⊥  α  β  γ
    t, t, t, t, t,  # ⊤
    t, b, α, β, γ,  # ⊥
    t, α, α, β, γ,  # α
    t, β, β, β, γ,  # β
    t, γ, γ, γ, γ   # γ
]

# α ∧ β = min{α, β}
meettruthtable = [
#   ⊤  ⊥  α  β
    t, b, α, β, γ,  # ⊤
    b, b, b, b, b,  # ⊥
    α, b, α, α, α,  # α
    β, b, α, β, β,  # β
    γ, b, α, β, γ   # γ  
]

join = BinaryOperation{5}(jointruthtable)
meet = BinaryOperation{5}(meettruthtable)
# In G5, the t-norm ⋅ is ∧

G5 = FiniteFLewAlgebra{5}(join, meet, meet, b, t)