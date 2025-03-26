# Domain ⊤, ⊥, α
t, b, α = FiniteTruth.([1:3]...)

# α ∨ β = max{α, β}
jointruthtable = [
#   ⊤  ⊥  α
    t, t, t,    # ⊤
    t, b, α,    # ⊥
    t, α, α     # α
]

# α ∧ β = min{α, β}
meettruthtable = [
#   ⊤  ⊥  α
    t, b, α,    # ⊤
    b, b, b,    # ⊥
    α, b, α     # α
]

join = BinaryOperation{3}(jointruthtable)
meet = BinaryOperation{3}(meettruthtable)
# In G3, the t-norm ⋅ is ∧

G3 = FiniteFLewAlgebra{3}(join, meet, meet, b, t)
