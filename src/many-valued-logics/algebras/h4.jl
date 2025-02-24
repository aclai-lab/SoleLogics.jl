# Domain ⊤, ⊥, α, β
t, b, α, β = FiniteTruth.([1:4]...)

# α ∨ β = max{α, β}
jointruthtable = [
#   ⊤  ⊥  α  β
    t, t, t, t, # ⊤
    t, b, α, β, # ⊥
    t, α, α, t, # α
    t, β, t, β  # β
]

# α ∧ β = min{α, β}
meettruthtable = [
#   ⊤  ⊥  α  β
    t, b, α, β, # ⊤
    b, b, b, b, # ⊥
    α, b, α, b, # α
    β, b, b, β  # β
]

join = BinaryOperation{4}(jointruthtable)
meet = BinaryOperation{4}(meettruthtable)
# In H4, the t-norm ⋅ is ∧

H4 = FiniteFLewAlgebra{4}(join, meet, meet, b, t)
