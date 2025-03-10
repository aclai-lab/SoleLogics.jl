# Domain ⊤, ⊥, α, β
t, b, α, β = FiniteTruth.([1:4]...)

# α ∨ β = max{α, β}
jointruthtable = [
#   ⊤  ⊥  α  β
    t, t, t, t, # ⊤
    t, b, α, β, # ⊥
    t, α, α, β, # α
    t, β, β, β  # β
]

# α ∧ β = min{α, β}
meettruthtable = [
#   ⊤  ⊥  α  β
    t, b, α, β, # ⊤
    b, b, b, b, # ⊥
    α, b, α, α, # α
    β, b, α, β  # β
]

join = BinaryOperation{4}(jointruthtable)
meet = BinaryOperation{4}(meettruthtable)
# In G4, the t-norm ⋅ is ∧

G4 = FiniteFLewAlgebra{4}(join, meet, meet, b, t)
