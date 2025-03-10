# Domain ⊤, ⊥
t, b = FiniteTruth.([1:2]...)

# α ∨ β = max{α, β}
jointruthtable = [
#   ⊤  ⊥
    t, t,   # ⊤
    t, b    # ⊥
]

# α ∧ β = min{α, β}
meettruthtable = [
#   ⊤  ⊥
    t, b,   # ⊤
    b, b    # ⊥
]

join = BinaryOperation{2}(jointruthtable)
meet = BinaryOperation{2}(meettruthtable)
# In booleanalgebra, the t-norm ⋅ is ∧

booleanalgebra = FiniteFLewAlgebra{2}(join, meet, meet, b, t)
