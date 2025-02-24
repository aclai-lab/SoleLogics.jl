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

# α ⋅Ł β = max{0, α + β - 1}
lnormtruthtable = [
#   ⊤  ⊥  α  β
    t, b, α,    # ⊤
    b, b, b,    # ⊥
    α, b, b    # α
]

join = BinaryOperation{3}(jointruthtable)
meet = BinaryOperation{3}(meettruthtable)
lnorm = BinaryOperation{3}(lnormtruthtable)

Ł3 = FiniteFLewAlgebra{3}(join, meet, lnorm, b, t)
