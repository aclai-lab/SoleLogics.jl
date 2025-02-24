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

# α ⋅Ł β = max{0, α + β - 1}
lnormtruthtable = [
#   ⊤  ⊥  α  β
    t, b, α, β, # ⊤
    b, b, b, b, # ⊥
    α, b, b, b, # α
    β, b, b, α  # β
]

join = BinaryOperation{4}(jointruthtable)
meet = BinaryOperation{4}(meettruthtable)
lnorm = BinaryOperation{4}(lnormtruthtable)

Ł4 = FiniteFLewAlgebra{4}(join, meet, lnorm, b, t)
