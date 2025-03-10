# Domain ⊤, ⊥, α, β, γ, δ, ε, ζ, η
t, b, α, β, γ, δ, ε, ζ, η = FiniteTruth.([1:9]...)

# α ∨ β = max{α, β}
jointruthtable = [
#   ⊤  ⊥  α  β  γ  δ  ε  ζ  η
    t, t, t, t, t, t, t, t, t,  # ⊤
    t, b, α, β, γ, δ, ε, ζ, η,  # ⊥
    t, α, α, δ, γ, δ, η, ζ, η,  # α
    t, β, δ, β, ζ, δ, ε, ζ, η,  # β
    t, γ, γ, ζ, γ, ζ, t, ζ, t,  # γ
    t, δ, δ, δ, ζ, δ, η, ζ, η,  # δ
    t, ε, η, ε, t, η, ε, t, η,  # ε
    t, ζ, ζ, ζ, ζ, ζ, t, ζ, t,  # ζ
    t, η, η, η, t, η, η, t, η   # η
]

# α ∧ β = min{α, β}
meettruthtable = [
#   ⊤  ⊥  α  β  γ  δ  ε  ζ  η
    t, b, α, β, γ, δ, ε, ζ, η, # ⊤
    b, b, b, b, b, b, b, b, b, # ⊥
    α, b, α, b, α, α, b, α, α, # α
    β, b, b, β, b, β, β, β, β, # β
    γ, b, α, b, γ, α, b, γ, α, # γ
    δ, b, α, β, α, δ, β, δ, δ, # δ
    ε, b, b, β, b, β, ε, β, ε, # ε
    ζ, b, α, β, γ, δ, β, ζ, δ, # ζ
    η, b, α, β, α, δ, ε, δ, η  # η
]

join = BinaryOperation{9}(jointruthtable)
meet = BinaryOperation{9}(meettruthtable)
# In H9, the t-norm ⋅ is ∧

H9 = FiniteFLewAlgebra{9}(join, meet, meet, b, t)
