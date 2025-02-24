using Test
using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: Operation

############################################################################################
#### Operations ############################################################################
############################################################################################

struct MyOperation <: Operation end

@test_logs (:warn,) string(MyOperation())
function Base.show(io::IO, o::MyOperation)
    print(io, "MyOperation")
end
@test_throws ErrorException arity(MyOperation())

t, b = FiniteTruth.([1:2]...)
jointruthtable = [
#   ⊤  ⊥
    t, t,   # ⊤
    t, b    # ⊥
]
join = BinaryOperation{2}(jointruthtable)

@test arity(join) == 2

############################################################################################
#### Finite truth ##########################################################################
############################################################################################

α = FiniteTruth(3)
@test string(α) == "α"

############################################################################################
#### Finite algebras (boolean case) ########################################################
############################################################################################

meettruthtable = [
#   ⊤  ⊥
    t, b,   # ⊤
    b, b    # ⊥
]
meet = BinaryOperation{2}(meettruthtable)
fl = FiniteLattice{2}(join, meet)
fbl = FiniteBoundedLattice{2}(join, meet, b, t)
tnorm = CommutativeMonoid{2}(meet, ⊤)
frl = FiniteResiduatedLattice{2}(join, meet, tnorm, b, t)

ffa = FiniteFLewAlgebra{2}(join, meet, tnorm, b, t)
@test ffa.implication(⊥, ⊥) == convert(FiniteTruth, ⊤)
@test ffa.implication(⊥, ⊤) == convert(FiniteTruth, ⊤)
@test ffa.implication(⊤, ⊥) == convert(FiniteTruth, ⊥)
@test ffa.implication(⊤, ⊤) == convert(FiniteTruth, ⊤)

# Defining truthtable as a Vector, SMatrix is created by column
# E.g.: [1 2 3 4 5 6 7 8 9] becomes
#   1 4 7
#   2 5 8
#   3 6 9
implicationtruthtable = [
#   ⊤  ⊥
    t, t,   # ⊤
    b, t    # ⊥
]
implication = BinaryOperation{2}(implicationtruthtable)
println(implication)
@test implication(⊥, ⊥) == convert(FiniteTruth, ⊤)
@test implication(⊥, ⊤) == convert(FiniteTruth, ⊤)
@test implication(⊤, ⊥) == convert(FiniteTruth, ⊥)
@test implication(⊤, ⊤) == convert(FiniteTruth, ⊤)
fha = FiniteHeytingAlgebra{2}(join, meet, implication, b, t)

# ############################################################################################
# #### Three-valued algebra (Łukasiewich norm case) ##########################################
# ############################################################################################

# d3 = Vector{FiniteTruth}([⊥, α, ⊤])
# jt3 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
#     (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, ⊤) => ⊤,
#     (α, ⊥) => α, (α, α) => α, (α, ⊤) => ⊤,
#     (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, ⊤) => ⊤
# )
# j3 = BinaryOperation(d3, jt3)
# mt3 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
#     (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, ⊤) => ⊥,
#     (α, ⊥) => ⊥, (α, α) => α, (α, ⊤) => α,
#     (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, ⊤) => ⊤
# )
# m3 = BinaryOperation(d3, mt3)
# lot3 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
#     (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, ⊤) => ⊥,
#     (α, ⊥) => ⊥, (α, α) => ⊥, (α, ⊤) => α,
#     (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, ⊤) => ⊤
# )
# lo3 = BinaryOperation(d3, lot3)
# l3 = CommutativeMonoid(lo3, ⊤)
# ffa3 = FiniteFLewAlgebra(j3, m3, l3, ⊥, ⊤)

# ############################################################################################
# #### Nine-valued algebra (Heyting case) ####################################################
# ############################################################################################

# β = FiniteTruth("β")
# γ = FiniteTruth("γ")
# δ = FiniteTruth("δ")
# ε = FiniteTruth("ε")
# ζ = FiniteTruth("ζ")
# η = FiniteTruth("η")

# d9 = Vector{FiniteTruth}([⊥, α, β, γ, δ, ε, ζ, η, ⊤])

# jt9 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
#     (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, β) => β, (⊥, γ) => γ, (⊥, δ) => δ, (⊥, ε) => ε, (⊥, ζ) => ζ, (⊥, η) => η, (⊥, ⊤) => ⊤,
#     (α, ⊥) => α, (α, α) => α, (α, β) => δ, (α, γ) => γ, (α, δ) => δ, (α, ε) => η, (α, ζ) => ζ, (α, η) => η, (α, ⊤) => ⊤,
#     (β, ⊥) => β, (β, α) => δ, (β, β) => β, (β, γ) => ζ, (β, δ) => δ, (β, ε) => ε, (β, ζ) => ζ, (β, η) => η, (β, ⊤) => ⊤,
#     (γ, ⊥) => γ, (γ, α) => γ, (γ, β) => ζ, (γ, γ) => γ, (γ, δ) => ζ, (γ, ε) => ⊤, (γ, ζ) => ζ, (γ, η) => ⊤, (γ, ⊤) => ⊤,
#     (δ, ⊥) => δ, (δ, α) => δ, (δ, β) => δ, (δ, γ) => ζ, (δ, δ) => δ, (δ, ε) => η, (δ, ζ) => ζ, (δ, η) => η, (δ, ⊤) => ⊤,
#     (ε, ⊥) => ε, (ε, α) => η, (ε, β) => ε, (ε, γ) => ⊤, (ε, δ) => η, (ε, ε) => ε, (ε, ζ) => ⊤, (ε, η) => η, (ε, ⊤) => ⊤,
#     (ζ, ⊥) => ζ, (ζ, α) => ζ, (ζ, β) => ζ, (ζ, γ) => ζ, (ζ, δ) => ζ, (ζ, ε) => ⊤, (ζ, ζ) => ζ, (ζ, η) => ⊤, (ζ, ⊤) => ⊤,
#     (η, ⊥) => η, (η, α) => η, (η, β) => η, (η, γ) => ⊤, (η, δ) => η, (η, ε) => η, (η, ζ) => ⊤, (η, η) => η, (η, ⊤) => ⊤,
#     (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, β) => ⊤, (⊤, γ) => ⊤, (⊤, δ) => ⊤, (⊤, ε) => ⊤, (⊤, ζ) => ⊤, (⊤, η) => ⊤, (⊤, ⊤) => ⊤
# )

# mt9 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
#     (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, β) => ⊥, (⊥, γ) => ⊥, (⊥, δ) => ⊥, (⊥, ε) => ⊥, (⊥, ζ) => ⊥, (⊥, η) => ⊥, (⊥, ⊤) => ⊥,
#     (α, ⊥) => ⊥, (α, α) => α, (α, β) => ⊥, (α, γ) => α, (α, δ) => α, (α, ε) => ⊥, (α, ζ) => α, (α, η) => α, (α, ⊤) => α,
#     (β, ⊥) => ⊥, (β, α) => ⊥, (β, β) => β, (β, γ) => ⊥, (β, δ) => β, (β, ε) => β, (β, ζ) => β, (β, η) => β, (β, ⊤) => β,
#     (γ, ⊥) => ⊥, (γ, α) => α, (γ, β) => ⊥, (γ, γ) => γ, (γ, δ) => α, (γ, ε) => ⊥, (γ, ζ) => γ, (γ, η) => α, (γ, ⊤) => γ,
#     (δ, ⊥) => ⊥, (δ, α) => α, (δ, β) => β, (δ, γ) => α, (δ, δ) => δ, (δ, ε) => β, (δ, ζ) => δ, (δ, η) => δ, (δ, ⊤) => δ,
#     (ε, ⊥) => ⊥, (ε, α) => ⊥, (ε, β) => β, (ε, γ) => ⊥, (ε, δ) => β, (ε, ε) => ε, (ε, ζ) => β, (ε, η) => ε, (ε, ⊤) => ε,
#     (ζ, ⊥) => ⊥, (ζ, α) => α, (ζ, β) => β, (ζ, γ) => γ, (ζ, δ) => δ, (ζ, ε) => β, (ζ, ζ) => ζ, (ζ, η) => δ, (ζ, ⊤) => ζ,
#     (η, ⊥) => ⊥, (η, α) => α, (η, β) => β, (η, γ) => α, (η, δ) => δ, (η, ε) => ε, (η, ζ) => δ, (η, η) => η, (η, ⊤) => η,
#     (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, β) => β, (⊤, γ) => γ, (⊤, δ) => δ, (⊤, ε) => ε, (⊤, ζ) => ζ, (⊤, η) => η, (⊤, ⊤) => ⊤
# )

# j9 = BinaryOperation(d9, jt9)

# m9 = BinaryOperation(d9, mt9)

# mm9 = CommutativeMonoid(m9, ⊤)

# ffa9 = FiniteFLewAlgebra(j9, m9, mm9, ⊥, ⊤)

# fha9 = FiniteHeytingAlgebra(ffa9)

############################################################################################
#### Many-valued check #####################################################################
############################################################################################

using SoleLogics.ManyValuedLogics: G3

@atoms p q
@test_nowarn check(parseformula("p∧q→q"), TruthDict([p => α, q => ⊥]), G3)
@test_nowarn check(parseformula("p∧q→q"), TruthDict([p => convert(FiniteTruth, α), q => ⊥]), convert(FiniteFLewAlgebra, G3))
