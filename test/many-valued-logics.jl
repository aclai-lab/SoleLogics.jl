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

domain = Vector{BooleanTruth}([⊥, ⊤])
jointable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, ⊤) => ⊤
)
join = BinaryOperation(domain, jointable)

@test string(join) == string(jointable)
@test arity(join) == 2

############################################################################################
#### Finite algebras (boolean case) ########################################################
############################################################################################

β = FiniteTruth("β")
@test string(β) == "β"

meettable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊥,
    (⊤, ⊥) => ⊥, (⊤, ⊤) => ⊤
)
meet = BinaryOperation(domain, meettable)
fa = FiniteLattice(join, meet)
fba = FiniteBoundedLattice(join, meet, ⊥, ⊤)

meetmonoid = CommutativeMonoid(meet, ⊤)
frl = FiniteResiduatedLattice(join, meet, meetmonoid, ⊥, ⊤)

ffa = FiniteFLewAlgebra(join, meet, meetmonoid, ⊥, ⊤)
@test ffa.implication(⊥, ⊥) == ⊤
@test ffa.implication(⊥, ⊤) == ⊤
@test ffa.implication(⊤, ⊥) == ⊥
@test ffa.implication(⊤, ⊤) == ⊤

implicationtable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊤,
    (⊥, ⊤) => ⊤,
    (⊤, ⊥) => ⊥,
    (⊤, ⊤) => ⊤
)
implication = BinaryOperation(domain, implicationtable)
fha = FiniteHeytingAlgebra(join, meet, implication, ⊥, ⊤)

############################################################################################
#### Three-valued algebra (Łukasiewich norm case) ##########################################
############################################################################################

d3 = Vector{FiniteTruth}([⊥, α, ⊤])
jt3 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, ⊤) => ⊤,
    (α, ⊥) => α, (α, α) => α, (α, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, ⊤) => ⊤
)
j3 = BinaryOperation(d3, jt3)
mt3 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => α, (α, ⊤) => α,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, ⊤) => ⊤
)
m3 = BinaryOperation(d3, mt3)
lot3 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => ⊥, (α, ⊤) => α,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, ⊤) => ⊤
)
lo3 = BinaryOperation(d3, lot3)
l3 = CommutativeMonoid(lo3, ⊤)
ffa3 = FiniteFLewAlgebra(j3, m3, l3, ⊥, ⊤)

############################################################################################
#### Nine-valued algebra (Heyting case) ####################################################
############################################################################################

β = FiniteTruth("β")
γ = FiniteTruth("γ")
δ = FiniteTruth("δ")
ε = FiniteTruth("ε")
ζ = FiniteTruth("ζ")
η = FiniteTruth("η")

d9 = Vector{FiniteTruth}([⊥, α, β, γ, δ, ε, ζ, η, ⊤])

jt9 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => α, (⊥, β) => β, (⊥, γ) => γ, (⊥, δ) => δ, (⊥, ε) => ε, (⊥, ζ) => ζ, (⊥, η) => η, (⊥, ⊤) => ⊤,
    (α, ⊥) => α, (α, α) => α, (α, β) => δ, (α, γ) => γ, (α, δ) => δ, (α, ε) => η, (α, ζ) => ζ, (α, η) => η, (α, ⊤) => ⊤,
    (β, ⊥) => β, (β, α) => δ, (β, β) => β, (β, γ) => ζ, (β, δ) => δ, (β, ε) => ε, (β, ζ) => ζ, (β, η) => η, (β, ⊤) => ⊤,
    (γ, ⊥) => γ, (γ, α) => γ, (γ, β) => ζ, (γ, γ) => γ, (γ, δ) => ζ, (γ, ε) => ⊤, (γ, ζ) => ζ, (γ, η) => ⊤, (γ, ⊤) => ⊤,
    (δ, ⊥) => δ, (δ, α) => δ, (δ, β) => δ, (δ, γ) => ζ, (δ, δ) => δ, (δ, ε) => η, (δ, ζ) => ζ, (δ, η) => η, (δ, ⊤) => ⊤,
    (ε, ⊥) => ε, (ε, α) => η, (ε, β) => ε, (ε, γ) => ⊤, (ε, δ) => η, (ε, ε) => ε, (ε, ζ) => ⊤, (ε, η) => η, (ε, ⊤) => ⊤,
    (ζ, ⊥) => ζ, (ζ, α) => ζ, (ζ, β) => ζ, (ζ, γ) => ζ, (ζ, δ) => ζ, (ζ, ε) => ⊤, (ζ, ζ) => ζ, (ζ, η) => ⊤, (ζ, ⊤) => ⊤,
    (η, ⊥) => η, (η, α) => η, (η, β) => η, (η, γ) => ⊤, (η, δ) => η, (η, ε) => η, (η, ζ) => ⊤, (η, η) => η, (η, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, α) => ⊤, (⊤, β) => ⊤, (⊤, γ) => ⊤, (⊤, δ) => ⊤, (⊤, ε) => ⊤, (⊤, ζ) => ⊤, (⊤, η) => ⊤, (⊤, ⊤) => ⊤
)

mt9 = Dict{Tuple{FiniteTruth, FiniteTruth}, FiniteTruth}(
    (⊥, ⊥) => ⊥, (⊥, α) => ⊥, (⊥, β) => ⊥, (⊥, γ) => ⊥, (⊥, δ) => ⊥, (⊥, ε) => ⊥, (⊥, ζ) => ⊥, (⊥, η) => ⊥, (⊥, ⊤) => ⊥,
    (α, ⊥) => ⊥, (α, α) => α, (α, β) => ⊥, (α, γ) => α, (α, δ) => α, (α, ε) => ⊥, (α, ζ) => α, (α, η) => α, (α, ⊤) => α,
    (β, ⊥) => ⊥, (β, α) => ⊥, (β, β) => β, (β, γ) => ⊥, (β, δ) => β, (β, ε) => β, (β, ζ) => β, (β, η) => β, (β, ⊤) => β,
    (γ, ⊥) => ⊥, (γ, α) => α, (γ, β) => ⊥, (γ, γ) => γ, (γ, δ) => α, (γ, ε) => ⊥, (γ, ζ) => γ, (γ, η) => α, (γ, ⊤) => γ,
    (δ, ⊥) => ⊥, (δ, α) => α, (δ, β) => β, (δ, γ) => α, (δ, δ) => δ, (δ, ε) => β, (δ, ζ) => δ, (δ, η) => δ, (δ, ⊤) => δ,
    (ε, ⊥) => ⊥, (ε, α) => ⊥, (ε, β) => β, (ε, γ) => ⊥, (ε, δ) => β, (ε, ε) => ε, (ε, ζ) => β, (ε, η) => ε, (ε, ⊤) => ε,
    (ζ, ⊥) => ⊥, (ζ, α) => α, (ζ, β) => β, (ζ, γ) => γ, (ζ, δ) => δ, (ζ, ε) => β, (ζ, ζ) => ζ, (ζ, η) => δ, (ζ, ⊤) => ζ,
    (η, ⊥) => ⊥, (η, α) => α, (η, β) => β, (η, γ) => α, (η, δ) => δ, (η, ε) => ε, (η, ζ) => δ, (η, η) => η, (η, ⊤) => η,
    (⊤, ⊥) => ⊥, (⊤, α) => α, (⊤, β) => β, (⊤, γ) => γ, (⊤, δ) => δ, (⊤, ε) => ε, (⊤, ζ) => ζ, (⊤, η) => η, (⊤, ⊤) => ⊤
)

j9 = BinaryOperation(d9, jt9)

m9 = BinaryOperation(d9, mt9)

mm9 = CommutativeMonoid(m9, ⊤)

ffa9 = FiniteFLewAlgebra(j9, m9, mm9, ⊥, ⊤)

fha9 = FiniteHeytingAlgebra(ffa9)
