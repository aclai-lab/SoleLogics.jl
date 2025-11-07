using Test
using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: Operation

################################################################################
#### Operations ################################################################
################################################################################

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

l = ContinuousTruth(1)
@test istop(l)

r = ContinuousTruth(0)
@test isbot(r)


@test_throws ErrorException f = ContinuousBinaryOperation(string)
@test_throws ErrorException f = ContinuousBinaryOperation(show)

godel_meet = ContinuousBinaryOperation(min)

@test arity(godel_meet) == 2

@test iszero(godel_meet(l, r).value)

@test iszero(godel_meet(r, ⊤).value)

@test iszero(godel_meet(⊤, r).value)

@test iszero(godel_meet(⊤, ⊥).value)


################################################################################
#### Finite truth ##############################################################
################################################################################

α = FiniteTruth(3)
@test string(α) == "α"

################################################################################
#### Finite algebras (boolean case) ############################################
################################################################################

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
@test implication(⊥, ⊥) == convert(FiniteTruth, ⊤)
@test implication(⊥, ⊤) == convert(FiniteTruth, ⊤)
@test implication(⊤, ⊥) == convert(FiniteTruth, ⊥)
@test implication(⊤, ⊤) == convert(FiniteTruth, ⊤)
fha = FiniteHeytingAlgebra{2}(join, meet, implication, b, t)

################################################################################
#### Three-valued algebra (Łukasiewich norm case) ##############################
################################################################################

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

################################################################################
#### Fuzzy Logics ##############################################################
################################################################################

@test FuzzyLogic(GodelTNorm)

@test iszero(GodelLogic.tnorm(x, y).value)

@test iszero(LukasiewiczLogic.tnorm(x, y).value)

@test iszero(ProductLogic.tnorm(x, y).value)

@test iscrisp(GodelLogic) == false

@test top(GodelLogic) == ContinuousTruth(1.0)

@test bot(GodelLogic) == ContinuousTruth(0.0)

################################################################################
#### Many-Expert Algebra #######################################################
################################################################################

MXA = ManyExpertAlgebra{3}([GodelLogic, ProductLogic, LukasiewiczLogic])

@test iscrisp(MXA) == false

@test top(MXA) == (1.0, 1.0, 1.0)
@test bot(MXA) == (0.0, 0.0, 0.0)

################################################################################
#### Nine-valued algebra (Heyting case) ########################################
################################################################################

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

################################################################################
#### Many-valued check #########################################################
################################################################################

using SoleLogics.ManyValuedLogics: G3

@atoms p q
@test_nowarn check(parseformula("p∧q→q"), TruthDict([p => α, q => ⊥]), G3)
@test_nowarn check(
    parseformula("p∧q→q"),
    TruthDict([p => convert(FiniteTruth, α), q => ⊥]),
    convert(FiniteFLewAlgebra, G3)
)

################################################################################
#### Finite FLew-chains generation #############################################
################################################################################

@test length(generateflewchains(9)) == 13775
