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

x = ContinuousTruth(0)
@test isbot(x)


@test_throws ErrorException f = ContinuousBinaryOperation(string)
@test_throws ErrorException f = ContinuousBinaryOperation(show)

godel_meet = ContinuousBinaryOperation(min)

@test arity(godel_meet) == 2

@test iszero(godel_meet(l, x).value)

@test iszero(godel_meet(x, ⊤).value)

@test iszero(godel_meet(⊤, x).value)

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

x = ContinuousTruth(1.0)
y = ContinuousTruth(0.0)

@test precedeq(GodelLogic, ⊥, ⊤)
@test precedes(GodelLogic, ⊥, ⊤)

@test succeedeq(GodelLogic, ⊤, ⊥)
@test succeedes(GodelLogic, ⊤, ⊥)

FuzzyLogic(GodelTNorm)

@test iszero(GodelLogic.tnorm(x, y).value)

@test iszero(LukasiewiczLogic.tnorm(x, y).value)

@test iszero(ProductLogic.tnorm(x, y).value)

@test iscrisp(GodelLogic) == false

@test top(GodelLogic) == ContinuousTruth(1.0)

@test bot(GodelLogic) == ContinuousTruth(0.0)

################################################################################
#### Many-Expert Algebra #######################################################
################################################################################

MXA = ManyExpertAlgebra{3}([GodelLogic, LukasiewiczLogic, ProductLogic])

@test iscrisp(MXA) == false

@test top(MXA) == ntuple(i -> top(MXA.experts[i]), 3)
@test bot(MXA) == ntuple(i -> bot(MXA.experts[i]), 3)


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
#### Fuzzy simplify and collatetruth ###########################################
################################################################################

@test SoleLogics.collatetruth(→, (ContinuousTruth(0.5), ⊤), GodelLogic) == top(GodelLogic)
@test SoleLogics.collatetruth(→, (ContinuousTruth(0.5), ⊤), MXA) == top(MXA)
@test SoleLogics.collatetruth(→, (⊥, ContinuousTruth(0.5)), GodelLogic) == top(GodelLogic)
@test SoleLogics.collatetruth(→, (⊥, ContinuousTruth(0.5)), MXA) == top(MXA)
@test SoleLogics.collatetruth(→, (ContinuousTruth(0.5), top(MXA)), MXA) == top(MXA)
@test SoleLogics.collatetruth(→, (bot(MXA), ContinuousTruth(1.0)), MXA) == top(MXA)
@test_throws ErrorException SoleLogics.collatetruth(→, (ContinuousTruth(0.5), ContinuousTruth(1.0)), FuzzyLogic(ContinuousBinaryOperation(+)))

@test SoleLogics.simplify(∧, (parseformula("p∧q"), ⊥), MXA) == bot(MXA)
@test SoleLogics.simplify(∧, (bot(MXA), ⊥), MXA) == bot(MXA)
@test SoleLogics.simplify(∧, (⊥, bot(MXA)), MXA) == bot(MXA)

@test SoleLogics.simplify(∨, (⊤, parseformula("p∧q")), MXA) == top(MXA)
@test SoleLogics.simplify(∨, (bot(MXA), ⊤), MXA) == top(MXA)
@test SoleLogics.simplify(∨, (⊤, bot(MXA)), MXA) == top(MXA)
@test SoleLogics.simplify(∨, (ContinuousTruth(1.0), bot(MXA)), MXA) == top(MXA)
@test SoleLogics.simplify(∨, (bot(MXA), ContinuousTruth(1.0)), MXA) == top(MXA)

@test SoleLogics.simplify(→, (bot(MXA), ⊤), MXA) == top(MXA)
@test SoleLogics.simplify(→, (⊥, top(MXA)), MXA) == top(MXA)
@test SoleLogics.simplify(→, (ContinuousTruth(0.0), top(MXA)), MXA) == top(MXA)
@test SoleLogics.simplify(→, (bot(MXA), ContinuousTruth(1.0)), MXA) == top(MXA)

################################################################################
#### Fuzzy-Logics interpret ####################################################
################################################################################

v = Atom("v")
w = Atom("w")
x = Atom("x")
y = Atom("y")
z = Atom("z")

@test check(parseformula("v∨w"), TruthDict([v => ⊥, w => ContinuousTruth(1)]), GodelLogic)
@test isbot(interpret(parseformula("(v∧w)∨v"), TruthDict([v => ContinuousTruth(0), w => ⊤]), GodelLogic))
@test istop(interpret(parseformula("v→w"), TruthDict([v => ContinuousTruth(0.5), w => ContinuousTruth(0.5)]), LukasiewiczLogic))
@test interpret(parseformula("(v∧w)→x"), TruthDict([v => ContinuousTruth(0.8), w => ContinuousTruth(0.6), x => ContinuousTruth(0.5)]), GodelLogic) == ContinuousTruth(0.5)
@test interpret(parseformula("(v∨w)∧x"), TruthDict([v => ContinuousTruth(0.3), w => ContinuousTruth(0.5), x => ContinuousTruth(0.4)]), LukasiewiczLogic) == ContinuousTruth(0.0)
@test istop(interpret(parseformula("v→(w→v)"), TruthDict([v => ContinuousTruth(0.7), w => ContinuousTruth(0.3)]), GodelLogic))

################################################################################
#### Many-Expert interpret #####################################################
################################################################################

@test interpret(parseformula("v∧w"), TruthDict([v => ⊤, w => ⊤]), MXA) == top(MXA)
@test interpret(parseformula("v∨w"), TruthDict([v => ⊥, w => ⊥]), MXA) == bot(MXA)
@test interpret(parseformula("v∧w"), TruthDict([v => ⊥, w => ⊤]), MXA) == bot(MXA)
@test interpret(parseformula("v∨w"), TruthDict([v => ⊤, w => ⊥]), MXA) == top(MXA)
@test top(MXA) == interpret(parseformula("v∨w"), TruthDict([v => ContinuousTruth(0), w => ⊤]), MXA)
@test bot(MXA) == interpret(parseformula("(v∧w∧x)∨(y∧z)"), TruthDict([v => ⊥, w => ContinuousTruth(0.5), x => ContinuousTruth(0.0), y => ⊥, z => ContinuousTruth(0.4)]), MXA)
@test top(MXA) == interpret(parseformula("v→w"), TruthDict([v => ⊥, w => ⊤]), MXA)
@test interpret(parseformula("(v→w)∧(w→v)"), TruthDict([v => ContinuousTruth(0.3), w => ContinuousTruth(0.6)]), MXA) == (ContinuousTruth(0.3), ContinuousTruth(0.7), ContinuousTruth(0.5))
@test interpret(parseformula("((v→w)∨(w→x))∧(v∨x)"), TruthDict([v => ContinuousTruth(0.7), w => ContinuousTruth(0.2), x => ContinuousTruth(0.5)]), MXA) == (ContinuousTruth(0.7), ContinuousTruth(0.7), ContinuousTruth(0.7))
@test interpret(parseformula("(v∧(w∨x))→(y∨z)"), TruthDict([v => ContinuousTruth(0.6), w => ContinuousTruth(0.4), x => ContinuousTruth(0.9), y => ContinuousTruth(0.3), z => ContinuousTruth(0.8)]), MXA) == (ContinuousTruth(1.0), ContinuousTruth(1.0), ContinuousTruth(1.0))

################################################################################
#### Finite FLew-chains generation #############################################
################################################################################

@test length(generateflewchains(9)) == 13775
