using SoleLogics
using SoleLogics.ManyValuedLogics

############################################################################################
#### Boolean algebra #######################################################################
############################################################################################

domain = Set{BooleanTruth}([⊥, ⊤])

jointable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊤,
    (⊤, ⊥) => ⊤, (⊤, ⊤) => ⊤
)

meettable = Dict{Tuple{BooleanTruth, BooleanTruth}, BooleanTruth}(
    (⊥, ⊥) => ⊥, (⊥, ⊤) => ⊥,
    (⊤, ⊥) => ⊥, (⊤, ⊤) => ⊤
)

join = BinaryOperation(domain, jointable)

meet = BinaryOperation(domain, meettable)

monoid = CommutativeMonoid(meet, ⊤)

ffa = FiniteFLewAlgebra(join, meet, monoid, ⊥, ⊤)

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
#### Three-valued algebra ##################################################################
############################################################################################

α = FiniteTruth("α")
d3 = Set{FiniteTruth}([⊥, α, ⊤])

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
