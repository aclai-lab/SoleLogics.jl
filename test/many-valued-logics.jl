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

domain = Set{BooleanTruth}([⊥, ⊤])
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

α = FiniteTruth("α")
@test string(α) == "α"

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
