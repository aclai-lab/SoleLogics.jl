using SoleLogics
using Graphs

### Check all basic operations work correctly foc simplest meaningful many-valued algebra ###

@heytingtruths α β
myalgebra = @heytingalgebra (α, β) (⊥, α) (⊥, β) (α, ⊤) (β, ⊤)

@test precedes(myalgebra, ⊥, α)   == true
@test precedes(myalgebra, α, ⊥)   == false
@test precedes(myalgebra, ⊥, β)   == true
@test precedes(myalgebra, β, ⊥)   == false
@test precedes(myalgebra, ⊥, ⊤)   == true
@test precedes(myalgebra, ⊤, ⊥)   == false
@test precedes(myalgebra, α, β)   == false
@test precedes(myalgebra, β, α)   == false
@test precedes(myalgebra, α, ⊤)   == true
@test precedes(myalgebra, ⊤, α)   == false
@test precedes(myalgebra, β, ⊤)   == true
@test precedes(myalgebra, ⊤, β)   == false

@test succeedes(myalgebra, ⊥, α)  == false
@test succeedes(myalgebra, α, ⊥)  == true
@test succeedes(myalgebra, ⊥, β)  == false
@test succeedes(myalgebra, β, ⊥)  == true
@test succeedes(myalgebra, ⊥, ⊤)  == false
@test succeedes(myalgebra, ⊤, ⊥)  == true
@test succeedes(myalgebra, α, β)  == false
@test succeedes(myalgebra, β, α)  == false
@test succeedes(myalgebra, α, ⊤)  == false
@test succeedes(myalgebra, ⊤, α)  == true
@test succeedes(myalgebra, β, ⊤)  == false
@test succeedes(myalgebra, ⊤, β)  == true

@test collatetruth(∧, (⊥, α), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (α, ⊥), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (⊥, β), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (β, ⊥), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (⊥, ⊤), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (⊤, ⊥), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (α, β), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (β, α), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (α, ⊤), myalgebra)  == α
@test collatetruth(∧, (⊤, α), myalgebra)  == α
@test collatetruth(∧, (β, ⊤), myalgebra)  == β
@test collatetruth(∧, (⊤, β), myalgebra)  == β

@test collatetruth(∨, (⊥, ⊥), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(∨, (⊥, α), myalgebra)  == α
@test collatetruth(∨, (α, ⊥), myalgebra)  == α
@test collatetruth(∨, (⊥, β), myalgebra)  == β
@test collatetruth(∨, (β, ⊥), myalgebra)  == β
@test collatetruth(∨, (⊥, ⊤), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (⊤, ⊥), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (α, β), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (β, α), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (α, ⊤), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (⊤, α), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (β, ⊤), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (⊤, β), myalgebra)  == convert(HeytingTruth, ⊤)

@test collatetruth(→, (⊥, α), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (α, ⊥), myalgebra)  == β
@test collatetruth(→, (⊥, β), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (β, ⊥), myalgebra)  == α
@test collatetruth(→, (⊥, ⊤), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (⊤, ⊥), myalgebra)  == convert(HeytingTruth, ⊥)
@test collatetruth(→, (α, β), myalgebra)  == β
@test collatetruth(→, (β, α), myalgebra)  == α
@test collatetruth(→, (α, ⊤), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (⊤, α), myalgebra)  == α
@test collatetruth(→, (β, ⊤), myalgebra)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (⊤, β), myalgebra)  == β

### Testing if check works on random propositional formulas and it gives the same result ###

using Random

@heytingalgebra booleanalgebra () (⊥, ⊤)

myalphabet = @atoms a b c
td1 = TruthDict([a => ⊥, b => ⊥, c => ⊥])
td2 = TruthDict([a => ⊥, b => ⊥, c => ⊤])
td3 = TruthDict([a => ⊥, b => ⊤, c => ⊥])
td4 = TruthDict([a => ⊤, b => ⊥, c => ⊥])
td5 = TruthDict([a => ⊥, b => ⊤, c => ⊤])
td6 = TruthDict([a => ⊤, b => ⊥, c => ⊤])
td7 = TruthDict([a => ⊤, b => ⊤, c => ⊥])
td8 = TruthDict([a => ⊤, b => ⊤, c => ⊤])

for i ∈ range(20, 100)
    height = div(i, 10)
    rf = randformula(Random.MersenneTwister(), height, myalphabet, SoleLogics.BASE_PROPOSITIONAL_CONNECTIVES)
    @test check(rf, td1) == check(rf, td1, booleanalgebra)
    @test check(rf, td2) == check(rf, td2, booleanalgebra)
    @test check(rf, td3) == check(rf, td3, booleanalgebra)
    @test check(rf, td4) == check(rf, td4, booleanalgebra)
    @test check(rf, td5) == check(rf, td5, booleanalgebra)
    @test check(rf, td6) == check(rf, td6, booleanalgebra)
    @test check(rf, td7) == check(rf, td7, booleanalgebra)
    @test check(rf, td8) == check(rf, td8, booleanalgebra)
end
