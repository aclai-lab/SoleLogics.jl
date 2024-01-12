using SoleLogics
using Graphs

### Check all basic operations work correctly foc simplest meaningful many-valued algebra ###

@heytingtruths α β
@heytingalgebra heytingalgebra4 (α, β) (⊥, α) (⊥, β) (α, ⊤) (β, ⊤)

@test precedes(heytingalgebra4, ⊥, α)   == true
@test precedes(heytingalgebra4, α, ⊥)   == false
@test precedes(heytingalgebra4, ⊥, β)   == true
@test precedes(heytingalgebra4, β, ⊥)   == false
@test precedes(heytingalgebra4, ⊥, ⊤)   == true
@test precedes(heytingalgebra4, ⊤, ⊥)   == false
@test precedes(heytingalgebra4, α, β)   == false
@test precedes(heytingalgebra4, β, α)   == false
@test precedes(heytingalgebra4, α, ⊤)   == true
@test precedes(heytingalgebra4, ⊤, α)   == false
@test precedes(heytingalgebra4, β, ⊤)   == true
@test precedes(heytingalgebra4, ⊤, β)   == false

@test succeedes(heytingalgebra4, ⊥, α)  == false
@test succeedes(heytingalgebra4, α, ⊥)  == true
@test succeedes(heytingalgebra4, ⊥, β)  == false
@test succeedes(heytingalgebra4, β, ⊥)  == true
@test succeedes(heytingalgebra4, ⊥, ⊤)  == false
@test succeedes(heytingalgebra4, ⊤, ⊥)  == true
@test succeedes(heytingalgebra4, α, β)  == false
@test succeedes(heytingalgebra4, β, α)  == false
@test succeedes(heytingalgebra4, α, ⊤)  == false
@test succeedes(heytingalgebra4, ⊤, α)  == true
@test succeedes(heytingalgebra4, β, ⊤)  == false
@test succeedes(heytingalgebra4, ⊤, β)  == true

@test collatetruth(∧, (⊥, α), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (α, ⊥), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (⊥, β), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (β, ⊥), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (⊥, ⊤), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (⊤, ⊥), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (α, β), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (β, α), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∧, (α, ⊤), heytingalgebra4)  == α
@test collatetruth(∧, (⊤, α), heytingalgebra4)  == α
@test collatetruth(∧, (β, ⊤), heytingalgebra4)  == β
@test collatetruth(∧, (⊤, β), heytingalgebra4)  == β

@test collatetruth(∨, (⊥, ⊥), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(∨, (⊥, α), heytingalgebra4)  == α
@test collatetruth(∨, (α, ⊥), heytingalgebra4)  == α
@test collatetruth(∨, (⊥, β), heytingalgebra4)  == β
@test collatetruth(∨, (β, ⊥), heytingalgebra4)  == β
@test collatetruth(∨, (⊥, ⊤), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (⊤, ⊥), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (α, β), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (β, α), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (α, ⊤), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (⊤, α), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (β, ⊤), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(∨, (⊤, β), heytingalgebra4)  == convert(HeytingTruth, ⊤)

@test collatetruth(→, (⊥, α), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (α, ⊥), heytingalgebra4)  == β
@test collatetruth(→, (⊥, β), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (β, ⊥), heytingalgebra4)  == α
@test collatetruth(→, (⊥, ⊤), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (⊤, ⊥), heytingalgebra4)  == convert(HeytingTruth, ⊥)
@test collatetruth(→, (α, β), heytingalgebra4)  == β
@test collatetruth(→, (β, α), heytingalgebra4)  == α
@test collatetruth(→, (α, ⊤), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (⊤, α), heytingalgebra4)  == α
@test collatetruth(→, (β, ⊤), heytingalgebra4)  == convert(HeytingTruth, ⊤)
@test collatetruth(→, (⊤, β), heytingalgebra4)  == β

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
