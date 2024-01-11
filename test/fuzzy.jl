using SoleLogics
using Graphs

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

@atoms a b
φ = parseformula("(a∧b)∨a")
td1 = TruthDict([a=>α, b=>β])
td2 = TruthDict([a=>⊤, b=>β])

interpret(φ, td1, heytingalgebra4) == α

check(φ, td1, heytingalgebra4) == false
check(φ, td2, heytingalgebra4) == true
