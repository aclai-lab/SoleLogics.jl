using SoleLogics
using Graphs

@heytingalgebra heytingalgebra4 (⊥0, α) (⊥0, β) (α, ⊤0) (β, ⊤0)

@test precedes(heytingalgebra4, ⊥0, α)   == true
@test precedes(heytingalgebra4, α, ⊥0)   == false
@test precedes(heytingalgebra4, ⊥0, β)   == true
@test precedes(heytingalgebra4, β, ⊥0)   == false
@test precedes(heytingalgebra4, ⊥0, ⊤0)   == true
@test precedes(heytingalgebra4, ⊤0, ⊥0)   == false
@test precedes(heytingalgebra4, α, β)   == false
@test precedes(heytingalgebra4, β, α)   == false
@test precedes(heytingalgebra4, α, ⊤0)   == true
@test precedes(heytingalgebra4, ⊤0, α)   == false
@test precedes(heytingalgebra4, β, ⊤0)   == true
@test precedes(heytingalgebra4, ⊤0, β)   == false

@test succeedes(heytingalgebra4, ⊥0, α)  == false
@test succeedes(heytingalgebra4, α, ⊥0)  == true
@test succeedes(heytingalgebra4, ⊥0, β)  == false
@test succeedes(heytingalgebra4, β, ⊥0)  == true
@test succeedes(heytingalgebra4, ⊥0, ⊤0)  == false
@test succeedes(heytingalgebra4, ⊤0, ⊥0)  == true
@test succeedes(heytingalgebra4, α, β)  == false
@test succeedes(heytingalgebra4, β, α)  == false
@test succeedes(heytingalgebra4, α, ⊤0)  == false
@test succeedes(heytingalgebra4, ⊤0, α)  == true
@test succeedes(heytingalgebra4, β, ⊤0)  == false
@test succeedes(heytingalgebra4, ⊤0, β)  == true

@test collatetruth(∧, (⊥0, α), heytingalgebra4)  == ⊥0
@test collatetruth(∧, (α, ⊥0), heytingalgebra4)  == ⊥0
@test collatetruth(∧, (⊥0, β), heytingalgebra4)  == ⊥0
@test collatetruth(∧, (β, ⊥0), heytingalgebra4)  == ⊥0
@test collatetruth(∧, (⊥0, ⊤0), heytingalgebra4)  == ⊥0
@test collatetruth(∧, (⊤0, ⊥0), heytingalgebra4)  == ⊥0
@test collatetruth(∧, (α, β), heytingalgebra4)  == ⊥0
@test collatetruth(∧, (β, α), heytingalgebra4)  == ⊥0
@test collatetruth(∧, (α, ⊤0), heytingalgebra4)  == α
@test collatetruth(∧, (⊤0, α), heytingalgebra4)  == α
@test collatetruth(∧, (β, ⊤0), heytingalgebra4)  == β
@test collatetruth(∧, (⊤0, β), heytingalgebra4)  == β

@test collatetruth(∨, (⊥0, ⊥0), heytingalgebra4)  == ⊥0
@test collatetruth(∨, (⊥0, α), heytingalgebra4)  == α
@test collatetruth(∨, (α, ⊥0), heytingalgebra4)  == α
@test collatetruth(∨, (⊥0, β), heytingalgebra4)  == β
@test collatetruth(∨, (β, ⊥0), heytingalgebra4)  == β
@test collatetruth(∨, (⊥0, ⊤0), heytingalgebra4)  == ⊤0
@test collatetruth(∨, (⊤0, ⊥0), heytingalgebra4)  == ⊤0
@test collatetruth(∨, (α, β), heytingalgebra4)  == ⊤0
@test collatetruth(∨, (β, α), heytingalgebra4)  == ⊤0
@test collatetruth(∨, (α, ⊤0), heytingalgebra4)  == ⊤0
@test collatetruth(∨, (⊤0, α), heytingalgebra4)  == ⊤0
@test collatetruth(∨, (β, ⊤0), heytingalgebra4)  == ⊤0
@test collatetruth(∨, (⊤0, β), heytingalgebra4)  == ⊤0

@test collatetruth(→, (⊥0, α), heytingalgebra4)  == ⊤0
@test collatetruth(→, (α, ⊥0), heytingalgebra4)  == β
@test collatetruth(→, (⊥0, β), heytingalgebra4)  == ⊤0
@test collatetruth(→, (β, ⊥0), heytingalgebra4)  == α
@test collatetruth(→, (⊥0, ⊤0), heytingalgebra4)  == ⊤0
@test collatetruth(→, (⊤0, ⊥0), heytingalgebra4)  == ⊥0
@test collatetruth(→, (α, β), heytingalgebra4)  == β
@test collatetruth(→, (β, α), heytingalgebra4)  == α
@test collatetruth(→, (α, ⊤0), heytingalgebra4)  == ⊤0
@test collatetruth(→, (⊤0, α), heytingalgebra4)  == α
@test collatetruth(→, (β, ⊤0), heytingalgebra4)  == ⊤0
@test collatetruth(→, (⊤0, β), heytingalgebra4)  == β
