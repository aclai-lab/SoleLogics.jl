using Test
using SoleLogics

@atoms A B C D
φ = ¬((A ∨ B) ∧ (C ∨ D))

@test normalize(φ; profile = :nnf) == (
	(¬C ∧ ¬D) ∨
	(¬A ∧ ¬B)
)

@test normalize(tree(cnf(φ))) == normalize(
	(¬C ∨ ¬A) ∧
	(¬D ∨ ¬A) ∧
	(¬C ∨ ¬B) ∧
	(¬D ∨ ¬B)
)

@test_nowarn dnf(φ)
@test_nowarn dnf(φ ∧ φ)
@test_nowarn dnf(φ ∧ A)
@test_nowarn dnf(φ ∧ (¬A ∧ ¬B))
@test_broken dnf(φ ∧ φ) == dnf(φ)
@test string(dnf(φ ∧ φ)) == string(dnf(φ))