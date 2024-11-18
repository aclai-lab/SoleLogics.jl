
using Test
using SoleLogics

@atoms A B C D
φ = ¬((A ∨ B) ∧ (C ∨ D))
result = SoleLogics.cnf(φ)

atoms(φ)

φ = deepcopy(normalize(φ; profile = :nnf))


@test normalize(SoleLogics.tree(SoleLogics.cnf(φ))) == normalize(
	(¬C ∨ ¬A) ∧
	(¬D ∨ ¬A) ∧
	(¬C ∨ ¬B) ∧
	(¬D ∨ ¬B)
)

@test_nowarn SoleLogics.dnf(φ)
@test_nowarn SoleLogics.dnf(φ ∧ φ)
@test_nowarn dnf(φ ∧ A)
@test_nowarn dnf(φ ∧ (¬A ∧ ¬B))
@test_broken dnf(φ ∧ φ) == dnf(φ)
@test dnf(φ ∧ φ) === dnf(φ)
@test string(dnf(φ ∧ φ)) == string(dnf(φ))