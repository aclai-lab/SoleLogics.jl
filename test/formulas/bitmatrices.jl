using Test
using SoleLogics

@test_nowarn BitMatrixNormalForm(true, (rand(Bool, 3, 10)))

conj_nf = @test_nowarn BitMatrixCNF([
  1  0
  0  0
  1  0
])

@test nconjuncts(conj_nf) == 3

t = tree(conj_nf; silent = true)
@test syntaxstring(conj_nf) == 
  syntaxstring(t)


@testset for i in 1:2
  @test check(conj_nf, TruthDict([i])) == check(t, TruthDict([i]))
end

disj_nf = @test_nowarn BitMatrixDNF([
  1  0  1  0  1  1  1  1  1  0
  0  0  1  1  1  0  0  0  1  0
  1  0  1  1  1  1  1  1  0  0
])

@test syntaxstring(disj_nf) == "(1 ∧ ¬2 ∧ 3 ∧ ¬4 ∧ 5 ∧ 6 ∧ 7 ∧ 8 ∧ 9 ∧ ¬10) ∨ (¬1 ∧ ¬2 ∧ 3 ∧ 4 ∧ 5 ∧ ¬6 ∧ ¬7 ∧ ¬8 ∧ 9 ∧ ¬10) ∨ (1 ∧ ¬2 ∧ 3 ∧ 4 ∧ 5 ∧ 6 ∧ 7 ∧ 8 ∧ ¬9 ∧ ¬10)"
@test check(disj_nf, TruthDict([1]))
@test check(disj_nf, TruthDict([2]))
@test check(disj_nf, TruthDict([1, 3, 5, 6, 7, 8, 9]))
@test check(disj_nf, TruthDict([3]))

disj_nf = @test_nowarn BitMatrixDNF([
  0  1  1
  1  1  0
  1  1  1
])

@test interpret(disj_nf, TruthDict([1])) == (Atom(2) ∧ ¬Atom(3)) ∨ (Atom(2) ∧ Atom(3))