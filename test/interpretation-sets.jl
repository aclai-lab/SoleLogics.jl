using Test
using SoleLogics

s = SoleLogics.InterpretationVector([TruthDict((1,false)), TruthDict((1,true)), TruthDict((1,true)),])

@test check(Atom(1), s, 1) == false
@test check(Atom(1), s, 2) == true
@test !all(check(Atom(1), s, 1))
@test [check(Atom(1), i) for i in eachinstance(s)] == [false, true, true]

@test check(Atom(1) ∧ Atom(1), s, 1) == false
@test check(Atom(1) ∧ Atom(1), s, 2) == true
@test !all(check(Atom(1) ∧ Atom(1), s, 1))
@test [check(Atom(1) ∧ Atom(1), i) for i in eachinstance(s)] == [false, true, true]

conj = LeftmostConjunctiveForm([Atom(1), Atom(1)])
disj = LeftmostDisjunctiveForm([Atom(1), Atom(1)])
conj_nf = LeftmostConjunctiveForm([disj, disj])
disj_nf = LeftmostDisjunctiveForm([conj, conj])

@test check(conj, s, 1) == false
@test check(conj, s, 2) == true
@test !all(check(conj, s, 1))
@test [check(conj, i) for i in eachinstance(s)] == [false, true, true]

@test check(disj, s, 1) == false
@test check(disj, s, 2) == true
@test !all(check(disj, s, 1))
@test [check(disj, i) for i in eachinstance(s)] == [false, true, true]

@test check(conj_nf, s, 1) == false
@test check(conj_nf, s, 2) == true
@test !all(check(conj_nf, s, 1))
@test [check(conj_nf, i) for i in eachinstance(s)] == [false, true, true]

@test check(disj_nf, s, 1) == false
@test check(disj_nf, s, 2) == true
@test !all(check(disj_nf, s, 1))
@test [check(disj_nf, i) for i in eachinstance(s)] == [false, true, true]
