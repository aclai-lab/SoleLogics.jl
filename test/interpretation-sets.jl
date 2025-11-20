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
cnf = LeftmostConjunctiveForm([disj, disj])
dnf = LeftmostDisjunctiveForm([conj, conj])

@test check(conj, s, 1) == false
@test check(conj, s, 2) == true
@test !all(check(conj, s, 1))
@test [check(conj, i) for i in eachinstance(s)] == [false, true, true]

@test check(disj, s, 1) == false
@test check(disj, s, 2) == true
@test !all(check(disj, s, 1))
@test [check(disj, i) for i in eachinstance(s)] == [false, true, true]

@test check(cnf, s, 1) == false
@test check(cnf, s, 2) == true
@test !all(check(cnf, s, 1))
@test [check(cnf, i) for i in eachinstance(s)] == [false, true, true]

@test check(dnf, s, 1) == false
@test check(dnf, s, 2) == true
@test !all(check(dnf, s, 1))
@test [check(dnf, i) for i in eachinstance(s)] == [false, true, true]
