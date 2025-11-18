using Test
using SoleLogics

s = SoleLogics.InterpretationVector([TruthDict((1,false)), TruthDict((1,true)), TruthDict((1,true)),])

@test check(Atom(1), s, 1) == false
@test check(Atom(1), s, 2) == true
@test !all(check(Atom(1), s, 1))
@test [check(Atom(1), i) for i in SoleLogics.eachinstance(s)] == [false, true, true]

@test check(Atom(1) ∧ Atom(1), s, 1) == false
@test check(Atom(1) ∧ Atom(1), s, 2) == true
@test !all(check(Atom(1) ∧ Atom(1), s, 1))
@test [check(Atom(1) ∧ Atom(1), i) for i in SoleLogics.eachinstance(s)] == [false, true, true]

@test check(LeftmostConjunctiveForm([Atom(1), Atom(1)]), s, 1) == false
@test check(LeftmostConjunctiveForm([Atom(1), Atom(1)]), s, 2) == true
@test !all(check(LeftmostConjunctiveForm([Atom(1), Atom(1)]), s, 1))
@test [check(LeftmostConjunctiveForm([Atom(1), Atom(1)]), i) for i in SoleLogics.eachinstance(s)] == [false, true, true]

@test check(LeftmostDisjunctiveForm([Atom(1), Atom(1)]), s, 1) == false
@test check(LeftmostDisjunctiveForm([Atom(1), Atom(1)]), s, 2) == true
@test !all(check(LeftmostDisjunctiveForm([Atom(1), Atom(1)]), s, 1))
@test [check(LeftmostDisjunctiveForm([Atom(1), Atom(1)]), i) for i in SoleLogics.eachinstance(s)] == [false, true, true]

@test check(LeftmostConjunctiveForm([LeftmostDisjunctiveForm([Atom(1), Atom(1)]), LeftmostDisjunctiveForm([Atom(1), Atom(1)])]), s, 1) == false
@test check(LeftmostConjunctiveForm([LeftmostDisjunctiveForm([Atom(1), Atom(1)]), LeftmostDisjunctiveForm([Atom(1), Atom(1)])]), s, 2) == true
@test !all(check(LeftmostConjunctiveForm([LeftmostDisjunctiveForm([Atom(1), Atom(1)]), LeftmostDisjunctiveForm([Atom(1), Atom(1)])]), s, 1))
@test [check(LeftmostConjunctiveForm([LeftmostDisjunctiveForm([Atom(1), Atom(1)]), LeftmostDisjunctiveForm([Atom(1), Atom(1)])]), i) for i in SoleLogics.eachinstance(s)] == [false, true, true]

@test check(LeftmostDisjunctiveForm([LeftmostConjunctiveForm([Atom(1), Atom(1)]), LeftmostConjunctiveForm([Atom(1), Atom(1)])]), s, 1) == false
@test check(LeftmostDisjunctiveForm([LeftmostConjunctiveForm([Atom(1), Atom(1)]), LeftmostConjunctiveForm([Atom(1), Atom(1)])]), s, 2) == true
@test !all(check(LeftmostDisjunctiveForm([LeftmostConjunctiveForm([Atom(1), Atom(1)]), LeftmostConjunctiveForm([Atom(1), Atom(1)])]), s, 1))
@test [check(LeftmostDisjunctiveForm([LeftmostConjunctiveForm([Atom(1), Atom(1)]), LeftmostConjunctiveForm([Atom(1), Atom(1)])]), i) for i in SoleLogics.eachinstance(s)] == [false, true, true]
