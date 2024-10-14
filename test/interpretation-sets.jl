using Test
using SoleLogics

s = SoleLogics.InterpretationVector([TruthDict((1,false)), TruthDict((1,true)), TruthDict((1,true)),])

@test_nowarn check(Atom(1), s, 1)
@test check(Atom(1), s, 1) == false
@test check(Atom(1), s, 2) == true

@test_nowarn check(Atom(1), s)
@test !all(check(Atom(1), s, 1))

@test_nowarn check.(Atom(1), eachinstance(s))
@test check.(Atom(1), eachinstance(s)) == [false, true, true]
