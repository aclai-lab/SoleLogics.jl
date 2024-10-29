using Test
using SoleLogics

s = SoleLogics.InterpretationVector([TruthDict((1,false)), TruthDict((1,true)), TruthDict((1,true)),])

@test_nowarn check(Atom(1), s, 1)
@test check(Atom(1), s, 1) == false
@test check(Atom(1), s, 2) == true

@test_nowarn check(Atom(1), s)
@test !all(check(Atom(1), s, 1))

@test_nowarn [check(Atom(1), i) for i in SoleLogics.eachinstance(s)]
@test [check(Atom(1), i) for i in SoleLogics.eachinstance(s)] == [false, true, true]

################################################################################

using DecisionTree: load_data
using DataFrames
using SoleData

X, y = load_data("iris")
X = Float64.(X)
X_df = DataFrame(X, :auto)
s = scalarlogiset(X_df; allow_propositional = true)
myalphabet = @test_nowarn alphabet(s)
a = @test_nowarn first(atoms(myalphabet)
@test_nowarn [check(a, i) for i in SoleLogics.eachinstance(s)]

