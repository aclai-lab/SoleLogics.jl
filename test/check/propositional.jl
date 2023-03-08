# using Revise; using SoleLogics; using Test

@testset "Propositional model checking" begin

d0 = Dict(["a" => true, "b" => false, "c" => true])
@test_throws ErrorException "a" in d0
@test haskey(d0, "a")
@test haskey(d0, Proposition("a"))
@test d0["a"]
@test !d0["b"]
@test check(parseformula("a ∧ ¬b"), d0)
@test check(parseformula("a ∧ c"), d0)

v0 = ["a", "c"]
@test "a" in v0
@test !("b" in v0)
@test !(Proposition("a") in v0)
@test check(parseformula("a ∧ ¬b"), v0)
@test check(parseformula("a ∧ c"), v0)

@test !check(parseformula("a ∧ b"), ["a"])
@test !check(parseformula("a ∧ ¬b"), ["a", "b"])
@test check(parseformula("a ∧ ¬b"), ["a"])

@test_nowarn TruthDict(1:4)
@test_nowarn TruthDict(1:4, false)

t0 = @test_nowarn TruthDict(["a" => true, "b" => false, "c" => true])
@test haskey(t0, Proposition("a"))
@test haskey(t0, Proposition("b"))
@test haskey(t0, "a")
@test haskey(t0, "b")
@test check(Proposition("a"), t0)
@test !check(Proposition("b"), t0)
@test check(parseformula("a ∨ b"), t0)

t1 = @test_nowarn TruthDict([1 => true, 2 => false, 3 => true])

@test_nowarn t1[2] = false
@test_nowarn t1[Proposition(2)]
@test_nowarn t1[2]
@test_nowarn t1[2.0]

@test_nowarn t1[2] = false
@test_nowarn t1[Proposition(2)] = false
@test_throws MethodError t1[Proposition(2.0)] = false
@test_throws MethodError t1[2.0] = false
@test_throws MethodError t1[10.0] = false

t2 = @test_nowarn TruthDict(Pair{Real,Bool}[1.0 => true, 2 => true, 3 => true])
@test haskey(t2, Proposition(1))
@test !xor(haskey(t2, Proposition(1)), isequal(1,1.0)) # Weird, but is consistent with the behavior: isequal(1,1.0)
# [isequal(Proposition(1.0),k) for k in keys(t2)]
@test haskey(t2, Proposition(1.0))
@test haskey(t2, Proposition(2))
@test haskey(t2, 1.0)
@test haskey(t2, 1)
@test haskey(t2, 2)

@test_nowarn t2[1]
@test_nowarn t2[Proposition(1)]
@test_nowarn t2[Proposition(1.0)]


@test_nowarn TruthDict([(Proposition(1.0), true), (Proposition(2), true), (Proposition(3), true)])
@test_nowarn TruthDict([(1.0, true), (2, true), (3, true)])
@test_nowarn TruthDict([Proposition(1.0) => true, Proposition(2) => true, Proposition(3) => true])
@test_nowarn TruthDict([(Proposition(1.0), true), (Proposition(2), true), (Proposition(3), true)])
@test_nowarn TruthDict(Dict([Proposition(1.0) => true, Proposition(2) => true, Proposition(3) => true]))
@test_nowarn TruthDict(1.0 => true)
@test_nowarn TruthDict(Proposition(1.0) => true)

@test_nowarn DefaultedTruthDict([(Proposition(1.0), true), (Proposition(2), true), (Proposition(3), true)])
@test_nowarn DefaultedTruthDict([(1.0, true), (2, true), (3, true)])
@test_nowarn DefaultedTruthDict([Proposition(1.0) => true, Proposition(2) => true, Proposition(3) => true])
@test_nowarn DefaultedTruthDict([(Proposition(1.0), true), (Proposition(2), true), (Proposition(3), true)])
@test_nowarn DefaultedTruthDict(Dict([Proposition(1.0) => true, Proposition(2) => true, Proposition(3) => true]))
@test_nowarn DefaultedTruthDict(1.0 => true)
@test_nowarn DefaultedTruthDict(Proposition(1.0) => true)

@test !check(parseformula("a ∧ b"), DefaultedTruthDict(["a"]))
@test !check(parseformula("a ∧ ¬b"), DefaultedTruthDict(["a", "b"]))
@test check(parseformula("a ∧ ¬b"), DefaultedTruthDict(["a"]))

end
