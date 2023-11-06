# using Revise; using SoleLogics; using Test

@testset "Propositional model checking" begin

d0 = Dict(["a" => true, "b" => false, "c" => true])
@test_throws ErrorException "a" in d0
@test haskey(d0, "a")
@test haskey(d0, Atom("a"))
@test d0["a"]
@test !d0["b"]
@test check(parsebaseformula("a ∧ ¬b"), d0)
@test check(parsebaseformula("a ∧ c"), d0)

v0 = ["a", "c"]
@test "a" in v0
@test !("b" in v0)
@test !(Atom("a") in v0)
@test check(parsebaseformula("a ∧ ¬b"), v0)
@test check(parsebaseformula("a ∧ c"), v0)

@test !check(parsebaseformula("a ∧ b"), ["a"])
@test !check(parsebaseformula("a ∧ ¬b"), ["a", "b"])
@test check(parsebaseformula("a ∧ ¬b"), ["a"])

@test_nowarn TruthDict(1:4)
@test_nowarn TruthDict(1:4, false)

t0 = @test_nowarn TruthDict(["a" => true, "b" => false, "c" => true])
@test haskey(t0, Atom("a"))
@test haskey(t0, Atom("b"))
@test haskey(t0, "a")
@test haskey(t0, "b")
@test check(Atom("a"), t0)
@test !check(Atom("b"), t0)
@test check(parsebaseformula("a ∨ b"), t0)

t1 = @test_nowarn TruthDict([1 => true, 2 => false, 3 => true])

@test_nowarn t1[2] = false
@test_nowarn t1[Atom(2)]
@test_nowarn t1[2]
@test_nowarn t1[2.0]

@test_nowarn t1[2] = false
@test_nowarn t1[Atom(2)] = false
@test_throws MethodError t1[Atom(2.0)] = false
@test_throws MethodError t1[2.0] = false
@test_throws MethodError t1[10.0] = false

t2 = @test_nowarn TruthDict(Pair{Real,Bool}[1.0 => true, 2 => true, 3 => true])
@test haskey(t2, Atom(1))
@test !xor(haskey(t2, Atom(1)), isequal(1,1.0)) # Weird, but is consistent with the behavior: isequal(1,1.0)
# [isequal(Atom(1.0), k) for k in keys(t2)]
@test haskey(t2, Atom(1.0))
@test haskey(t2, Atom(2))
@test haskey(t2, 1.0)
@test haskey(t2, 1)
@test haskey(t2, 2)

@test_nowarn t2[1]
@test_nowarn t2[Atom(1)]
@test_nowarn t2[Atom(1.0)]


@test_nowarn TruthDict([(Atom(1.0), true), (Atom(2), true), (Atom(3), true)])
@test_nowarn TruthDict([(1.0, true), (2, true), (3, true)])
@test_nowarn TruthDict([Atom(1.0) => true, Atom(2) => true, Atom(3) => true])
@test_nowarn TruthDict([(Atom(1.0), true), (Atom(2), true), (Atom(3), true)])
@test_nowarn TruthDict(Dict([Atom(1.0) => true, Atom(2) => true, Atom(3) => true]))
@test_nowarn TruthDict(1.0 => true)
@test_nowarn TruthDict(Atom(1.0) => true)

@test_nowarn DefaultedTruthDict([(Atom(1.0), true), (Atom(2), true), (Atom(3), true)])
@test_nowarn DefaultedTruthDict([(Atom(1.0), true), (Atom(2), BOT), (Atom(3), true)])
@test_nowarn DefaultedTruthDict([(1.0, true), (2, true), (3, true)])
@test_nowarn DefaultedTruthDict([Atom(1.0) => true, Atom(2) => true, Atom(3) => true])
@test_nowarn DefaultedTruthDict([(Atom(1.0), true), (Atom(2), true), (Atom(3), true)])
@test_nowarn DefaultedTruthDict(Dict([Atom(1.0) => true, Atom(2) => true, Atom(3) => true]))
@test_nowarn DefaultedTruthDict(1.0 => true)
@test_nowarn DefaultedTruthDict(Atom(1.0) => true)

@test !check(parsebaseformula("a ∧ b"), DefaultedTruthDict(["a"]))
@test !check(parsebaseformula("a ∧ ¬b"), DefaultedTruthDict(["a", "b"]))
@test check(parsebaseformula("a ∧ ¬b"), DefaultedTruthDict(["a"]))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test TruthDict(["p", "q"])["p"] isa Top
@test TruthDict(["p", "q"])[Atom("p")] isa Top
@test_throws MethodError interpret("p", TruthDict(["p", "q"])) isa Top
@test interpret(Atom("p"), TruthDict(["p", "q"])) isa Top

@test TruthDict(["p", "q"])["r"] isa Atom
@test TruthDict(["p", "q"])[Atom("r")] isa Atom
@test_throws MethodError interpret("r", TruthDict(["p", "q"])) isa Atom
@test interpret(Atom("r"), TruthDict(["p", "q"])) isa Atom

@test DefaultedTruthDict(["p", "q"])["p"] isa Top
@test DefaultedTruthDict(["p", "q"])[Atom("p")] isa Top
@test_throws MethodError interpret("p", DefaultedTruthDict(["p", "q"])) isa Top
@test interpret(Atom("p"), DefaultedTruthDict(["p", "q"])) isa Top

@test DefaultedTruthDict(["p", "q"])["r"] isa Bot
@test DefaultedTruthDict(["p", "q"])[Atom("r")] isa Bot
@test_throws MethodError interpret("r", DefaultedTruthDict(["p", "q"])) isa Bot
@test interpret(Atom("r"), DefaultedTruthDict(["p", "q"])) isa Bot


#  normalization: negations compression ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test syntaxstring(normalize(parsetree("¬¬ p"))) == "p"
@test syntaxstring(normalize(parsetree("¬¬¬ p"))) == "¬p"
@test syntaxstring(normalize(parsetree("¬¬¬¬ p"))) == "p"
@test syntaxstring(normalize(parsetree("¬¬¬ □□□ ◊◊◊ p ∧ ¬¬¬ q")); remove_redundant_parentheses = true) == "¬q ∧ ◊◊◊□□□¬p"
@test syntaxstring(normalize(parsetree("¬¬¬ □□□ ◊◊◊ p → ¬¬¬ q")); remove_redundant_parentheses = true) == "¬q ∨ □□□◊◊◊p"

# normalization: diamond and box compression ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test syntaxstring(normalize(parsetree("¬◊¬p"))) == "□p"
@test syntaxstring(normalize(parsetree("¬□¬p"))) == "◊p"

# normalization: rotate commutatives ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

_test_rot_comm1 = normalize(parsetree("((d ∧ c) ∧ ((e ∧ f) ∧ (g ∧ h))) ∧ (b ∧ a)"))
for f in [
    parsetree("(a∧b)∧(c∧d)∧(e∧f)∧(g∧h)")
    parsetree("(c∧d)∧(b∧a)∧(f∧e)∧(g∧h)")
    parsetree("(a∧b)∧(f∧e)∧(d∧c)∧(g∧h)")
    parsetree("(b∧a)∧(h∧g)∧(d∧c)∧(f∧e)")
    parsetree("(b∧a)∧(c∧d)∧(f∧e)∧(g∧h)")
    parsetree("(a∧b)∧(d∧c)∧(f∧e)∧(g∧h)")
    parsetree("(b∧a)∧(d∧c)∧(f∧e)∧(h∧g)")
]
    @test syntaxstring(f |> normalize) == syntaxstring(_test_rot_comm1)
end

_test_rot_comm2 = normalize(parsetree("(a∧b)∧(c∧d)∧(e∧f)∧(g∧h)"))
for f in [
    parsetree("(c∧d)∧(b∧a)∧(f∧e)∧(g∧h)"),
    parsetree("(a∧b)∧(f∧e)∧(d∧c)∧(g∧h)"),
    parsetree("(b∧a)∧(h∧g)∧(d∧c)∧(f∧e)")
]
    @test syntaxstring(f |> normalize) == syntaxstring(_test_rot_comm2)
end

_test_rot_comm3 = normalize(parsetree("b∧a∧d∧c∧e∧f∧h∧g"))
for f in [
    parsetree("a∧b∧c∧d∧e∧f∧g∧h"),
    parsetree("g∧d∧a∧b∧c∧f∧e∧h")
]
    @test syntaxstring(f |> normalize) == syntaxstring(_test_rot_comm3)

end

_test_rot_comm4 = normalize(parsetree("g∧c∧f∧((p∧¬q)→r)∧h∧d∧a∧b"))
for f in [
    parsetree("a∧b∧c∧d∧((p∧¬q)→r)∧f∧g∧h"),
    parsetree("g∧d∧a∧b∧c∧f∧((p∧¬q)→r)∧h")
]
    @test syntaxstring(f |> normalize) == syntaxstring(_test_rot_comm4)
end

end
