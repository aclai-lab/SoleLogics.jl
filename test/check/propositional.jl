
@testset "Dict" begin
    d0 = Dict(["a" => true, "b" => false, "c" => true])
    @test haskey(d0, "a")
    @test haskey(d0, Atom("a"))
    @test d0["a"]
    @test !d0["b"]
    @test check(parseformula("a ∧ c"), d0)

    v0 = ["a", "c"]
    @test "a" in v0
    @test !("b" in v0)
    @test !(Atom("a") in v0)
    @test check(parseformula("a ∧ ¬b"), v0)
end

@testset "AbstractArray" begin
    @test check(parseformula("a ∧ b"), ["a", "b"])
    @test !check(parseformula("a ∧ ¬b"), ["a", "b"])
    @test check(parseformula("a ∧ b ∧ c"; atom_parser = str -> only(str)), 'a':'c')
    @test !check(parseformula("a ∧ b ∧ ¬c"; atom_parser = str -> only(str)), 'a':'c')
end

@testset "TruthDict" begin
    @test_nowarn TruthDict(1:4)
    @test_nowarn TruthDict(1:4, false)

    t0 = @test_nowarn TruthDict(["a" => true, "b" => false, "c" => true])
    @test haskey(t0, Atom("a"))
    @test haskey(t0, Atom("b"))
    @test haskey(t0, "a")
    @test haskey(t0, "b")
    @test check(Atom("a"), t0)
    @test !check(Atom("b"), t0)

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
    @test_nowarn TruthDict([(Atom(1.0), true), (Atom(2), BOT), (Atom(3), true)])
    @test_nowarn TruthDict([(1.0, true), (2, true), (3, true)])
    @test_nowarn TruthDict([Atom(1.0) => true, Atom(2) => true, Atom(3) => true])
    @test_nowarn TruthDict([(Atom(1.0), true), (Atom(2), true), (Atom(3), true)])
    @test_nowarn TruthDict(Dict([Atom(1.0) => true, Atom(2) => true, Atom(3) => true]))
    @test_nowarn TruthDict(1.0 => true)
    @test_nowarn TruthDict(Atom(1.0) => true)

    @test TruthDict(["p", "q"])["p"] |> istop
    @test TruthDict(["p", "q"])[Atom("p")] |> istop
    @test_throws MethodError interpret("p", TruthDict(["p", "q"])) |> istop
    @test interpret(Atom("p"), TruthDict(["p", "q"])) |> istop

    @test TruthDict(["p", "q"])["r"] isa AbstractAtom
    @test TruthDict(["p", "q"])[Atom("r")] isa AbstractAtom
    @test_throws MethodError interpret("r", TruthDict(["p", "q"])) isa AbstractAtom
    @test interpret(Atom("r"), TruthDict(["p", "q"])) isa AbstractAtom

end

@testset "DefaultedTruthDict" begin

    @test_nowarn DefaultedTruthDict([(Atom(1.0), true), (Atom(2), true), (Atom(3), true)])
    @test_nowarn DefaultedTruthDict([(Atom(1.0), true), (Atom(2), BOT), (Atom(3), true)])
    @test_nowarn DefaultedTruthDict([(1.0, true), (2, true), (3, true)])
    @test_nowarn DefaultedTruthDict([Atom(1.0) => true, Atom(2) => true, Atom(3) => true])
    @test_nowarn DefaultedTruthDict([(Atom(1.0), true), (Atom(2), true), (Atom(3), true)])
    @test_nowarn DefaultedTruthDict(Dict([Atom(1.0) => true, Atom(2) => true, Atom(3) => true]))
    @test_nowarn DefaultedTruthDict(1.0 => true)
    @test_nowarn DefaultedTruthDict(Atom(1.0) => true)

    @test !check(parseformula("a ∧ b"), DefaultedTruthDict(["a"]))

    @test DefaultedTruthDict(["p", "q"])["p"] |> istop
    @test DefaultedTruthDict(["p", "q"])[Atom("p")] |> istop
    @test_throws MethodError interpret("p", DefaultedTruthDict(["p", "q"])) |> istop
    @test interpret(Atom("p"), DefaultedTruthDict(["p", "q"])) |> istop

    @test DefaultedTruthDict(["p", "q"])["r"] |> isbot
    @test DefaultedTruthDict(["p", "q"])[Atom("r")] |> isbot
    @test_throws MethodError interpret("r", DefaultedTruthDict(["p", "q"])) |> isbot
    @test interpret(Atom("r"), DefaultedTruthDict(["p", "q"])) |> isbot

end