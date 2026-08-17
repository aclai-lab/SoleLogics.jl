
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
    @test_nowarn istop(TruthDict(["p", "q"])["p"])

    io = IOBuffer();
    print(io, TruthDict(["p", "q"]))
    @test String(take!(io)) == "TruthDict with values:\n┌────────┬────────┐\n│      q │      p │\n│ String │ String │\n├────────┼────────┤\n│      ⊤ │      ⊤ │\n└────────┴────────┘\n"

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

    # Equality and hashing (#108)
    td_a = TruthDict(["p" => true, "q" => false])
    td_b = TruthDict(["p" => true, "q" => false])
    td_c = TruthDict(["p" => true, "q" => true])
    @test td_a == deepcopy(td_a)
    @test isequal(td_a, deepcopy(td_a))
    @test td_a == td_b
    @test isequal(td_a, td_b)
    @test td_a != td_c
    @test !isequal(td_a, td_c)
    @test hash(td_a) == hash(deepcopy(td_a))
    @test hash(td_a) == hash(td_b)
    @test length(Set([td_a, deepcopy(td_a), td_b])) == 1
    @test Dict(td_a => 1)[deepcopy(td_a)] == 1
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

    # Equality and hashing (#108)
    dtd_a = DefaultedTruthDict(["p" => true, "q" => false], TOP)
    dtd_b = DefaultedTruthDict(["p" => true, "q" => false], TOP)
    dtd_c = DefaultedTruthDict(["p" => true, "q" => false], BOT)
    dtd_d = DefaultedTruthDict(["p" => true, "q" => true], TOP)
    @test dtd_a == deepcopy(dtd_a)
    @test isequal(dtd_a, deepcopy(dtd_a))
    @test dtd_a == dtd_b
    @test isequal(dtd_a, dtd_b)
    @test dtd_a != dtd_c
    @test !isequal(dtd_a, dtd_c)
    @test dtd_a != dtd_d
    @test !isequal(dtd_a, dtd_d)
    @test hash(dtd_a) == hash(deepcopy(dtd_a))
    @test hash(dtd_a) == hash(dtd_b)
    @test length(Set([dtd_a, deepcopy(dtd_a), dtd_b])) == 1
    @test Dict(dtd_a => 1)[deepcopy(dtd_a)] == 1
end

@testset "Issue #108 reproduction" begin
    my_model = randmodel(42, 5, 10, [Atom("p"), Atom("q")], BooleanAlgebra())
    @test my_model.assignment == deepcopy(my_model.assignment)
end
