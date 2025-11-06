@testset "Operators" begin
    @test isunary(NEGATION)
    @test !isunary(IMPLICATION)
    @test !isunary(CONJUNCTION)
    @test !isunary(DISJUNCTION)
    @test isunary(DIAMOND)
    @test isunary(BOX)

    @test !isbinary(NEGATION)
    @test isbinary(IMPLICATION)
    @test isbinary(CONJUNCTION)
    @test isbinary(DISJUNCTION)
    @test !isbinary(DIAMOND)
    @test !isbinary(BOX)

    @test !isternary(BOX)

    @test iscommutative(NEGATION)
    @test !iscommutative(IMPLICATION)
    @test iscommutative(CONJUNCTION)
    @test iscommutative(DISJUNCTION)
    @test iscommutative(DIAMOND)
    @test iscommutative(BOX)

    # Associativity tests

    @test parseformula("(1 → 2) → 3") != parseformula("1 → 2 → 3")
    @test parseformula("1 → (2 → 3)") == parseformula("1 → 2 → 3")

    @test →(Atom.(["1","2","3"])...) == parseformula("1 → 2 → 3")
    @test ∧(Atom.(["1","2","3"])...) == parseformula("1 ∧ 2 ∧ 3")

    @test (@synexpr my_a → my_b → my_c) == parseformula("my_a → my_b → my_c")
    @test (@synexpr Atom("1") → Atom("2") → Atom("3"))  == parseformula("1 → 2 → 3")

    @test (Atom("1") → Atom("2") → Atom("3")) == parseformula("1 → 2 → 3")
    @test (Atom("1") ∧ Atom("2") ∧ Atom("3")) == parseformula("1 ∧ 2 ∧ 3")

end
