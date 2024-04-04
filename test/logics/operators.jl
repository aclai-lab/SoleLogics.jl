@testset "Operators" begin
    @test isunary(NEGATION) == true
    @test isunary(IMPLICATION) == false
    @test isunary(CONJUNCTION) == false
    @test isunary(DISJUNCTION) == false
    @test isunary(DIAMOND) == true
    @test isunary(BOX) == true

    @test isbinary(NEGATION) == false
    @test isbinary(IMPLICATION) == true
    @test isbinary(CONJUNCTION) == true
    @test isbinary(DISJUNCTION) == true
    @test isbinary(DIAMOND) == false
    @test isbinary(BOX) == false

    @test SoleLogics.iscommutative(NEGATION) == true
    @test SoleLogics.iscommutative(IMPLICATION) == false
    @test SoleLogics.iscommutative(CONJUNCTION) == true
    @test SoleLogics.iscommutative(DISJUNCTION) == true
    @test SoleLogics.iscommutative(DIAMOND) == true
    @test SoleLogics.iscommutative(BOX) == true

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
