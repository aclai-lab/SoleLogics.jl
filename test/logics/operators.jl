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


    @test iscommutative(NEGATION) == false
    @test iscommutative(IMPLICATION) == false
    @test iscommutative(CONJUNCTION) == true
    @test iscommutative(DISJUNCTION) == true
    @test iscommutative(DIAMOND) == false
    @test iscommutative(BOX) == false
end
