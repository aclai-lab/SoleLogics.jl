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
end
