@testset "Operators" begin
    @test is_unary_operator(NEGATION) == true
    @test is_unary_operator(IMPLICATION) == false
    @test is_unary_operator(CONJUNCTION) == false
    @test is_unary_operator(DISJUNCTION) == false
    @test is_unary_operator(DIAMOND) == true
    @test is_unary_operator(BOX) == true

    @test is_binary_operator(NEGATION) == false
    @test is_binary_operator(IMPLICATION) == true
    @test is_binary_operator(CONJUNCTION) == true
    @test is_binary_operator(DISJUNCTION) == true
    @test is_binary_operator(DIAMOND) == false
    @test is_binary_operator(BOX) == false


    @test is_commutative(NEGATION) == false
    @test is_commutative(IMPLICATION) == false
    @test is_commutative(CONJUNCTION) == true
    @test is_commutative(DISJUNCTION) == true
    @test is_commutative(DIAMOND) == false
    @test is_commutative(BOX) == false
end
