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

    @test typeof(DIAMOND) <: AbstractUnaryOperator
    @test typeof(DIAMOND) <: AbstractModalOperator
    @test typeof(DIAMOND) <: AbstractExistentialModalOperator

    @test typeof(BOX) <: AbstractUnaryOperator
    @test typeof(BOX) <: AbstractModalOperator
    @test typeof(BOX) <: AbstractUniversalModalOperator

    op1234 = EXMODOP(("OP1", "OP2", "OP3", "OP4"))
    @test EXMODOP(("OP1", "OP2", "OP3", "OP4")) isa AbstractExistentialModalOperator
    @test is_unary_operator(op1234) == true
    @test is_binary_operator(op1234) == false
end
