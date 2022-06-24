using SoleLogics
using Test

@testset "SoleLogics.jl" begin
    @testset "operators.jl" begin
        # testing unary operators
        @test reltype(SoleLogics.UnaryOperator("¬")) == :¬
        @test reltype(SoleLogics.UnaryOperator("∧")) == :∧
        @test reltype(SoleLogics.UnaryOperator("∨")) == :∨
    end
end
