using Test
using SoleLogics

@testset "Test Propositional Logic" begin

    # g = SoleLogics.CompleteFlatGrammar(ExplicitAlphabet([Atom("p"), Atom("q")]), SoleLogics.BASE_CONNECTIVES)
    # @test propositionallogic(; alphabet = ExplicitAlphabet([Atom("p"), Atom("q")])) == BaseLogic(g, BooleanAlgebra())

    io = IOBuffer();
    keys = [1,2,3];
    values = [⊤,⊤,⊥];
    _hpretty_table(io,keys,values);
    @test String(take!(io)) == "┌───────┬───────┬───────┐\n│     1 │     2 │     3 │\n│ Int64 │ Int64 │ Int64 │\n├───────┼───────┼───────┤\n│     ⊤ │     ⊤ │     ⊥ │\n└───────┴───────┴───────┘\n"

