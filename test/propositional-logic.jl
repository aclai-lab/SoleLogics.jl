using Test
using SoleLogics

# g = SoleLogics.CompleteFlatGrammar(ExplicitAlphabet([Atom("p"), Atom("q")]), SoleLogics.BASE_CONNECTIVES)
# @test propositionallogic(; alphabet = ExplicitAlphabet([Atom("p"), Atom("q")])) == BaseLogic(g, BooleanAlgebra())

io = IOBuffer();
_keys = [1,2,3];
_values = [⊤,⊤,⊥];
SoleLogics.show_assignment_pretty_table(io,_keys,_values);
@test String(take!(io)) == "┌───────┬───────┬───────┐\n│     1 │     2 │     3 │\n│ Int64 │ Int64 │ Int64 │\n├───────┼───────┼───────┤\n│     ⊤ │     ⊤ │     ⊥ │\n└───────┴───────┴───────┘\n"

