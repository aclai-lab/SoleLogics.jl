import SoleLogics: arity

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ random ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@testset "Random" begin

_alphabet = ExplicitAlphabet(Proposition.(["p", "q", "r", "s"]))
_operators = [NEGATION, CONJUNCTION, IMPLICATION]

@test_nowarn all([begin
    # Incremental stress test
    randformulatree(i, _alphabet, _operators)
end for i in 1:12])

end

@testset "Random+Parsing" begin

const TERNOP = SoleLogics.NamedOperator{:⇶}()
SoleLogics.arity(::Type{typeof(TERNOP)}) = 3

const QUATERNOP = SoleLogics.NamedOperator{:⩰}()
SoleLogics.arity(::Type{typeof(QUATERNOP)}) = 4

_alphabet = ExplicitAlphabet(Proposition.(["p", "q", "r", "s"]))
_operators = [NEGATION, CONJUNCTION, IMPLICATION,
    DiamondRelationalOperator(globalrel), BoxRelationalOperator(globalrel)]

@test all([begin
    f = randformula(i%5, _alphabet, _operators)
    s = syntaxstring(f)
    s == syntaxstring(parseformulatree(s))
end
 for i in 1:1000])

@test all([begin
    f = randformulatree(i%5, _alphabet, vcat(_operators, TERNOP, QUATERNOP))
    s = syntaxstring(f)
    s == syntaxstring(parseformulatree(s))
end
for i in 1:1000])

@test all([begin
    f = randformula(i%5, _alphabet, _operators)
    s = syntaxstring(f; function_notation = true)
    s == syntaxstring(parseformulatree(s; function_notation = true);
        function_notation = true)
end for i in 1:1000])

end
