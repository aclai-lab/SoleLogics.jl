
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ random ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@testset "Random" begin

# TODO @Mauro: bring back and throw away output
# Mauro: I commented the following tests since a cryptic error message fills up the REPL.
# This is strange, also because `randformulatree` actually returns correct SyntaxTrees.
# _alphabet = ExplicitAlphabet(Proposition.([1,2]))
_alphabet = ExplicitAlphabet(Proposition.(["pr", "qt_aoeu"]))
_operators = [NEGATION, CONJUNCTION, IMPLICATION]
@test_broken randformulatree(10, _alphabet, _operators)
@test_nowarn randformulatree(2, _alphabet, _operators)

end


@testset "Random+Parsing" begin

const TERN = SoleLogics.NamedOperator{:TERN}()
import SoleLogics: arity
SoleLogics.arity(::Type{typeof(TERN)}) = 3

_operators = [_operators..., DiamondRelationalOperator(globalrel), BoxRelationalOperator(globalrel), TERN]
@test all([begin
    f = randformula(4, _alphabet, _operators; 1)
    s = syntaxstring(f)
    s == syntaxstring(parseformulatree(s))
end
 for i in 1:1000])

@test all([begin
    f = randformula(4, _alphabet, _operators; 1)
    s = syntaxstring(f)
    s == syntaxstring(parseformulatree(s; function_notation = true); function_notation = true)
end for i in 1:1000])


end
