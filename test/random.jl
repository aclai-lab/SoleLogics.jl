import SoleLogics: arity

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ random ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@testset "Random" begin

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION]

@test_nowarn [randformulatree(i, _alphabet, _operators) for i in 1:15]

end

@testset "Random+Parsing" begin

TERNOP = SoleLogics.NamedOperator{:⇶}()
SoleLogics.arity(::Type{typeof(TERNOP)}) = 3

QUATERNOP = SoleLogics.NamedOperator{:⩰}()
SoleLogics.arity(::Type{typeof(QUATERNOP)}) = 4

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION,
    DiamondRelationalOperator(globalrel), BoxRelationalOperator(globalrel)]

@test all([begin
        f = randformula(i%5, _alphabet, _operators)
        s = syntaxstring(f)
        s == syntaxstring(parseformulatree(s))
    end for i in 1:1000])

@test all([begin
        f = randformulatree(i%5, _alphabet, [_operators..., TERNOP, QUATERNOP])
        # "function_notation = true" is essential in each parsing and string conversion
        # to represent ternary operators (or generally operators whose arity is > 2).
        s = syntaxstring(f; function_notation = true)
        s == syntaxstring(
                parseformulatree(
                    s, [_operators..., TERNOP, QUATERNOP]; function_notation = true),
                function_notation = true)
    end for i in 1:1000])

@test all([begin
        f = randformula(i%5, _alphabet, _operators)
        s = syntaxstring(f; function_notation = true)
        s == syntaxstring(parseformulatree(s; function_notation = true);
            function_notation = true)
    end for i in 1:1000])

end
