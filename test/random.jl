using StatsBase
import SoleLogics: arity

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ random logic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@testset "Random" begin

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION]
w = [10,1,1]

@test_nowarn [randbaseformula(i, _alphabet, _operators) for i in 1:15]
@test_nowarn [randbaseformula(i, _alphabet, _operators, opweights=w) for i in 1:2]
@test_nowarn [randformula(i, _alphabet, _operators, opweights=w) for i in 1:10]

end

@testset "Random+Parsing" begin

TERNOP = SoleLogics.NamedOperator{:⇶}()
SoleLogics.arity(::Type{typeof(TERNOP)}) = 3

QUATERNOP = SoleLogics.NamedOperator{:⩰}()
SoleLogics.arity(::Type{typeof(QUATERNOP)}) = 4

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION,
    DiamondRelationalOperator(globalrel), BoxRelationalOperator(globalrel)]
w = [5,1,1,1,1,1,1]

@test all([begin
        f = randbaseformula(3, _alphabet, _operators)
        s = syntaxstring(f)
        s == syntaxstring(parsetree(s))
    end for i in 1:1000])

@test all([begin
        f = randformula(i%5, _alphabet, [_operators..., TERNOP, QUATERNOP])
        # "function_notation = true" is essential in each parsing and string conversion
        # to represent ternary operators (or generally operators whose arity is > 2).
        s = syntaxstring(f; function_notation = true)
        s == syntaxstring(
                parseformula(
                    s, [_operators..., TERNOP, QUATERNOP]; function_notation = true),
                function_notation = true)
    end for i in 1:1000])

@test all([begin
        f = randbaseformula(i%5, _alphabet, _operators)
        s = syntaxstring(f; function_notation = true)
        s == syntaxstring(parsebaseformula(s; function_notation = true);
            function_notation = true)
    end for i in 1:1000])

end

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ random interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

alph = ExplicitAlphabet(1:5)
g = SoleLogics.CompleteFlatGrammar(alph, [∧,¬])

@test_nowarn Base.rand(4, g)
@test_nowarn Base.rand(Random.MersenneTwister(1), 4, g)
@test_nowarn randbaseformula(4, g)
@test_nowarn randbaseformula(4, g; rng = Random.MersenneTwister(1))

@test_nowarn StatsBase.sample(4, g)
@test_nowarn StatsBase.sample(Random.MersenneTwister(1), 4, g)
