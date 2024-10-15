using StatsBase
import SoleLogics: arity
using Random

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ random logic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@testset "Random" begin

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION]
w = [10,1,1]

@test_nowarn [randformula(i, _alphabet, _operators) for i in 1:15]
@test_nowarn [randformula(i, _alphabet, _operators, opweights=w) for i in 1:10]

end

@testset "Random+Parsing" begin

TERNOP = SoleLogics.NamedConnective{:⇶}()
SoleLogics.arity(::typeof(TERNOP)) = 3

QUATERNOP = SoleLogics.NamedConnective{:⩰}()
SoleLogics.arity(::typeof(QUATERNOP)) = 4

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION,
    DiamondRelationalConnective(globalrel), BoxRelationalConnective(globalrel)]
w = [5,1,1,1,1,1,1]

@test all([begin
        f = randformula(3, _alphabet, _operators)
        s = syntaxstring(f)
        s == syntaxstring(parseformula(s))
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

# @test all([begin
#         f = randformula(i%5, _alphabet, _operators)
#         s = syntaxstring(f; function_notation = true)
#         s == syntaxstring(parseformula(AnchoredFormula, s; function_notation = true);
#             function_notation = true)
#     end for i in 1:1000])

end

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ random interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

using Random

alph = ExplicitAlphabet(1:5)
ops = [∧,¬]
g = SoleLogics.CompleteFlatGrammar(alph, ops)

@test_nowarn Base.rand(4, g)
@test_nowarn Base.rand(Random.MersenneTwister(1), 4, g)
@test_nowarn randformula(4, g)
@test_nowarn randformula(4, g; rng = Random.MersenneTwister(1))

@test_nowarn StatsBase.sample(4, g)
@test_nowarn StatsBase.sample(Random.MersenneTwister(1), 4, g)

@test_nowarn randformula(4, g)
@test_nowarn randformula(Random.MersenneTwister(1), 4, g)
@test_nowarn randformula(4, alph, ops)
@test_nowarn randformula(Random.MersenneTwister(1), 4, alph, ops; atompicker = 1:5)
@test_throws AssertionError randformula(Random.MersenneTwister(1), 4, alph, ops; atompicker = 1:6)

@test_nowarn StatsBase.sample(4, g, Weights([1:natoms(alphabet(g))]...))
@test_nowarn StatsBase.sample(4, g, Weights([1,1,1,1,100]))
@test_throws MethodError StatsBase.sample(4, g, [1,1,1,1,100])
@test_throws MethodError StatsBase.sample(Random.MersenneTwister(1), 4, g, 1:2)
