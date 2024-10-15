using StatsBase
import SoleLogics: arity
using SoleLogics: parsebaseformula
using Random

@testset "randformula + randbaseformula" begin

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION]
w = [10,1,1]

@test_nowarn [randbaseformula(i, _alphabet, _operators) for i in 1:5]
@test_nowarn [randbaseformula(i, _alphabet, _operators, opweights=w) for i in 1:2]
@test_nowarn [randformula(i, _alphabet, _operators, opweights=w) for i in 1:5]

end



@testset "generation w. custom operators" begin

TERNOP = SoleLogics.NamedConnective{:⇶}()
SoleLogics.arity(::typeof(TERNOP)) = 3

QUATERNOP = SoleLogics.NamedConnective{:⩰}()
SoleLogics.arity(::typeof(QUATERNOP)) = 4

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION,
    DiamondRelationalConnective(globalrel), BoxRelationalConnective(globalrel)]
w = [5,1,1,1,1,1,1]

@test all([begin
        f = randbaseformula(3, _alphabet, _operators)
        s = syntaxstring(f)
        s == syntaxstring(parseformula(s))
    end for i in 1:10])

@test all([begin
        f = randformula(i%5, _alphabet, [_operators..., TERNOP, QUATERNOP])
        s = syntaxstring(f; function_notation = true)
        s == syntaxstring(
                parseformula(
                    s, [_operators..., TERNOP, QUATERNOP]; function_notation = true),
                function_notation = true)
    end for i in 1:10])

@test all([begin
        f = randbaseformula(i%5, _alphabet, _operators)
        s = syntaxstring(f; function_notation = true)
        s == syntaxstring(parsebaseformula(s; function_notation = true);
            function_notation = true)
    end for i in 1:10])

end



@testset "Dispatches made with @__rng_dispatch" begin

my_alph = ExplicitAlphabet(1:5)
my_ops = [∧,¬]
my_grammar = SoleLogics.CompleteFlatGrammar(my_alph, my_ops)
my_logic = propositionallogic(alphabet=my_alph)


@test_nowarn randatom(my_alph)
@test randatom(42, my_alph) == Atom(4)

alph2 = ExplicitAlphabet(6:10)
unionalph = UnionAlphabet([my_alph,alph2])
@test_nowarn randatom(unionalph)
@test randatom(42, unionalph) == Atom(6)

_subalphabets_weights_test_dim = 100
@test count(x -> x<5,
    atom.([randatom(unionalph; atompicking_mode=:weighted, subalphabets_weights=[5,1])
        for i in 1:_subalphabets_weights_test_dim])
    ) > convert(Int32, (_subalphabets_weights_test_dim/2))

@test_nowarn rand(my_alph)
@test_nowarn rand(42, my_alph) == Atom(4)

@test_nowarn rand(4)
@test rand(MersenneTwister(42), 2, my_logic) |> syntaxstring == "(1 ∧ 5) ∨ ¬2"


@test_nowarn rand(4, my_grammar)
@test_nowarn rand(Random.MersenneTwister(1), 4, my_grammar)

@test_nowarn sample(my_alph, Weights([1,2,3,4,5]))
@test sample(2, my_alph, Weights([1,2,3,4,5])) == Atom(3)

@test_nowarn StatsBase.sample(2, my_logic, Weights([1,2,3,4,5]), Weights([1,2]))
@test StatsBase.sample(
        MersenneTwister(42),
        2,
        my_logic,
        Weights([1,2,3,4,5]),
        Weights([1,2])
    ) |> syntaxstring == "(1 ∧ 1) ∨ (5 → 2)"

@test StatsBase.sample(MersenneTwister(42), 2, my_grammar) |> syntaxstring == "¬(1 ∧ 1)"
@test_nowarn StatsBase.sample(2, my_grammar)

@test_nowarn randformula(4, my_grammar)
@test_nowarn randformula(MersenneTwister(1), 4, my_grammar)
@test_nowarn randformula(4, my_alph, my_ops)
@test_nowarn randformula(MersenneTwister(1), 4, my_alph, my_ops; atompicker = 1:5)
@test_throws ArgumentError randformula(
    MersenneTwister(1), 4, my_alph, my_ops; atompicker = 1:6)

@test_nowarn randbaseformula(2, my_alph, my_ops)
@test_nowarn randbaseformula(MersenneTwister(42), 2, my_alph, my_ops)

@test_nowarn randbaseformula(4, my_grammar)
@test_nowarn randbaseformula(MersenneTwister(42), 4, my_grammar)
end
