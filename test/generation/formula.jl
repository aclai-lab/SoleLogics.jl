using StatsBase
using SoleLogics: parsebaseformula, @__rng_dispatch
using Random

import SoleLogics: arity, syntaxstring

# @testset "randformula + randbaseformula" begin

_alphabet = ExplicitAlphabet(["p", "q", "r", "s"])
_operators = [NEGATION, CONJUNCTION, IMPLICATION]
w = [10,1,1]

@test_nowarn [randbaseformula(i, _alphabet, _operators) for i in 1:5]
@test_nowarn [randbaseformula(i, _alphabet, _operators, opweights=w) for i in 1:2]
@test_nowarn [randformula(i, _alphabet, _operators, opweights=w) for i in 1:5]

# end # endof test set



# @testset "generation w. custom operators" begin

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

# end # endof test set



# @testset "Dispatches made with @__rng_dispatch" begin

my_alph = ExplicitAlphabet(1:5)
my_ops = [∧,¬]
my_grammar = SoleLogics.CompleteFlatGrammar(my_alph, my_ops)
my_logic = propositionallogic(alphabet=my_alph)

@test_nowarn randatom(my_alph)
@test randatom(42, my_alph) == Atom(4)

non_finite_alph = AlphabetOfAny{Atom{String}}()
@test_throws Exception randatom(42, non_finite_alph)

alph2 = ExplicitAlphabet(6:10)
unionalph = UnionAlphabet([my_alph,alph2])
@test_nowarn randatom(unionalph)
@test randatom(42, unionalph) == Atom(6)

_subalphabets_weights_test_dim = 100
@test count(x -> x<5,
    atom.([randatom(unionalph; atompicking_mode=:weighted, subalphabets_weights=[5,1])
        for i in 1:_subalphabets_weights_test_dim])
    ) > convert(Int32, (_subalphabets_weights_test_dim/2))

@test_throws UndefVarError randatom(unionalph; atompicking_mode=:invalid_mode)
@test_throws ArgumentError randatom(unionalph; atompicking_mode=:weighted)
@test_throws ArgumentError randatom(
    unionalph; atompicking_mode=:weighted, subalphabets_weights=[1])
@test_nowarn randatom(unionalph; atompicking_mode=:uniform_subalphabets)

@test_nowarn rand(my_alph)
@test_nowarn rand(42, my_alph) == Atom(4)

@test_nowarn rand(4)
@test rand(MersenneTwister(42), 2, my_logic) |> syntaxstring == "(1 ∧ 5) ∨ ¬2"

@test_nowarn rand(4, my_grammar)
@test_nowarn rand(Random.MersenneTwister(1), 4, my_grammar)

@test_nowarn rand(Random.MersenneTwister(42), 4, my_alph, [CONJUNCTION, DISJUNCTION])
@test_throws ArgumentError rand(
    Random.MersenneTwister(42), 4, my_alph, [TOP]; truthvalues=[TOP])

# Testing rand edge case: truth values common ancestor is Truth;
# here, we make a custom Truth
struct MyTruth <: Truth
    val::Integer
end
MyTruthTOP = MyTruth(5);
syntaxstring(mt::MyTruth) = mt.val
# Base.promote_rule(::Type{<:BooleanTruth}, ::Type{<:MyTruth}) = Truth

@test_throws ArgumentError rand(
    42, 4, my_alph, [CONJUNCTION]; truthvalues=Truth[TOP,MyTruthTOP]);

@test_nowarn sample(my_alph, Weights([1,2,3,4,5]))
@test sample(2, my_alph, Weights([1,2,3,4,5])) == Atom(3)
@test_throws Exception sample(2, non_finite_alph, Weights([1,2,3,4,5]))

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
@test_throws ArgumentError randformula(4, my_alph, my_ops; atompicker=[1,2])
@test_throws ArgumentError randformula(4, non_finite_alph, my_ops)

@test_nowarn randformula(MersenneTwister(1), 4, my_alph, my_ops; atompicker = 1:5)
@test_throws ArgumentError randformula(
    MersenneTwister(1), 4, my_alph, my_ops; atompicker = 1:6)
@test_nowarn randformula(MersenneTwister(1), 4, my_alph, my_ops; atompicker = 1:5)

@test_nowarn randbaseformula(2, my_alph, my_ops)
@test_nowarn randbaseformula(MersenneTwister(42), 2, my_alph, my_ops)

@test_nowarn randbaseformula(4, my_grammar)
@test_nowarn randbaseformula(MersenneTwister(42), 4, my_grammar)

# end # endof test set



# @testset "@__rng_dispatch" begin

@test_throws LoadError @eval @__rng_dispatch 1+1
@test_throws LoadError @eval @__rng_dispatch function foo() end

# end # endof test set
