
@testset "Parsing" begin

@test_nowarn parseformulatree("p")
@test_nowarn parseformulatree("⊤")

@test syntaxstring(parseformulatree("p∧q"); function_notation = true) == "∧(p, q)"
@test syntaxstring(parseformulatree("p→q"); function_notation = true) == "→(p, q)"
@test parseformulatree("¬p∧q") == parseformulatree("¬(p)∧q")
@test parseformulatree("¬p∧q") != parseformulatree("¬(p∧q)")
@test filter(!isspace, syntaxstring(parseformulatree("¬p∧q∧(¬s∧¬z)"); function_notation = true)) == "∧(¬(p),∧(q,∧(¬(s),¬(z))))"


@test_nowarn parseformula("p")

@test_nowarn ¬ parseformula("p")
@test_nowarn ¬ parseformulatree("p")
@test_nowarn ¬ parseformulatree("(s∧z)", propositionallogic())
@test_nowarn ¬ parseformula("p", propositionallogic())

@test_nowarn parseformulatree("¬p∧q∧(¬s∧¬z)", [NEGATION, CONJUNCTION])
@test_nowarn parseformulatree("¬p∧q∧(¬s∧¬z)", [NEGATION])
# @test ((@test_logs (:warn,) operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)", [BOX])))) == Union{typeof(□),typeof(¬),typeof(∧)})
@test operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BaseModalOperators
@test !(operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BasePropositionalOperators)
@test !(operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)", modallogic()))) <: SoleLogics.BasePropositionalOperators)
@test (@test_nowarn operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)"))) <: SoleLogics.BasePropositionalOperators)
@test_nowarn parseformulatree("¬p∧q→(¬s∧¬z)")
@test filter(!isspace, syntaxstring(parseformulatree("¬p∧q→(¬s∧¬z)"); function_notation = true)) == "→(∧(¬(p),q),∧(¬(s),¬(z)))"
@test_nowarn parseformulatree("¬p∧q→     (¬s∧¬z)")
@test parseformulatree("□p∧   q∧(□s∧◊z)", [BOX]) == parseformulatree("□p∧   q∧(□s∧◊z)")
@test syntaxstring(parseformulatree("◊ ◊ ◊ ◊ p∧q"); function_notation = true) == "∧(◊(◊(◊(◊(p)))), q)"
@test syntaxstring(parseformulatree("¬¬¬ □□□ ◊◊◊ p ∧ ¬¬¬ q"); function_notation = true) == "∧(¬(¬(¬(□(□(□(◊(◊(◊(p))))))))), ¬(¬(¬(q))))"

@test alphabet(logic(parseformula("p→q"))) == AlphabetOfAny{String}()

# Malformed input
# TODO fix @Mauro
# @test_throws ErrorException parseformulatree("¬p◊")
# @test_throws ErrorException parseformulatree("¬p◊q")
# @test_throws ErrorException parseformulatree("(p∧q", [NEGATION, CONJUNCTION])
# @test_throws ErrorException parseformulatree("))))", [CONJUNCTION])

# TODO
# @test ErrorException parseformulatree("⟨G⟩p", [DiamondRelationalOperator{GlobalRel}()])

# Mauro: I commented the following tests since a cryptic error message fills up the REPL.
# This is strange, also because `randformulatree` actually returns correct SyntaxTrees.
# TODO bring back
# _alphabet = ExplicitAlphabet(Proposition.(["pr", "qt_aoeu"]))
# _operators = [NEGATION, CONJUNCTION, IMPLICATION]
# # @test_broken randformulatree(10, _alphabet, _operators)
# # @test_nowarn randformulatree(2, _alphabet, _operators)

# const TERN = SoleLogics.NamedOperator{:TERN}()
# import SoleLogics: arity
# SoleLogics.arity(::Type{typeof(TERN)}) = 3

# _operators = [_operators..., DiamondRelationalOperator(globalrel), BoxRelationalOperator(globalrel), TERN]
# @test all([begin
#     f = randformula(4, _alphabet, _operators; 1)
#     s = syntaxstring(f)
#     s == syntaxstring(parseformulatree(s))
# end
#  for i in 1:1000])

# @test all([begin
#     f = randformula(4, _alphabet, _operators; 1)
#     s = syntaxstring(f)
#     s == syntaxstring(parseformulatree(s; function_notation = true); function_notation = true)
# end for i in 1:1000])

end
