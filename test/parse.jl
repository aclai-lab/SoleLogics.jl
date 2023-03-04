
@testset "Parsing" begin

@test_nowarn parseformulatree("p")
@test_nowarn parseformulatree("⊤")

@test syntaxstring(parseformulatree("p∧q"); function_notation = true) == "∧(p, q)"
@test syntaxstring(parseformulatree("p→q"); function_notation = true) == "→(p, q)"
@test parseformulatree("¬p∧q") == parseformulatree("¬(p)∧q")
@test parseformulatree("¬p∧q") != parseformulatree("¬(p∧q)")
@test filter(!isspace, syntaxstring(parseformulatree("¬p∧q∧(¬s∧¬z)"); function_notation = true)) == "∧(¬(p),∧(q,∧(¬(s),¬(z))))"


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

@test_nowarn parseformula("p")

end
