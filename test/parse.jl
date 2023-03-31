
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
# 
@test operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BaseModalOperators
@test !(operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BasePropositionalOperators)
@test !(operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)", modallogic()))) <: SoleLogics.BasePropositionalOperators)
@test (@test_nowarn operatorstype(logic(parseformula("¬p∧q∧(¬s∧¬z)"))) <: SoleLogics.BasePropositionalOperators)
# 
@test_nowarn parseformulatree("¬p∧q→(¬s∧¬z)")
@test filter(!isspace, syntaxstring(parseformulatree("¬p∧q→(¬s∧¬z)"); function_notation = true)) == "→(∧(¬(p),q),∧(¬(s),¬(z)))"
@test_nowarn parseformulatree("¬p∧q→     (¬s∧¬z)")
@test parseformulatree("□p∧   q∧(□s∧◊z)", [BOX]) == parseformulatree("□p∧   q∧(□s∧◊z)")
@test syntaxstring(parseformulatree("◊ ◊ ◊ ◊ p∧q"); function_notation = true) == "∧(◊(◊(◊(◊(p)))), q)"
@test syntaxstring(parseformulatree("¬¬¬ □□□ ◊◊◊ p ∧ ¬¬¬ q"); function_notation = true) == "∧(¬(¬(¬(□(□(□(◊(◊(◊(p))))))))), ¬(¬(¬(q))))"

@test syntaxstring(parseformulatree("⟨G⟩p")) == "⟨G⟩(p)"
@test syntaxstring(parseformulatree("[G]p")) == "[G](p)"

@test_nowarn parseformulatree("⟨G⟩p", [DiamondRelationalOperator{GlobalRel}()])

@test alphabet(logic(parseformula("p→q"))) == AlphabetOfAny{String}()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ malformed input ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_throws ErrorException parseformulatree("")
@test_throws ErrorException parseformulatree("¬p◊")
@test_throws ErrorException parseformulatree("¬p◊q")
@test_throws ErrorException parseformulatree("◊¬p◊")
@test_throws ErrorException parseformulatree("◊¬p◊q")
@test_throws ErrorException parseformulatree("(p∧q", [NEGATION, CONJUNCTION])
@test_throws ErrorException parseformulatree("))))", [CONJUNCTION])
@test_throws ErrorException parseformulatree("⟨G⟩p ¬⟨G⟩q",
    [DiamondRelationalOperator(globalrel)]
    )
@test_throws ErrorException parseformulatree("¬[[G]]p", [BoxRelationalOperator(globalrel)])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ parsing propositions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_nowarn parseformulatree("¬1→0";
    proposition_parser=(x->Proposition{Float64}(parse(Float64, x)))
)
@test_nowarn parseformulatree("¬0.42∧1";
    proposition_parser=(x->Proposition{Float64}(parse(Float64, x)))
)
@test_nowarn parseformulatree("¬-96";
    proposition_parser=(x->Proposition{Float64}(parse(Float64, x)))
)

@test_throws ErrorException parseformulatree("[G][G]-1.2[G]",
    [BoxRelationalOperator(globalrel)];
    proposition_parser=(x->Proposition{Float64}(parse(Float64, x)))
)
@test_throws ErrorException parseformulatree("¬-3(";
    proposition_parser=(x->Proposition{Float64}(parse(Float64, x)))
)

@test_throws ArgumentError parseformulatree("p";
    proposition_parser=(x->Proposition{Float64}(parse(Float64, x)))
)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ custom operator ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


struct _TestRel <: AbstractRelation end;
testrel  = _TestRel();
SoleLogics.arity(::Type{_TestRel}) = 2
SoleLogics.syntaxstring(::Type{_TestRel}; kwargs...) = "Test,Relation"

# If AbstractRelationalOperator interface changes, just redefine the following:
struct SoleRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(SoleRelationalOperator)(r::AbstractRelation) = SoleRelationalOperator{typeof(r)}()
SoleLogics.syntaxstring(op::SoleRelationalOperator; kwargs...) =
    "🌅$(syntaxstring(relationtype(op);  kwargs...))🌄"

struct PipeRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(PipeRelationalOperator)(r::AbstractRelation) = PipeRelationalOperator{typeof(r)}()
SoleLogics.syntaxstring(op::PipeRelationalOperator; kwargs...) =
    "|$(syntaxstring(relationtype(op);  kwargs...))|"

struct CurlyRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(CurlyRelationalOperator)(r::AbstractRelation) = CurlyRelationalOperator{typeof(r)}()
SoleLogics.syntaxstring(op::CurlyRelationalOperator; kwargs...) =
    "{$(syntaxstring(relationtype(op);  kwargs...))}"

struct MyCustomRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(MyCustomRelationalOperator)(r::AbstractRelation) = MyCustomRelationalOperator{typeof(r)}()
SoleLogics.syntaxstring(op::MyCustomRelationalOperator; kwargs...) = "LEFT CUSTOM BRACKET $(syntaxstring(relationtype(op);  kwargs...)) RIGHT CUSTOM BRACKET"
f = parseformulatree("LEFT CUSTOM BRACKET G RIGHT CUSTOM BRACKET p ∧ ¬ LEFT CUSTOM BRACKET G RIGHT CUSTOM BRACKET q", [MyCustomRelationalOperator(globalrel)])




@test_nowarn parseformulatree("🌅G🌄p ∧ ¬🌅G🌄q", [SoleRelationalOperator(globalrel)])
@test_nowarn parseformulatree("|G|p ∧ ¬|G|q", [PipeRelationalOperator(globalrel)])
@test_nowarn parseformulatree("{G}p ∧ ¬{G}q", [CurlyRelationalOperator(globalrel)])

_f = parseformulatree("|G|p ∧ ¬{G}q", [CurlyRelationalOperator(globalrel)])
@test syntaxstring(token(children(_f)[1])) == "|G|p" # PipeRelationalOperator not specified

_f = parseformulatree("{Gp ∧ ¬{G}q", [CurlyRelationalOperator(globalrel)])
@test syntaxstring(token(children(_f)[1])) == "{Gp"

@test_nowarn parseformulatree("¬⟨Test,Relation⟩[Test,Relation]p",
    [BoxRelationalOperator(testrel), DiamondRelationalOperator(testrel)]
)

end
