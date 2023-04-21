
@testset "Parsing" begin

@test_nowarn parseformulatree("p")
@test_nowarn parseformulatree("⊤")

@test parseformulatree("¬p∧q") == parseformulatree("¬(p)∧q")
@test parseformulatree("¬p∧q") != parseformulatree("¬(p∧q)")

@test_nowarn parseformula("p")

@test_nowarn ¬ parseformula("p")
@test_nowarn ¬ parseformulatree("p")
@test_nowarn ¬ parseformulatree("(s∧z)", propositionallogic())
@test_nowarn ¬ parseformula("p", propositionallogic())

@test_nowarn parseformulatree("¬p∧q∧(¬s∧¬z)", [NEGATION, CONJUNCTION])
@test_nowarn parseformulatree("¬p∧q∧(¬s∧¬z)", [NEGATION])
@test_nowarn parseformulatree("¬p∧q∧{¬s∧¬z}",
    opening_bracket=Symbol("{"), closing_bracket=Symbol("}"))
@test_nowarn parseformulatree("¬p∧q∧ A ¬s∧¬z    B",
    opening_bracket=Symbol("A"), closing_bracket=Symbol("B"))

@test operatorstype(
        logic(parseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BaseModalOperators
@test !(operatorstype(
    logic(parseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BasePropositionalOperators)
@test !(operatorstype(logic(
    parseformula("¬p∧q∧(¬s∧¬z)", modallogic()))) <: SoleLogics.BasePropositionalOperators)
@test (@test_nowarn operatorstype(
    logic(parseformula("¬p∧q∧(¬s∧¬z)"))) <: SoleLogics.BasePropositionalOperators)

@test_nowarn parseformulatree("¬p∧q→(¬s∧¬z)")

@test syntaxstring(parseformulatree("⟨G⟩p")) == "⟨G⟩(p)"
@test syntaxstring(parseformulatree("[G]p")) == "[G](p)"

@test_nowarn parseformulatree("⟨G⟩p", [DiamondRelationalOperator{GlobalRel}()])

@test alphabet(logic(parseformula("p→q"))) == AlphabetOfAny{String}()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ function notation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test syntaxstring(parseformulatree("p∧q"); function_notation = true) == "∧(p, q)"
@test syntaxstring(parseformulatree("p→q"); function_notation = true) == "→(p, q)"

@test filter(!isspace, syntaxstring(parseformulatree("¬p∧q∧(¬s∧¬z)");
    function_notation = true)) == "∧(¬(p),∧(q,∧(¬(s),¬(z))))"

@test_nowarn parseformulatree("→(∧(¬p, q), ∧(¬s, ¬z))", function_notation=true)
@test_nowarn parseformulatree("→(∧(¬p; q); ∧(¬s; ¬z))",
    function_notation=true, arg_separator = Symbol(";"))
@test_nowarn parseformulatree("→{∧{¬p; q}; ∧{¬s; ¬z}}", function_notation=true,
    opening_bracket = Symbol("{"), closing_bracket = Symbol("}"),
    arg_separator = Symbol(";"))


@test filter(!isspace, syntaxstring(parseformulatree("¬p∧q→(¬s∧¬z)");
    function_notation = true)) == "→(∧(¬(p),q),∧(¬(s),¬(z)))"
@test filter(!isspace, syntaxstring(
    parseformulatree("¬p∧q→A¬s∧¬zB",
        opening_bracket = Symbol("A"),
        closing_bracket = Symbol("B"));
    function_notation = true)) == "→(∧(¬(p),q),∧(¬(s),¬(z)))"
@test_nowarn parseformulatree("¬p∧q→     (¬s∧¬z)")
@test parseformulatree("□p∧   q∧(□s∧◊z)", [BOX]) == parseformulatree("□p∧   q∧(□s∧◊z)")
@test syntaxstring(parseformulatree("◊ ◊ ◊ ◊ p∧q"); function_notation = true) ==
    "∧(◊(◊(◊(◊(p)))), q)"
@test syntaxstring(parseformulatree("¬¬¬ □□□ ◊◊◊ p ∧ ¬¬¬ q"); function_notation = true) ==
    "∧(¬(¬(¬(□(□(□(◊(◊(◊(p))))))))), ¬(¬(¬(q))))"

f = parseformulatree("¬((¬(⟨G⟩(q))) → (([G](p)) ∧ ([G](q))))",
    [BoxRelationalOperator(globalrel), DiamondRelationalOperator(globalrel)])
@test syntaxstring(f) == syntaxstring(parseformulatree(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(parseformulatree(syntaxstring(f)); function_notation = true)
f = parseformulatree("((¬(q ∧ q)) ∧ ((p ∧ p) ∧ (q → q))) → ([G]([G](⟨G⟩(p))))",
    [BoxRelationalOperator(globalrel), DiamondRelationalOperator(globalrel)])
@test syntaxstring(f) == syntaxstring(parseformulatree(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(parseformulatree(syntaxstring(f)); function_notation = true)
f = parseformulatree("((⟨G⟩(⟨G⟩(q))) ∧ (¬([G](p)))) → (((q → p) → (¬(q))) ∧ (¬([G](q))))",
    [BoxRelationalOperator(globalrel), DiamondRelationalOperator(globalrel)])
@test syntaxstring(f) == syntaxstring(parseformulatree(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(parseformulatree(syntaxstring(f)); function_notation = true)
f = parseformulatree("[G](¬(⟨G⟩(p ∧ q)))",
    [BoxRelationalOperator(globalrel), DiamondRelationalOperator(globalrel)])
@test syntaxstring(f) == syntaxstring(parseformulatree(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(parseformulatree(syntaxstring(f)); function_notation = true)

f = parseformulatree("⟨G⟩(((¬(⟨G⟩((q ∧ p) → (¬(q))))) ∧ (((¬(q → q)) → ((q → p) → (¬(q))))"*
    "∧ (((¬(p)) ∧ (⟨G⟩(p))) → (¬(⟨G⟩(q)))))) ∧ ((¬(([G](p ∧ q)) → (¬(p → q)))) →" *
    "([G](([G](q∧ q)) ∧ ([G](q → p))))))",
    [BoxRelationalOperator(globalrel), DiamondRelationalOperator(globalrel)])
@test syntaxstring(f) == syntaxstring(parseformulatree(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(parseformulatree(syntaxstring(f)); function_notation = true)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ malformed input ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_throws ErrorException parseformulatree("")
@test_throws ErrorException parseformulatree("¬p◊")
@test_throws ErrorException parseformulatree("¬p◊q")
@test_throws ErrorException parseformulatree("◊¬p◊")
@test_throws ErrorException parseformulatree("◊¬p◊q")
@test_throws ErrorException parseformulatree("(p∧q", [NEGATION, CONJUNCTION])
@test_throws ErrorException parseformulatree("))))", [CONJUNCTION])
@test_throws ErrorException parseformulatree("⟨G⟩p ¬⟨G⟩q",
    [DiamondRelationalOperator(globalrel)])
@test_throws ErrorException parseformulatree("¬[[G]]p", [BoxRelationalOperator(globalrel)])

@test_throws ErrorException parseformulatree(""; function_notation = true)
@test_throws ErrorException parseformulatree("¬p◊"; function_notation = true)
@test_throws ErrorException parseformulatree("¬p◊q"; function_notation = true)
@test_throws ErrorException parseformulatree("◊¬p◊"; function_notation = true)
@test_throws ErrorException parseformulatree("◊¬p◊q"; function_notation = true)
@test_throws ErrorException parseformulatree("(p∧q", [NEGATION, CONJUNCTION];
    function_notation = true)
@test_throws ErrorException parseformulatree("))))", [CONJUNCTION];
    function_notation = true)
@test_throws ErrorException parseformulatree("¬[[G]]p", [BoxRelationalOperator(globalrel)];
    function_notation = true)

@test_throws ErrorException parseformulatree("¬p∧q∧(¬s∧¬z)", opening_bracket=Symbol("{"))
@test_throws ErrorException parseformulatree("¬p∧q∧{¬s∧¬z)",
    opening_bracket=Symbol("{"), closing_bracket=Symbol("}"))
@test_throws ErrorException parseformulatree("¬p∧q∧ C ¬s∧¬z    B",
    opening_bracket=Symbol("A"), closing_bracket=Symbol("B"))

@test_throws ErrorException parseformulatree("¬p∧q→ |¬s∧¬z|",
    opening_bracket = Symbol("|"), closing_bracket = Symbol("|"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ parsing propositions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_nowarn parseformulatree("¬1→0";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))))
@test_nowarn parseformulatree("¬0.42∧1";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))))
@test_nowarn parseformulatree("¬-96";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))))

@test_nowarn parseformulatree("→(¬1,0)";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))),
    function_notation = true)
@test_nowarn parseformulatree("→(¬1;0)";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))),
    function_notation = true, arg_separator = Symbol(";"))
@test_nowarn parseformulatree("→(¬1/0)";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))),
    function_notation = true, arg_separator = Symbol("/"))
@test_nowarn parseformulatree("∧(¬0.42,1)";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))),
    function_notation = true)
@test_nowarn parseformulatree("¬-96";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))),
    function_notation = true)

@test_throws ErrorException parseformulatree("[G][G]-1.2[G]",
    [BoxRelationalOperator(globalrel)];
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))))
@test_throws ErrorException parseformulatree("¬-3(";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))))

@test_throws ArgumentError parseformulatree("p";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x))))

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
SoleLogics.syntaxstring(op::MyCustomRelationalOperator; kwargs...) =
    "LEFT CUSTOM BRACKET $(syntaxstring(relationtype(op);  kwargs...)) RIGHT CUSTOM BRACKET"
f = parseformulatree("LEFT CUSTOM BRACKET G RIGHT CUSTOM BRACKET p ∧ ¬" *
    "LEFT CUSTOM BRACKET G RIGHT CUSTOM BRACKET q", [MyCustomRelationalOperator(globalrel)])

@test_nowarn parseformulatree("🌅G🌄p ∧ ¬🌅G🌄q", [SoleRelationalOperator(globalrel)])
@test_nowarn parseformulatree("∧(🌅G🌄p,¬🌅G🌄q)", [SoleRelationalOperator(globalrel)];
    function_notation = true)
@test_nowarn parseformulatree("∧[🌅G🌄p SEP ¬🌅G🌄q)", [SoleRelationalOperator(globalrel)];
    function_notation = true, opening_bracket = Symbol("["), arg_separator = Symbol("SEP"))

@test_nowarn parseformulatree("|G|p   ∧ ¬|G|q", [PipeRelationalOperator(globalrel)])
@test_nowarn parseformulatree("∧(|G|p,  ¬|G|q)", [PipeRelationalOperator(globalrel)];
    function_notation = true)

@test_nowarn parseformulatree("{G}p   ∧  ¬{G}q", [CurlyRelationalOperator(globalrel)])
@test_nowarn parseformulatree("∧({G}p   ,¬{G}q)", [CurlyRelationalOperator(globalrel)];
    function_notation = true)

_f = parseformulatree("|G|p ∧ ¬{G}q", [CurlyRelationalOperator(globalrel)])
@test syntaxstring(token(children(_f)[1])) == "|G|p" # PipeRelationalOperator not specified
_f = parseformulatree("∧(|G|p,¬{G}q)", [CurlyRelationalOperator(globalrel)];
    function_notation = true)
@test syntaxstring(token(children(_f)[1])) == "|G|p"

_f = parseformulatree("{Gp ∧ ¬{G}q", [CurlyRelationalOperator(globalrel)])
@test syntaxstring(token(children(_f)[1])) == "{Gp"

@test_nowarn parseformulatree("¬⟨Test,Relation⟩[Test,Relation]p",
    [BoxRelationalOperator(testrel), DiamondRelationalOperator(testrel)]
)
end

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ stress tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

s = "¬((¬(([G](⟨G⟩(¬((¬([G](⟨G⟩(⟨G⟩(q))))) → (¬(⟨G⟩((¬(q)) ∧ ([G](p))))))))) ∧ (⟨G⟩((" *
    "[G](⟨G⟩([G](⟨G⟩(⟨G⟩(q ∧ q)))))) ∧ (¬(⟨G⟩((([G](⟨G⟩(p))) ∧ (⟨G⟩(⟨G⟩(p)))) ∧ (⟨G⟩(" *
    "[G](p → p)))))))))) ∧ (([G](([G]([G](¬((((¬(p)) → (⟨G⟩(q))) → ((⟨G⟩(p)) → (q → p" *
    "))) ∧ (⟨G⟩(¬([G](p)))))))) ∧ ([G](⟨G⟩([G](¬([G]([G](q ∧ p))))))))) ∧ (¬([G]((⟨G⟩" *
    "(⟨G⟩(¬(((⟨G⟩(q)) ∧ (⟨G⟩(q))) → (⟨G⟩(q → p)))))) ∧ ([G](¬(((¬(¬(q))) → (¬(q → p))" *
    ") ∧ (([G](p → p)) → ((⟨G⟩(p)) → (q → p)))))))))))"
f = parseformulatree(s,
    [BoxRelationalOperator(globalrel), DiamondRelationalOperator(globalrel)])
@test syntaxstring(f) == syntaxstring(parseformulatree(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(parseformulatree(syntaxstring(f)); function_notation = true)

s = "◊((¬((◊(◊(((¬(¬(q))) ∧ ((p ∧ p) ∨ (¬(p)))) → (¬(□(¬(q))))))) ∨ ((□(((□(◊(q))) →"  *
    "((p → q) ∨ (□(q)))) → (◊(□(◊(p)))))) ∨ ((((□(q ∨ p)) → (◊(¬(q)))) → (((p ∨ q) →"  *
    "(◊(q))) ∧ ((q ∨ p) ∧ (◊(q))))) ∧ ((¬((◊(p)) ∨ (¬(p)))) ∧ (□(◊(q ∧ p)))))))) → ((" *
    "◊(¬((□((◊(q → q)) ∨ (□(□(p))))) ∧ (¬((¬(◊(p))) ∨ ((◊(q)) ∨ (□(q)))))))) → ((¬((¬" *
    "(◊((q ∨ q) ∨ (□(q))))) → (((¬(□(q))) ∨ (□(◊(q)))) → (((◊(p)) ∧ (◊(q))) ∨ (¬(q ∧"  *
    "q)))))) → ((□(◊(¬(◊(¬(p)))))) ∨ ((□(□((q → p) ∧ (p ∧ p)))) ∨ (((◊(◊(p))) → ((p →" *
    "q) ∧ (p → q))) ∧ (□((p ∨ q) ∧ (◊(q))))))))))"
f = parseformulatree(s,
    [BoxRelationalOperator(globalrel), DiamondRelationalOperator(globalrel)])
@test syntaxstring(f) == syntaxstring(parseformulatree(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(parseformulatree(syntaxstring(f)); function_notation = true)

