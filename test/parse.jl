import SoleLogics: arity

using SoleLogics: parsebaseformula, relation

# testing utilities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function test_parsing_equivalence(f::SyntaxBranch)
    @test syntaxstring(f) == syntaxstring(parseformula(syntaxstring(f)))
    @test syntaxstring(f; function_notation = true) ==
        syntaxstring(
            parseformula(
                syntaxstring(f; function_notation = true);
                function_notation = true
            );
            function_notation = true
        )
end

# simple tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_throws ErrorException parseformula("")
@test_nowarn parseformula("p")
@test_nowarn parseformula("⊤")

@test parseformula("¬p∧q") == parseformula("¬(p)∧q")
@test parseformula("¬p∧q") != parseformula("¬(p∧q)")

@test_nowarn parsebaseformula("p")

@test_nowarn ¬ parsebaseformula("p")
@test_nowarn ¬ parseformula("p")
@test_nowarn ¬ parseformula("(s∧z)", propositionallogic())
@test_nowarn ¬ parsebaseformula("p", propositionallogic())

@test_nowarn parseformula("¬p∧q∧(¬s∧¬z)", [NEGATION, CONJUNCTION])
@test_nowarn parseformula("¬p∧q∧(¬s∧¬z)", [NEGATION])
@test_nowarn parseformula("¬p∧q∧{¬s∧¬z}",
    opening_parenthesis="{", closing_parenthesis="}")
@test_nowarn parseformula("¬p∧q∧ A ¬s∧¬z    B",
    opening_parenthesis="A", closing_parenthesis="B")

@test operatorstype(
        logic(parsebaseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BaseModalOperators
@test !(operatorstype(
    logic(parsebaseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BasePropositionalOperators)
@test !(operatorstype(logic(
    parsebaseformula("¬p∧q∧(¬s∧¬z)", modallogic()))) <: SoleLogics.BasePropositionalOperators)
@test (@test_nowarn operatorstype(
    logic(parsebaseformula("¬p∧q∧(¬s∧¬z)"))) <: SoleLogics.BasePropositionalOperators)

@test_nowarn parseformula("¬p∧q→(¬s∧¬z)")

@test syntaxstring(parseformula("⟨G⟩p")) == "⟨G⟩p"
@test syntaxstring(parseformula("⟨G⟩(p)"); remove_redundant_parentheses = false) == "⟨G⟩(p)"

@test syntaxstring(parseformula("[G]p")) == "[G]p"
@test syntaxstring(parseformula("[G]p"); remove_redundant_parentheses = false) == "[G](p)"

@test_nowarn parseformula("⟨G⟩p")

@test alphabet(logic(parsebaseformula("p→q"))) == AlphabetOfAny{String}()


# function notation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test syntaxstring(parseformula("p∧q"); function_notation = true) == "∧(p, q)"
@test syntaxstring(parseformula("p→q"); function_notation = true) == "→(p, q)"

@test filter(!isspace, syntaxstring(parseformula("¬p∧q∧(¬s∧¬z)");
    function_notation = true)) == "∧(¬(p),∧(q,∧(¬(s),¬(z))))"

@test_nowarn parseformula("→(∧(¬p, q), ∧(¬s, ¬z))", function_notation=true)
@test_nowarn parseformula("→(∧(¬p; q); ∧(¬s; ¬z))",
    function_notation=true, arg_delim = ";")
@test_nowarn parseformula("→{∧{¬p; q}; ∧{¬s; ¬z}}", function_notation=true,
    opening_parenthesis = "{", closing_parenthesis = "}",
    arg_delim = ";")


@test filter(!isspace, syntaxstring(parseformula("¬p∧q→(¬s∧¬z)");
    function_notation = true)) == "→(∧(¬(p),q),∧(¬(s),¬(z)))"
@test filter(!isspace, syntaxstring(
    parseformula("¬p∧q→A¬s∧¬zB",
        opening_parenthesis = "A",
        closing_parenthesis = "B");
    function_notation = true)) == "→(∧(¬(p),q),∧(¬(s),¬(z)))"
@test_nowarn parseformula("¬p∧q→     (¬s∧¬z)")
@test parseformula("□p∧   q∧(□s∧◊z)", [BOX]) == parseformula("□p∧   q∧(□s∧◊z)")
@test syntaxstring(parseformula("◊ ◊ ◊ ◊ p∧q"); function_notation = true) == "∧(◊(◊(◊(◊(p)))), q)"
@test syntaxstring(parseformula("¬¬¬ □□□ ◊◊◊ p ∧ ¬¬¬ q"); function_notation = true) ==
    "∧(¬(¬(¬(□(□(□(◊(◊(◊(p))))))))), ¬(¬(¬(q))))"

@test token(parseformula("¬¬¬ □□□ ◊◊◊ p ∧ ¬¬¬ q")) == ∧
@test token(parseformula("¬¬¬ □□□ ◊◊◊ p → ¬¬¬ q")) == →

fxs = [
    "(¬(¬(⟨G⟩(q))) → (([G](p)) ∧ ([G](q))))", #¬((¬(⟨G⟩(q))) → (([G](p)) ∧ ([G](q))))
    "((¬(q ∧ q)) ∧ ((p ∧ p) ∧ (q → q))) → ([G]([G](⟨G⟩(p))))",
    "((⟨G⟩(⟨G⟩(q))) ∧ (¬([G](p)))) → (((q → p) → (¬(q))) ∧ (¬([G](q))))",
    "[G](¬(⟨G⟩(p ∧ q)))",
    "⟨G⟩(((¬(⟨G⟩((q ∧ p) → (¬(q))))) ∧ (((¬(q → q)) → ((q → p) → (¬(q))))" *
    "∧ (((¬(p)) ∧ (⟨G⟩(p))) → (¬(⟨G⟩(q)))))) ∧ ((¬(([G](p ∧ q)) → (¬(p → q)))) →" *
    "([G](([G](q∧ q)) ∧ ([G](q → p))))))"
]
[test_parsing_equivalence(parseformula(f)) for f in fxs]

fxs = ["→(→(q, p), ¬q)", "∧(∧(q, p), ¬q)"]
[test_parsing_equivalence(parseformula(f, function_notation = true)) for f in fxs ]

# malformed input ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_throws ErrorException parseformula("")
@test_throws ErrorException parseformula("¬p◊")
@test_throws ErrorException parseformula("¬p◊q")
@test_throws ErrorException parseformula("◊¬p◊")
@test_throws ErrorException parseformula("◊¬p◊q")
@test_throws ErrorException parseformula("(p∧q", [NEGATION, CONJUNCTION])
@test_throws ErrorException parseformula("))))", [CONJUNCTION])
@test_throws ErrorException parseformula("⟨G⟩p ¬⟨G⟩q")
@test_throws ErrorException parseformula("¬[[G]]p")

@test_throws ErrorException parseformula(""; function_notation = true)
@test_throws ErrorException parseformula("¬p◊"; function_notation = true)
@test_throws ErrorException parseformula("¬p◊q"; function_notation = true)
@test_throws ErrorException parseformula("◊¬p◊"; function_notation = true)
@test_throws ErrorException parseformula("◊¬p◊q"; function_notation = true)
@test_throws ErrorException parseformula("(p∧q", [NEGATION, CONJUNCTION];
    function_notation = true)
@test_throws ErrorException parseformula("))))", [CONJUNCTION];
    function_notation = true)
@test_throws ErrorException parseformula("¬[[G]]p"; function_notation = true)

@test_throws ErrorException parseformula("¬p∧q∧(¬s∧¬z)", opening_parenthesis="{")
@test_throws ErrorException parseformula("¬p∧q∧{¬s∧¬z)",
    opening_parenthesis="{", closing_parenthesis="}")
@test_throws ErrorException parseformula("¬p∧q∧ C ¬s∧¬z    B",
    opening_parenthesis="A", closing_parenthesis="B")

@test_throws ErrorException parseformula("¬p∧q→ |¬s∧¬z|",
    opening_parenthesis = "|", closing_parenthesis = "|")

# parsing atoms ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_nowarn parseformula("¬1→0";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))))
@test_nowarn parseformula("¬0.42∧1";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))))
@test_nowarn parseformula("¬-96";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))))

@test_nowarn parseformula("→(¬1,0)";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))),
    function_notation = true)
@test_nowarn parseformula("→(¬1;0)";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))),
    function_notation = true, arg_delim = ";")
@test_nowarn parseformula("→(¬1/0)";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))),
    function_notation = true, arg_delim = "/")
@test_nowarn parseformula("∧(¬0.42,1)";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))),
    function_notation = true)
@test_nowarn parseformula("¬-96";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))),
    function_notation = true)

@test_throws ErrorException parseformula("[G][G]-1.2[G]";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))))
@test_throws ErrorException parseformula("¬-3(";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))))

@test_throws ArgumentError parseformula("p";
    atom_parser = (x -> Atom{Float64}(parse(Float64, x))))

@test_nowarn parseformula("10 ∧ ⟨G⟩ 2 ∧ [=] -1", Connective[];
    atom_parser = x->(Atom{Int64}(parse(Int, x))))
@test_nowarn parseformula("10 ∧ ⟨G⟩ 2 ∧ [=] -1";
    atom_parser = x->(Atom{Int64}(parse(Int, x))))
@test_nowarn parseformula("10 ∧ ⟨G⟩ 2 ∧ [=] -1", Connective[];
    atom_parser = x->(Atom{Int64}(parse(Int, x))))
@test_nowarn parseformula("10 ∧ ⟨G⟩ 2 ∧ [=] -1";
    atom_parser = x->(Atom{Int64}(parse(Int, x))))


# custom operators ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TERNOP = SoleLogics.NamedConnective{:⇶}()
SoleLogics.arity(::typeof(TERNOP)) = 3

QUATERNOP = SoleLogics.NamedConnective{:⩰}()
SoleLogics.arity(::typeof(QUATERNOP)) = 4

@test_nowarn parseformula("⇶(p, q, r)", [TERNOP]; function_notation=true)
@test_nowarn parseformula("⇶(p1, q1, ⇶(p2, q2, r2))", [TERNOP]; function_notation=true)

@test_nowarn parseformula("⩰(p, q, r, s)", [QUATERNOP]; function_notation=true)
@test_nowarn parseformula("⩰(p1, q1, r1, ⩰(p2, q2, r2, s2))",
    [QUATERNOP]; function_notation=true)

# custom relations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

struct _TestRel <: AbstractRelation end;
testrel  = _TestRel();
SoleLogics.arity(::_TestRel) = 2
SoleLogics.syntaxstring(::_TestRel; kwargs...) = "Test,Relation"

# If AbstractRelationalOperator interface changes, just redefine the following:
struct SoleRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(SoleRelationalOperator)(r::AbstractRelation) = SoleRelationalOperator{typeof(r)}()
SoleLogics.syntaxstring(op::SoleRelationalOperator; kwargs...) =
    "🌅$(syntaxstring(relation(op);  kwargs...))🌄"

struct PipeRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(PipeRelationalOperator)(r::AbstractRelation) = PipeRelationalOperator{typeof(r)}()
SoleLogics.syntaxstring(op::PipeRelationalOperator; kwargs...) =
    "|$(syntaxstring(relation(op);  kwargs...))|"

struct CurlyRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(CurlyRelationalOperator)(r::AbstractRelation) = CurlyRelationalOperator{typeof(r)}()
SoleLogics.syntaxstring(op::CurlyRelationalOperator; kwargs...) =
    "{$(syntaxstring(relation(op);  kwargs...))}"

struct MyCustomRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(MyCustomRelationalOperator)(r::AbstractRelation) = MyCustomRelationalOperator{typeof(r)}()
SoleLogics.syntaxstring(op::MyCustomRelationalOperator; kwargs...) =
    "LEFT CUSTOM PARENTHESIS $(syntaxstring(relation(op);  kwargs...)) RIGHT CUSTOM PARENTHESIS"
f = parseformula("LEFT CUSTOM PARENTHESIS G RIGHT CUSTOM PARENTHESIS p ∧ ¬" *
    "LEFT CUSTOM PARENTHESIS G RIGHT CUSTOM PARENTHESIS q", [MyCustomRelationalOperator(globalrel)])

@test_nowarn parseformula("🌅G🌄p ∧ ¬🌅G🌄q", [SoleRelationalOperator(globalrel)])
@test_nowarn parseformula("∧(🌅G🌄p,¬🌅G🌄q)", [SoleRelationalOperator(globalrel)];
    function_notation = true)
@test_nowarn parseformula("∧[🌅G🌄p DELIM ¬🌅G🌄q)", [SoleRelationalOperator(globalrel)];
    function_notation = true, opening_parenthesis = "[", arg_delim = "DELIM")

@test_nowarn parseformula("|G|p   ∧ ¬|G|q", [PipeRelationalOperator(globalrel)])
@test_nowarn parseformula("∧(|G|p,  ¬|G|q)", [PipeRelationalOperator(globalrel)];
    function_notation = true)

@test_nowarn parseformula("{G}p   ∧  ¬{G}q", [CurlyRelationalOperator(globalrel)])
@test_nowarn parseformula("∧({G}p   ,¬{G}q)", [CurlyRelationalOperator(globalrel)];
    function_notation = true)

_f = parseformula("|G|p ∧ ¬{G}q", [CurlyRelationalOperator(globalrel)])
@test syntaxstring(token(children(_f)[1])) == "|G|p" # PipeRelationalOperator not specified
_f = parseformula("∧(|G|p,¬{G}q)", [CurlyRelationalOperator(globalrel)];
    function_notation = true)
@test syntaxstring(token(children(_f)[1])) == "|G|p"

_f = parseformula("{Gp ∧ ¬{G}q", [CurlyRelationalOperator(globalrel)])
@test syntaxstring(token(children(_f)[1])) == "{Gp"

@test_nowarn parseformula("¬⟨Test,Relation⟩[Test,Relation]p",
    [BoxRelationalOperator(testrel), DiamondRelationalOperator(testrel)]
)

# parsebaseformula ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_throws ErrorException parsebaseformula("")
@test_nowarn parsebaseformula("⊤")
@test_nowarn parsebaseformula("⊤ ∧ ⊤")
@test_nowarn parsebaseformula("⊤ ∧ p")
@test_nowarn parsebaseformula("⊥ ∧ □¬((p∧¬q)→r)")
@test_nowarn parsebaseformula("□¬((p∧¬q)→r) ∧ ⊤")
@test_nowarn parsebaseformula("⊤ ∧ (⊥∧¬⊤→⊤)")

# stress test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

s = "¬((¬(([G](⟨G⟩(¬((¬([G](⟨G⟩(⟨G⟩(q))))) → (¬(⟨G⟩((¬(q)) ∧ ([G](p))))))))) ∧ (⟨G⟩((" *
    "[G](⟨G⟩([G](⟨G⟩(⟨G⟩(q ∧ q)))))) ∧ (¬(⟨G⟩((([G](⟨G⟩(p))) ∧ (⟨G⟩(⟨G⟩(p)))) ∧ (⟨G⟩(" *
    "[G](p → p)))))))))) ∧ (([G](([G]([G](¬((((¬(p)) → (⟨G⟩(q))) → ((⟨G⟩(p)) → (q → p" *
    "))) ∧ (⟨G⟩(¬([G](p)))))))) ∧ ([G](⟨G⟩([G](¬([G]([G](q ∧ p))))))))) ∧ (¬([G]((⟨G⟩" *
    "(⟨G⟩(¬(((⟨G⟩(q)) ∧ (⟨G⟩(q))) → (⟨G⟩(q → p)))))) ∧ ([G](¬(((¬(¬(q))) → (¬(q → p))" *
    ") ∧ (([G](p → p)) → ((⟨G⟩(p)) → (q → p)))))))))))"
f = parseformula(s)
@test syntaxstring(f) == syntaxstring(parseformula(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(
        parseformula(
            syntaxstring(f; function_notation = true); function_notation = true
        );
        function_notation = true
    )

s = "◊((¬((◊(◊(((¬(¬(q))) ∧ ((p ∧ p) ∨ (¬(p)))) → (¬(□(¬(q))))))) ∨ ((□(((□(◊(q))) →"  *
    "((p → q) ∨ (□(q)))) → (◊(□(◊(p)))))) ∨ ((((□(q ∨ p)) → (◊(¬(q)))) → (((p ∨ q) →"  *
    "(◊(q))) ∧ ((q ∨ p) ∧ (◊(q))))) ∧ ((¬((◊(p)) ∨ (¬(p)))) ∧ (□(◊(q ∧ p)))))))) → ((" *
    "◊(¬((□((◊(q → q)) ∨ (□(□(p))))) ∧ (¬((¬(◊(p))) ∨ ((◊(q)) ∨ (□(q)))))))) → ((¬((¬" *
    "(◊((q ∨ q) ∨ (□(q))))) → (((¬(□(q))) ∨ (□(◊(q)))) → (((◊(p)) ∧ (◊(q))) ∨ (¬(q ∧"  *
    "q)))))) → ((□(◊(¬(◊(¬(p)))))) ∨ ((□(□((q → p) ∧ (p ∧ p)))) ∨ (((◊(◊(p))) → ((p →" *
    "q) ∧ (p → q))) ∧ (□((p ∨ q) ∧ (◊(q))))))))))"
f = parseformula(s)
@test syntaxstring(f) == syntaxstring(parseformula(syntaxstring(f)))
@test syntaxstring(f; function_notation = true) ==
    syntaxstring(
        parseformula(
            syntaxstring(f; function_notation = true); function_notation = true
        );
        function_notation = true
    )

# If commenting the while !isempty(tokstack) ... end block, it works
@test_broken parseformula("10 ∧ ⟨G⟩ 2 ∧ [=] -1"; atom_parser = x->(Atom{Int64}(parse(Int, x))))
