using Test
using SoleLogics

const SL = SoleLogics # SL.name to reference unexported names

# The following test set is intended to test the new type hierarchy update,
# considering both trivial and complex assertions regarding various aspects of SoleLogics.

@testset "TypeHierarchyUpdate" begin

#=
    New syntactical type hierarchy

    Syntactical
    ├── AbstractFormula
    │   ├── AbstractSyntaxStructure
    │   │   ├── AbstractLeaf
    │   │   │   ├── Atom
    │   │   │   └── Truth
    │   │   │       ├── BooleanTruth
    │   │   │       │   ├── Top
    │   │   │       │   └── Bottom
    │   │   │       └── ...
    │   │   └── AbstractComposite
    │   │       ├── SyntaxTree
    │   │       ├── LeftmostLinearForm
    │   │       └── ...
    │   └── AbstractMemoFormula
    │       └── TruthTable
    └── Connective
        ├── NamedConnective
        ├── AbstractRelationalOperator
        ├── DiamondRelationalOperator
        ├── BoxRelationalOperator
        └── ...

    Also:
    const Operator = Union{Connective,Truth}
    const SyntaxToken = Union{Connective,AbstractLeaf}
    const BooleanTruth = Union{Top,Bottom}
=#

# Declaration section

p = Atom("p")
q = Atom("q")
r = Atom("r")
m = Atom(1)
n = Atom(2)

pandq               = SyntaxTree(CONJUNCTION, (p,q))
pandq_demorgan      = DISJUNCTION(p |> NEGATION, q |> NEGATION) |> NEGATION
qandp               = SyntaxTree(CONJUNCTION, (q,p))
pandr               = SyntaxTree(CONJUNCTION, (p,r))
porq                = SyntaxTree(DISJUNCTION, (p,q))
norm                = SyntaxTree(DISJUNCTION, (m,n))
trees_implication   = SyntaxTree(IMPLICATION, (pandq, porq))

interp1             = TruthDict([p => TOP, q => TOP])
interp2             = TruthDict(1:4, BOTTOM)

# Test section

@test AbstractFormula           <: Syntactical
@test AbstractSyntaxStructure   <: AbstractFormula
@test AbstractLeaf              <: AbstractSyntaxStructure
@test Truth                     <: AbstractLeaf

@test TOP               isa Truth
@test TOP               isa BooleanTruth
@test BOTTOM            isa Truth
@test BOTTOM            isa BooleanTruth

@test Connective        <: Syntactical
@test NamedConnective   <: Connective

@test Connective        <: Operator
@test Truth             <: Operator
@test Connective        <: SyntaxToken
@test AbstractLeaf      <: SyntaxToken

@test AbstractComposite <: AbstractSyntaxStructure
@test SyntaxTree        <: AbstractComposite

@test NEGATION          isa NamedConnective
@test CONJUNCTION       isa NamedConnective
@test DISJUNCTION       isa NamedConnective
@test IMPLICATION       isa NamedConnective
@test typeof(¬)         <: NamedConnective
@test typeof(∧)         <: NamedConnective
@test typeof(∨)         <: NamedConnective
@test typeof(→)         <: NamedConnective

@test isnullary(p)      == true
@test syntaxstring(p)   == "p"

@test pandq             |> syntaxstring == CONJUNCTION(p, q)        |> syntaxstring
@test pandq             |> syntaxstring == (@synexpr p ∧ q)         |> syntaxstring
@test porq              |> syntaxstring == DISJUNCTION(p, q)        |> syntaxstring
@test porq              |> syntaxstring == (@synexpr p ∨ q)         |> syntaxstring
@test trees_implication |> syntaxstring == IMPLICATION(pandq, porq) |> syntaxstring
@test trees_implication |> syntaxstring == (@synexpr pandq → porq)  |> syntaxstring

@test pandq             |> syntaxstring ==
    SL.joinformulas(CONJUNCTION, (p, q)) |> syntaxstring
@test porq              |> syntaxstring ==
    SL.joinformulas(DISJUNCTION, (p, q)) |> syntaxstring
@test trees_implication |> syntaxstring ==
    SL.joinformulas(IMPLICATION, (pandq, porq)) |> syntaxstring

# @test Base.operator_precedence(TOP)         == SL.MAX_PRECEDENCE
# @test Base.operator_precedence(BOTTOM)      == SL.MAX_PRECEDENCE
# @test Base.operator_precedence(NEGATION)    == SL.HIGH_PRECEDENCE
# @test Base.operator_precedence(CONJUNCTION) == SL.BASE_PRECEDENCE
# @test Base.operator_precedence(DISJUNCTION) == SL.BASE_PRECEDENCE
# @test Base.operator_precedence(IMPLICATION) == SL.LOW_PRECEDENCE

@test parsetree("p → q ∧ r") == (@synexpr p → q ∧ r)
@test parsetree("p → (q → r)") == (@synexpr p → (q → r))
@test parsetree("p → (q ∧ r)") == (@synexpr p → (q ∧ r))

@test natoms(pandq)                 == 2
@test natoms(trees_implication)     == natoms(pandq) + natoms(porq)
@test Set(atoms(pandq))             == Set(atoms(qandp))
@test Set(atoms(pandq))             == Set(atoms(porq))
@test Set(atoms(pandq))             != Set(atoms(pandr))
@test height(trees_implication)     == height(pandq) + height(qandp)

@test isequal(p, p)                     == true
@test isequal(p, q)                     == false
@test isequal(pandq, porq)              == false
@test isequal(porq, porq)               == true
@test isequal(pandq, pandq_demorgan)    == false # This is not semantics, but syntax only.

@test token(pandq)                      == CONJUNCTION
@test token(norm)                       == DISJUNCTION
@test token(pandq_demorgan)             == NEGATION
@test token(trees_implication)          == IMPLICATION

@test trees_implication |> children |> first == pandq
@test trees_implication |> children |> last  == porq

@test norm |> children |> first                     == SyntaxTree(m)
@test norm |> children |> first |> token            == m
@test norm |> children |> first |> token |> value   == value(m)

@test_nowarn interp1[p] = BOTTOM
@test_broken check(pandq, interp1)


end
