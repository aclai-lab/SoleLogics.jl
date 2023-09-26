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
    │   │   │       └── TOP
    │   │   │       └── BOTTOM
    │   │   │       └── ...
    │   │   └── AbstractComposite
    │   │       ├── SyntaxTree
    │   │       ├── LeftmostLinearForm
    │   │       └── ...
    │   └── AbstractMemoFormula
    │       └── TruthTable
    └── Connective
        └── NamedConnective
        └── RelationalConnective
        └── ...

    Also:
    const Operator = Union{Connective,Truth}
    const SyntaxToken = Union{Connective,AbstractLeaf}
    const BooleanTruth = Union{Top,Bottom}
=#

@test AbstractFormula <: Syntactical
@test AbstractSyntaxStructure <: AbstractFormula
@test AbstractLeaf <: AbstractSyntaxStructure
@test Truth <: AbstractLeaf

@test TOP isa Truth
@test TOP isa BooleanTruth
@test BOTTOM isa Truth
@test BOTTOM isa BooleanTruth

@test AbstractComposite <: AbstractSyntaxStructure
@test SyntaxTree <: AbstractComposite

@test Connective <: Syntactical
@test NamedConnective <: Connective

@test NEGATION isa NamedConnective
@test typeof(¬) <: NamedConnective
@test CONJUNCTION isa NamedConnective
@test typeof(∧) <: NamedConnective
@test DISJUNCTION isa NamedConnective
@test typeof(∨) <: NamedConnective
@test IMPLICATION isa NamedConnective
@test typeof(→) <: NamedConnective

end
