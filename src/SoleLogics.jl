module SoleLogics

import Base: show
using DataStructures
using Dictionaries
using PrettyTables
using Random
using StatsBase
using Reexport
using Lazy


include("utils.jl")

export iscrisp, isfinite, isnullary, isunary, isbinary

export Syntactical,
    Formula, AbstractSyntaxStructure, SyntaxTree, SyntaxLeaf,
    Atom, Truth, SyntaxBranch

export Operator, SyntaxToken

export Top, TOP, ⊤
export Bot, BOT, ⊥
export BooleanTruth

export syntaxstring

export arity, valuetype, tokentype, tokenstype,
        atomstype, operatorstype, truthtype,
        associativity, precedence
export value, token, children, formulas
export tree
export istop, isbot

export tokens, ntokens, atoms, natoms, truths, ntruths, leaves, nleaves,
        connectives, nconnectives, operators, noperators, height

export interpret, check

include("core.jl")

export AlphabetOfAny, ExplicitAlphabet

export alphabet
export domain, top, bot, grammar, algebra, logic

include("logics.jl")

export Connective
export NamedConnective, ∧, ¬, ∨, →
export CONJUNCTION, NEGATION, DISJUNCTION, IMPLICATION

export BooleanAlgebra

export BaseLogic

include("base-logic.jl")

export propositionallogic

export TruthDict, DefaultedTruthDict
export truth_table

include("propositional-logic.jl")

export accessibles
export ismodal, modallogic
export DiamondRelationalOperator, BoxRelationalOperator
export DIAMOND, BOX, ◊, □

export KripkeStructure
export AbstractRelationalOperator, DiamondRelationalOperator, BoxRelationalOperator
export relationtype, truthtype, worldtype

export AbstractWorld

export AbstractWorldSet, WorldSet

export Interval, Interval2D, OneWorld

export globaldiamond, globalbox

include("modal-logic.jl")

export LeftmostLinearForm, LeftmostConjunctiveForm, LeftmostDisjunctiveForm, Literal

export subformulas, normalize

include("syntax-utils.jl")

include("interpretation-sets.jl")

export parsebaseformula, parseformula, parsetree

include("parse.jl")

export randbaseformula, randformula

include("random.jl")

export AnchoredFormula

include("anchored-formula.jl")

export @atoms, @synexpr

include("ui.jl")

include("deprecate.jl")

end
