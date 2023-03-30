module SoleLogics

import Base: show
using DataStructures
using Dictionaries
using Random
using Reexport
using Lazy

include("utils.jl")

export iscrisp, isfinite, isnullary, isunary, isbinary

export Proposition,
    #
    AlphabetOfAny,
    ExplicitAlphabet,
    #
    SyntaxTree,
    #
    AbstractFormula,
    Formula

export syntaxstring

export TOP, ⊤
export BOTTOM, ⊥

export arity, atomtype, tokentype, tokenstype,
        propositionstype, operatorstype, truthtype
export check
export atom, token, children, alphabet, formulas
export domain, top, bottom, grammar, algebra, logic, tree
export tops, bottoms

export tokens, operators, propositions

include("general.jl")

export ∧, ¬, ∨, →
export CONJUNCTION, NEGATION, DISJUNCTION, IMPLICATION

export BaseLogic


include("base-logic.jl")

export propositionallogic

export TruthDict, DefaultedTruthDict

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

include("modal-logic.jl")

export subformulas, normalize

include("check.jl")

include("interpretation-sets.jl")

export parseformula, parseformulatree

include("parse.jl")

include("syntax-forms.jl")

export randformula, randformulatree

include("random.jl")

end
