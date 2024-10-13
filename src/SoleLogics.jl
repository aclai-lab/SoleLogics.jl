module SoleLogics

import Base: show
using DataStructures
using Dictionaries
using PrettyTables
using Random
using StatsBase
using Reexport
using Lazy

using SoleBase
using SoleBase: initrng

############################################################################################

export iscrisp, isfinite, isnullary, isunary, isbinary

export Syntactical, Connective,
    Formula, SyntaxStructure, SyntaxTree, SyntaxLeaf,
    Atom, Truth, SyntaxBranch

export Operator, SyntaxToken

export syntaxstring

export arity, valuetype, tokentype, tokenstype,
        atomstype, operatorstype, truthtype,
        associativity, precedence

# export value # TODO remove. The name is too generic, and it clashes, e.g., with JuMP.value.
export token, children, formulas
export tree

export tokens, ntokens, atoms, natoms, truths, ntruths, leaves, nleaves,
        connectives, nconnectives, operators, noperators, height
export composeformulas

include("types/syntactical.jl")

############################################################################################

export interpret, check

include("types/interpretation.jl")

############################################################################################

export AlphabetOfAny, ExplicitAlphabet, UnionAlphabet

export alphabet, alphabets
export domain, top, bot, grammar, algebra, logic

include("types/logic.jl")

############################################################################################

export TOP, ⊤
export BOT, ⊥
export BooleanTruth
export istop, isbot

export NamedConnective, CONJUNCTION, NEGATION, DISJUNCTION, IMPLICATION
export ∧, ¬, ∨, →

export BooleanAlgebra

export BaseLogic

include("utils.jl")

############################################################################################

export propositionallogic

export TruthDict, DefaultedTruthDict
export truth_table

include("propositional-logic.jl")

############################################################################################

export accessibles
export ismodal, modallogic

export DIAMOND, BOX, ◊, □
export DiamondRelationalConnective, BoxRelationalConnective
export diamond, box
export globaldiamond, globalbox

export KripkeStructure
export truthtype, worldtype

export AbstractWorld

export AbstractWorlds, Worlds

export Interval, Interval2D, OneWorld


export AbstractRelation

export GlobalRel, IdentityRel
export globalrel, identityrel


include("modal-logic.jl")

############################################################################################

include("many-valued-logics/ManyValuedLogics.jl")

############################################################################################

export LeftmostLinearForm, LeftmostConjunctiveForm, LeftmostDisjunctiveForm, Literal

export subformulas, normalize

export CNF, DNF, cnf

include("syntax-utils.jl")

############################################################################################

include("interpretation-sets.jl")

############################################################################################

export parseformula

include("parse.jl")

############################################################################################

export randatom
export randbaseformula, randformula
export randframe, randmodel

include("generation/formula.jl")
include("generation/models.jl")

############################################################################################

export AnchoredFormula

include("anchored-formula.jl")

############################################################################################

export @atoms, @synexpr

include("ui.jl")

############################################################################################

include("experimentals.jl")

############################################################################################

include("deprecate.jl")

############################################################################################
# Fast isempty(intersect(u, v))
function intersects(u, v)
    for x in u
        if x in v
            return true
        end
    end
    false
end

function inittruthvalues(truthvalues::Union{Vector{<:Truth}, AbstractAlgebra})
    return (truthvalues isa AbstractAlgebra) ? domain(truthvalues) : truthvalues
end

function displaysyntaxvector(a, maxnum = 8; quotes = true)
    q = e -> (quotes ? "\"$(e)\"" : "$(e)")
    els = begin
        if length(a) > maxnum
            [(q.(syntaxstring.(a)[1:div(maxnum, 2)]))..., "...",
                (q.(syntaxstring.(a)[(end - div(maxnum, 2)):end]))...,]
        else
            q.(syntaxstring.(a))
        end
    end
    "$(eltype(a))[$(join(els, ", "))]"
end

############################################################################################

end
