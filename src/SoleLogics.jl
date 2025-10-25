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


export iscrisp, isfinite, isnullary, isunary, isbinary

export Syntactical, Connective,
    Formula, SyntaxStructure, SyntaxTree, SyntaxLeaf,
    AbstractAtom, Truth

export Operator, SyntaxToken

export tree, syntaxstring

export arity, valuetype, tokentype, tokenstype,
        atomstype, operatorstype, truthtype,
        associativity, precedence

export token, children, formulas

export tokens, ntokens, atoms, natoms, truths, ntruths, leaves, nleaves,
        connectives, nconnectives, operators, noperators, height

        export composeformulas

include("types/syntactical.jl")


export parseformula

include("types/parse.jl")



export interpret, check

include("types/interpretation.jl")


export AlphabetOfAny, ExplicitAlphabet, UnionAlphabet
export alphabet, subalphabets
export domain, top, bot, grammar, algebra, logic

include("types/logic.jl")


include("types/interpretation-sets.jl")

export Atom, SyntaxBranch

include("utils/syntactical.jl")

export TOP, ⊤
export BOT, ⊥
export BooleanTruth
export istop, isbot

export NamedConnective, CONJUNCTION, NEGATION, DISJUNCTION, IMPLICATION
export ∧, ¬, ∨, →

export BooleanAlgebra

export BaseLogic

include("utils/base-logic.jl")

export propositionallogic
export inlinedisplay

export TruthDict, DefaultedTruthDict
export truth_table

include("types/propositional-logic.jl")
include("utils/propositional-logic.jl")



export accessibles
export ismodal, isbox, isdiamond
export relationtype, relation
export collateworlds

export modallogic

export DIAMOND, BOX, ◊, □
export DiamondRelationalConnective, BoxRelationalConnective
export diamond, box
export globaldiamond, globalbox

export KripkeStructure
export truthtype, worldtype

export AbstractWorld
export AbstractWorlds, Worlds

export OneWorld
export GeometricalWorld, Point, Point1D, Point2D, Point3D
export Interval, Interval2D
export RelativeGeometricalWorld

export AbstractRelation

export GlobalRel, IdentityRel
export globalrel, identityrel


include("types/modal-logic.jl")

include("utils/modal-logic/modal-logic.jl")

include("utils/modal-logic/extended-k-modal-logic.jl")

include("utils/modal-logic/multi-modal-logic.jl")

export WorldFilter
export FunctionalWorldFilter, FilteredRelation
export IntervalLengthFilter


include("many-valued-logics/ManyValuedLogics.jl")



export LeftmostLinearForm, LeftmostConjunctiveForm, LeftmostDisjunctiveForm, Literal

export subformulas, normalize

export CNF, DNF, cnf, dnf

include("utils/syntactical-normal-forms.jl")

include("utils/tools.jl")



include("utils/interpretation-sets.jl")



include("utils/parse.jl")


export AnchoredFormula
include("utils/anchored-formula.jl")


# these first files are included here to avoid repeated inclusions in those below;
# "generation" could become a SoleLogics submodule.
include("generation/docstrings.jl")
include("generation/utils.jl")

export randatom
export randformula
include("generation/formula.jl")

export randframe, randmodel
include("generation/models.jl")




export @atoms, @synexpr

include("ui.jl")



include("experimentals.jl")



include("deprecate.jl")


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



end
