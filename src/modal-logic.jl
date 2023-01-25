using DataStructures: OrderedDict
using NamedArrays

export ismodal, modal_logic
export DIAMOND, BOX, ◊, □

export KripkeStructure
export AbstractRelationalOperator, DiamondRelationalOperator, BoxRelationalOperator
export relationtype


"""
    abstract type AbstractWorld end

Abstract type for the nodes of an annotated accessibility graph (Kripke structure).
In modal logic, the truth of formulas is relativized to *worlds*, that is, nodes of a graph.

See also [`AbstractKripkeStructure`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractWorld end

"""
    abstract type AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

Abstract type for an accessibility graph (Kripke frame), that gives the structure to
    [Kripke structures](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).

See also [`AbstractKripkeStructure`](@ref), [`AbstractWorld`](@ref).
"""
abstract type AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

"""
    truthtype(::Type{<:AbstractFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = T
    truthtype(a::AbstractFrame) = truthtype(typeof(a))

The truth type for the relation(s) in the frame.

See also [`AbstractFrame`](@ref).
"""
truthtype(::Type{<:AbstractFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = T
truthtype(a::AbstractFrame) = truthtype(typeof(a))

"""
    worldtype(::Type{<:AbstractFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = W
    worldtype(a::AbstractFrame) = worldtype(typeof(a))

The world type of the frame.

See also [`AbstractFrame`](@ref).
"""
worldtype(::Type{<:AbstractFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = W
worldtype(a::AbstractFrame) = worldtype(typeof(a))

"""
TODO
"""
function worlds(::AbstractFrame{W})::AbstractVector{<:W} where {W<:AbstractWorld}
    error("Please, provide ...")
end
function nworlds(::AbstractFrame)::Integer
    error("Please, provide ...")
end
function initialworld(::AbstractFrame{W})::W where {W<:AbstractWorld}
    error("Please, provide ...")
end


############################################################################################
############################################################################################
############################################################################################

"""
    abstract type AbstractModalFrame{
        W<:AbstractWorld,
        T<:TruthValue
    } <: AbstractFrame{W,T} end

Frame of a modal logic based on a single accessibility relation.

See also [`AbstractFrame`](@ref)
"""
abstract type AbstractModalFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractFrame{W,T} end

"""
TODO
"""
function accessibles(fr::AbstractModalFrame{W}, w::W)::Vector{W} where {W<:AbstractWorld}
    error("Please, provide method accessibles(fr::$(typeof(f)), w::$(typeof(w)))::Vector{$(W)}.")
end


# """
# TODO Association "(w1,w2) => truth_value". Not recommended in sparse scenarios.
# """
# struct AdjMatModalFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractModalFrame{W,T}
#     adjacents::NamedMatrix{T,Matrix{T},Tuple{OrderedDict{W,Int64},OrderedDict{W,Int64}}}
# end
# accessibles(...) = ...


abstract type AbstractRelation end

"""
    abstract type AbstractMultiModalFrame{
        W<:AbstractWorld,
        T<:TruthValue,
        NR,
        Rs<:NTuple{NR,<:AbstractRelation}
    } <: AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

Frame of a multi-modal logic, that is, a modal logic based on a set
of accessibility relations.

See also [`AbstractFrame`](@ref), [`AbstractModalFrame`](@ref).
"""
abstract type AbstractMultiModalFrame{
    W<:AbstractWorld,
    T<:TruthValue,
    NR,
    Rs<:NTuple{NR,R where R<:AbstractRelation},
} <: AbstractFrame{W,T} end

"""
TODO
"""
function accessibles(::AbstractModalFrame{W}, ::W, ::AbstractRelation)::Vector{W} where {W<:AbstractWorld}
    error("Please, provide ...")
end


"""
    TODO
Wrapper used to manage many `AbstractRelation`s using a specific `AbstractModalFrame` for
each of them.

See also [`AbstractRelation`](@ref), [`AbstractModalFrame`](@ref).
"""
struct WrapperMultiModalFrame{W<:AbstractWorld,T<:TruthValue,NR,Rs} <: AbstractMultiModalFrame{W,T,NR,Rs}
    frames::Dict{<:eltype(Rs),<:AbstractModalFrame{W,T}} # Could be done better?
end
# accessibles(...) = ...

"""
TODO
"""
struct AdjMatMultiModalFrame{W<:AbstractWorld,T<:TruthValue,NR,Rs} <: AbstractMultiModalFrame{W,T,NR,Rs}
    adjacents::NamedArray{W,3}
end
# accessibles(...) = ...

"""
TODO
"""
abstract type AbstractAssignment{W<:AbstractWorld,A,T<:TruthValue} end

"""
TODO
"""
function check(::AbstractAssignment{W,A,T}, ::W, ::Proposition{A})::T where {W<:AbstractWorld,A,T<:TruthValue} # TODO?: ::Formula
    error("Please, provide ...")
end

# struct GenericAssignment{W<:AbstractWorld,A,T<:TruthValue} <: AbstractAssignment{W,A,T}
#     dict::Dict{W,Valuation{A,T}}
# end

############################################################################################
############################################################################################
############################################################################################


"""
    abstract type KripkeStructure{A,T<:TruthValue} <: AbstractInterpretation{A,T} end

Abstract type for representing
[Kripke structures](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).
It comprehends a directed graph structure (Kripke frame), where nodes are referred to as
*worlds*, and the binary relation between them is referred to as the
*accessibility relation*. Additionally, each world is associated with a mapping from
`Proposition`s of atom type `A` to truth values of type `T`.

See also [`AbstractInterpretation`](@ref).
"""
abstract type AbstractKripkeStructure{
    W<:AbstractWorld,
    A,
    T<:TruthValue,
    KF<:AbstractFrame{W,T},
} <: AbstractInterpretation{A,T} end

function check(::AbstractKripkeStructure{W,A,T,KF}, ::W, ::Proposition{A})::T where {W<:AbstractWorld,A,T<:TruthValue,KF<:AbstractFrame{W,T}}
    error("Please, provide ...")
end

function check(::AbstractKripkeStructure{W,A,T,KF}, ::W, ::Formula{A})::T where {W<:AbstractWorld,A,T<:TruthValue,KF<:AbstractFrame{W,T}}
    error("Please, provide ...")
end

function frame(i::AbstractKripkeStructure{W,A,T,KF})::KF where {W<:AbstractWorld,A,T<:TruthValue,KF<:AbstractFrame{W,T}}
    return error("Please, provide method frame(i::$(typeof(i))).")
end

nworlds(i::AbstractKripkeStructure) = nworlds(frame(i))
initialworld(i::AbstractKripkeStructure) = initialworld(frame(i))
accessibles(i::AbstractKripkeStructure, args...) = accessibles(frame(i), args...)

"""
    struct KripkeStructure{W<:AbstractWorld,A,T<:TruthValue,K<:AbstractFrame{W,T},D<:AbstractDict{W,V<:Valuation{A,T}}} <: AbstractKripkeStructure{W,A,T}
        frame::K
        interpretations::D
    end

Structure for representing
[Kripke structures](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).
explicitly; it wraps a `frame`, and an abstract dictionary that assigns an interpretation to
each world.
"""
struct KripkeStructure{W<:AbstractWorld,A,T<:TruthValue,KF<:AbstractFrame{W,T}, AS<:AbstractAssignment{W,A,T}} <: AbstractKripkeStructure{W,A,T,KF}
    frame::KF
    assignment::AS
end

function check(i::KripkeStructure{W,A,T}, w::W, f::Formula)::T where {W<:AbstractWorld,A,T<:TruthValue} end

function check(i::KripkeStructure{W,A,T}, f::Formula)::T where {W<:AbstractWorld,A,T<:TruthValue}
    check(i, initial(i), f)
end

function check(i::KripkeStructure{W,A}, w::W, p::Proposition{A}) where {W<:AbstractWorld,A}
    check(i.assignment, w, p)
end

# TODO maybe this yields the worlds where a certain formula is true...?
# function check(i::KripkeStructure{W,A,T}, f::Formula)::AbstractVector{W} where {W<:AbstractWorld,A,T<:TruthValue}

############################################################################################
############################################################################################
############################################################################################

ismodal(::Type{<:AbstractOperator}) = false
ismodal(o::AbstractOperator)::Bool = ismodal(typeof(o))

doc_DIAMOND = """
    const DIAMOND = NamedOperator{:◊}()
    const ◊ = DIAMOND
    ismodal(::NamedOperator{:◊}) = true
    arity(::Type{typeof(◊)}) = 1

Logical diamond operator, typically interpreted as the modal existential quantifier.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_DIAMOND)
"""
const DIAMOND = NamedOperator{:◊}()
"""
$(doc_DIAMOND)
"""
const ◊ = DIAMOND
ismodal(::NamedOperator{:◊}) = true
arity(::Type{typeof(◊)}) = 1


doc_BOX = """
    const BOX = NamedOperator{:□}()
    const □ = BOX
    arity(::Type{typeof(□)}) = 1

Logical box operator, typically interpreted as the modal universal quantifier.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_BOX)
"""
const BOX = NamedOperator{:□}()
"""
$(doc_BOX)
"""
const □ = BOX
ismodal(::NamedOperator{:□}) = true
arity(::Type{typeof(□)}) = 1

############################################################################################

const BASE_MODAL_OPERATORS = [BASE_PROPOSITIONAL_OPERATORS..., ◊, □]
const BaseModalOperators = Union{typeof.(BASE_MODAL_OPERATORS)...}

"""
    modal_logic(;
        alphabet = AlphabetOfAny{String}(),
        operators = [⊤, ⊥, ¬, ∧, ∨, →, ◊, □],
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), [⊤, ⊥, ¬, ∧, ∨, →, ◊, □]),
        algebra = BooleanAlgebra(),
    )

Instantiates a [modal logic](https://simple.m.wikipedia.org/wiki/Modal_logic)
given a grammar and an algebra. Alternatively, an alphabet and a set of operators
can be specified instead of the grammar.

# Examples
```julia-repl
julia> modal_logic()
julia> modal_logic(; operators = [¬, ∨])
julia> modal_logic(; alphabet = ["p", "q"])
julia> modal_logic(; alphabet = ExplicitAlphabet([Proposition("p"), Proposition("q")]))
```

See also [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref).
"""
function modal_logic(;
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
)
    if !isnothing(operators) && length(setdiff(operators, BASE_PROPOSITIONAL_OPERATORS)) == 0
        @warn "Instantiating modal logic (via `modal_logic`) with solely" *
            " propositional operators. Consider using propositional_logic instead."
    end
    _base_logic(
        alphabet = alphabet,
        operators = operators,
        grammar = grammar,
        algebra = algebra;
        default_operators = BASE_MODAL_OPERATORS,
        logictypename = "modal logic",
    )
end

# A modal logic based on the base modal operators
const BaseModalLogic = AbstractLogic{G,A} where {ALP,G<:AbstractGrammar{ALP,<:BaseModalOperators},A<:AbstractAlgebra}

############################################################################################


abstract type AbstractRelationalOperator{R<:AbstractRelation} <: AbstractOperator end
# TODO: why the type parameter?
# TODO-reply: We want to dispatch on it. In this case, because different relations
#  carry different algorithmic behaviors (e.g., Later vs. After are computed in a
#  different way).

Base.operator_precedence(::AbstractRelationalOperator) = HIGH_PRIORITY

relationtype(::AbstractRelationalOperator{R}) where {R<:AbstractRelation} = R

arity(::Type{<:AbstractRelationalOperator{R}}) where {R<:AbstractRelation} = arity(R)-1

struct DiamondRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end

struct BoxRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end

############################################################################################
######################################## BASE ##############################################
############################################################################################

# A type for a world identified by its name
struct NamedWorld{T} <: AbstractWorld
    name::T
end


# Named-relation type
struct NamedRelation <: AbstractRelation
    name::Symbol
    # adjacency matrix between NamedWorld's
end
