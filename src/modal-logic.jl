using DataStructures: OrderedDict
using NamedArrays

export ismodal, modal_logic
export DIAMOND, BOX, ◊, □

############################################################################################
############################################################################################
############################################################################################

"""
    abstract type AbstractWorld end

Abstract type for the nodes of an annotated accessibility graph (Kripke structure/model).
In modal logic, the truth of formulas is relativized to *worlds*, that is, nodes of a graph.

See also [`AbstractKripkeModel`](@ref), [`AbstractKripkeFrame`](@ref).
"""
abstract type AbstractWorld end

"""
    abstract type AbstractKripkeFrame{W<:AbstractWorld,T<:TruthValue} end

Abstract type for an accessibility graph (Kripke frame), that gives the structure to
    [Kripke models](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).

See also [`AbstractKripkeModel`](@ref), [`AbstractWorld`](@ref).
"""
abstract type AbstractKripkeFrame{W<:AbstractWorld,T<:TruthValue} end

"""
    truthtype(::Type{<:AbstractKripkeFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = T
    truthtype(a::AbstractKripkeFrame) = truthtype(typeof(a))

The Julia type for representing truth values of the algebra.

See also [`AbstractKripkeFrame`](@ref).
"""
truthtype(::Type{<:AbstractKripkeFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = T
truthtype(a::AbstractKripkeFrame) = truthtype(typeof(a))

function worlds(::AbstractKripkeFrame{W})::AbstractVector{<:W} where {W<:AbstractWorld} end
function nworlds(::AbstractKripkeFrame)::Integer end
function initialworld(::AbstractKripkeFrame{W})::W where {W<:AbstractWorld} end
function accessible_worlds(::AbstractKripkeFrame{W}, ::W) where {W<:AbstractWorld} end

"""
"""
struct AdjMatKripkeFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractKripkeFrame{W,T}
    adjacents::NamedMatrix{T,Matrix{T},Tuple{OrderedDict{W,Int64},OrderedDict{W,Int64}}}
end


"""
    abstract type KripkeModel{A,T<:TruthValue} <: AbstractLogicalModel{A,T} end

Abstract type for representing
[Kripke models](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).
It comprehends a directed graph structure (Kripke frame), where nodes are referred to as
*worlds*, and the binary relation between them is referred to as the
*accessibility relation*. Additionally, each world is associated with a mapping from
`Proposition`ss of atom type `A` to truth values of type `T`.

See also [`AbstractLogicalModel`](@ref).
"""
abstract type AbstractKripkeModel{W<:AbstractWorld,A,T<:TruthValue} <: AbstractLogicalModel{A,T} end

function frame(m::AbstractKripkeModel)::AbstractKripkeFrame
    return error("Please, provide method frame(m::$(typeof(m))).")
end

nworlds(m::AbstractKripkeModel) = nworlds(frame(m))
initialworld(m::AbstractKripkeModel) = initialworld(frame(m))
accessible_worlds(m::AbstractKripkeModel) = accessible_worlds(frame(m))

"""
    struct KripkeModel{W<:AbstractWorld,A,T<:TruthValue,K<:AbstractKripkeFrame{W,T},D<:AbstractDict{W,V<:Interpretation{A,T}}} <: AbstractKripkeModel{W,A,T}
        frame::K
        interpretations::D
    end

Structure for representing
[Kripke models](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).
explicitly; it wraps a `frame`, and an abstract dictionary that assigns an interpretation to
each world.
"""
struct KripkeModel{W<:AbstractWorld,A,T<:TruthValue,K<:AbstractKripkeFrame{W,T},D<:AbstractDict{W,<:Interpretation{A,T}}} <: AbstractKripkeModel{W,A,T}
    frame::K
    interpretations::D
end

function check(::Formula, ::KripkeModel{A,T})::T where {A,T<:TruthValue} end

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

############################################################################################
######################################## BASE ##############################################
############################################################################################

# A type for a world identified by its name
struct NamedWorld{T} <: AbstractWorld
    name::T
end

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Notes of 15/12 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Up above, this definition already exists as "AbstractKripkeFrame";
# think about renaming all the occurrences in this.
# TODO2: I did not understand this.. I assume, at this point, that is correct.
"""
    abstract type AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

Abstract type for an accessibility graph (Kripke frame), that gives the structure to
    [Kripke models](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).

See also [`AbstractKripkeModel`](@ref), [`AbstractWorld`](@ref).
"""
abstract type AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

"""
    abstract type AbstractModalFrame{
        W<:AbstractWorld,
        T<:TruthValue
    } <: AbstractFrame{W,T} end

Specific frame involving modal logic.

See also [`AbstractFrame`](@ref)
"""
abstract type AbstractModalFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractFrame{W,T} end

# Association "(w1,w2) => truth_value". Not recommended in sparse scenarios.
struct AdjacencyModalFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractModalFrame{W,T}
    adjacents::NamedMatrix{T,Matrix{T},Tuple{OrderedDict{W,Int64},OrderedDict{W,Int64}}}
end

# function enum_accessibles(...)

# TODO2: Should we not provide some constants for the "basic" modal logic? Similar to propositional logic?
# TODO: I'm on it.
