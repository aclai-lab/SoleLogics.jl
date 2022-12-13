using DataStructures: OrderedDict
using NamedArrays

export ismodal
export DIAMOND, BOX, ◊, □

############################################################################################
############################################################################################
############################################################################################

abstract type AbstractWorld end

abstract type AbstractKripkeFrame{W<:AbstractWorld,T<:TruthValue} end

"""
    truthtype(::Type{<:AbstractKripkeFrame{W, T}}) where {W<:AbstractWorld, T<:TruthValue} = T
    truthtype(a::AbstractKripkeFrame) = truthtype(typeof(a))

The Julia type for representing truth values of the algebra.

See also [`AbstractKripkeFrame`](@ref).
"""
truthtype(::Type{<:AbstractKripkeFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = T
truthtype(a::AbstractKripkeFrame) = truthtype(typeof(a))

function nworlds(::AbstractKripkeFrame)::Integer end
# TODO: perhaps first --> initial?
# TODO: also there can be many initial worlds
function firstworld(::AbstractKripkeFrame{W})::W where {W<:AbstractWorld} end
function accessible_worlds(::AbstractKripkeFrame{W}, ::W) where {W<:AbstractWorld} end

# TODO: please explain the logic of this
struct AdjMatKripkeFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractKripkeFrame{W,T}
    adjacents::NamedMatrix{T,Matrix{T},Tuple{OrderedDict{W,Int64},OrderedDict{W,Int64}}}
end


"""
    abstract type KripkeModel{A, T<:TruthValue} <: AbstractLogicalModel{A, T} end

Abstract type for representing
[Kripke structures](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).
It comprehends a directed graph structure (Kripke frame), where nodes are referred to as
*worlds*, and the binary relation between them is referred to as the
*accessibility relation*. Additionally, each world is associated with a mapping from
`Proposition`'s of atom type `A` to truth values of type `T`.

See also [`AbstractLogicalModel`](@ref).
"""
abstract type AbstractKripkeModel{W<:AbstractWorld,A,T<:TruthValue} <: AbstractLogicalModel{A,T} end

function frame(m::AbstractKripkeModel)::AbstractKripkeFrame
    return error("Please, provide method frame(m::$(typeof(m))).")
end

nworlds(m::AbstractKripkeModel) = nworlds(frame(m))
firstworld(m::AbstractKripkeModel) = firstworld(frame(m)) # TODO: see above comment
accessible_worlds(m::AbstractKripkeModel) = accessible_worlds(frame(m))

# Most general case
struct KripkeModel{W<:AbstractWorld,A,T<:TruthValue,K<:AbstractKripkeFrame{W,T},V<:Interpretation{A,T}} <: AbstractKripkeModel{W,A,T}
    frame::K
    valuations::Dict{W,V}
end

# TODO: delete it? it is bareboned
function check(f::Formula, m::KripkeModel{A,T})::T where {A,T<:TruthValue} end

############################################################################################
############################################################################################
############################################################################################

ismodal(::Type{<:AbstractOperator}) = false
ismodal(o::AbstractOperator)::Bool = ismodal(typeof(o))

doc_DIAMOND = """
    const DIAMOND = NamedOperator{:◊}()
    const ◊ = DIAMOND
    TODO: add ismodal also
    arity(::NamedOperator{:◊}) = 1

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
arity(::NamedOperator{:◊}) = 1


doc_BOX = """
    const BOX = NamedOperator{:□}()
    const □ = BOX
    TODO: add ismodal also
    arity(::NamedOperator{:□}) = 1

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
arity(::NamedOperator{:□}) = 1

############################################################################################
######################################## BASE ##############################################
############################################################################################



# A type for a world identified by its name
struct World{T} <: AbstractWorld
    name::T
end
