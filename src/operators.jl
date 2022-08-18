using IterTools

#################################
#       Abstract Types          #
#################################
abstract type AbstractOperator{T} end

abstract type AbstractUnaryOperator{T} <: AbstractOperator{T} end
abstract type AbstractBinaryOperator{T} <: AbstractOperator{T} end

abstract type AbstractModalOperator{T} <: AbstractUnaryOperator{T} end

abstract type AbstractExistentialModalOperator{T} <: AbstractModalOperator{T} end
abstract type AbstractUniversalModalOperator{T} <: AbstractModalOperator{T} end

#################################
#       Concrete Types          #
#################################
struct UnaryOperator{T} <: AbstractUnaryOperator{T} end
UnaryOperator(s::AbstractString) = UnaryOperator{Symbol(s)}()
UnaryOperator(s::Symbol) = UnaryOperator{s}()
const UNOP(op) = UnaryOperator(op)

struct BinaryOperator{T} <: AbstractBinaryOperator{T} end
BinaryOperator(s::AbstractString) = BinaryOperator{Symbol(s)}()
BinaryOperator(s::Symbol) = BinaryOperator{s}()
const BINOP(op) = BinaryOperator(op)

struct ExistentialModalOperator{T} <: AbstractExistentialModalOperator{T} end
function ExistentialModalOperator(t::NTuple{N,AbstractString}) where {N}
    if length(t) > 1
        s = *(["$(x)," for x in t[1:end-1]]...) * "$(t[end])"
    else
        s = "$(t[1])"
    end
    return ExistentialModalOperator(s)
end
ExistentialModalOperator(s::AbstractString) = ExistentialModalOperator{Symbol(s)}()
ExistentialModalOperator(s::Symbol) = ExistentialModalOperator{s}()
const EXMODOP(op) = ExistentialModalOperator(op)

struct UniversalModalOperator{T} <: AbstractUniversalModalOperator{T} end
function UniversalModalOperator(t::NTuple{N,AbstractString}) where {N}
    if length(t) > 1
        s = *(["$(x)," for x in t[1:end-1]]...) * "$(t[end])"
    else
        s = "$(t[1])"
    end
    return UniversalModalOperator(s)
end
UniversalModalOperator(s::AbstractString) = UniversalModalOperator{Symbol(s)}()
UniversalModalOperator(s::Symbol)         = UniversalModalOperator{s}()
const UNIVMODOP(op) = UniversalModalOperator(op)

reltype(::AbstractOperator{T}) where {T} = T
show(io::IO, op::AbstractOperator{T}) where {T} = print(io, "$(reltype(op))")

function show(io::IO, op::AbstractExistentialModalOperator{T}) where {T}
    # ⟨ ⟩ should not be printed in the simplest case ◊
    print(io, "⟨$(reltype(op))⟩")
end

function show(io::IO, op::AbstractUniversalModalOperator{T}) where {T}
    # [ ] should not be printed in the simplest case □
    print(io, "[$(reltype(op))]")
end

#################################
#            Traits             #
#################################
SoleTraits.is_unary_operator(::AbstractUnaryOperator) = true
SoleTraits.is_binary_operator(::AbstractBinaryOperator) = true

SoleTraits.is_modal_operator(::AbstractModalOperator) = true
SoleTraits.is_existential_modal_operator(::AbstractExistentialModalOperator) = true
SoleTraits.is_universal_modal_operator(::AbstractUniversalModalOperator) = true

#################################
#      `Operators` wrapper      #
#         and utilities         #
#################################
struct Operators <: AbstractArray{AbstractOperator,1}
    ops::AbstractArray{AbstractOperator,1}
end
Base.size(ops::Operators) = (length(ops.ops))
Base.axes(ops::Operators) = (1:length(ops.ops),)
Base.IndexStyle(::Type{<:Operators}) = IndexLinear()
Base.getindex(ops::Operators, i::Int) = ops.ops[i]
Base.setindex!(ops::Operators, op::AbstractOperator, i::Int) = ops.ops[i] = op

const NEGATION = UNOP("¬")
const DIAMOND = EXMODOP("") # ⟨⟩
const BOX = UNIVMODOP("")   # []

const CONJUNCTION = BINOP("∧")
const DISJUNCTION = BINOP("∨")
const IMPLICATION = BINOP("→")

# This could be considered a trait, consider modify SoleTraits
ariety(::AbstractUnaryOperator) = return 1
ariety(::AbstractBinaryOperator) = return 2
ariety(::AbstractOperator) = error("Expand code")

#################################
#    More on modal operators    #
#   and modal logic extensions  #
#################################
const HSRELATIONS = [
    "L",    # later
    "A",    # after
    "O",    # overlaps
    "E",    # ends
    "D",    # during
    "B",    # begins
    "L̅",    # before
    "A̅",    # met by
    "O̅",    # overlapped by
    "E̅",    # ended by
    "D̅",    # contains
    "B̅",    # begun by
    "="     # equals/identity
]

const HS₃RELATIONS = [
    "L",    # later
    "L̅",    # before
    "I"     # intersects
]

const HS₇RELATIONS = [
    "L",    # later
    "AO",   # after or overlaps
    "DBE",  # during or begins or ends
    "L̅",    # before
    "A̅O̅",   # met by or overlapped by
    "D̅B̅E̅",  # contains or begun by or ended by
    "="     # equals/identity
]

# Macro to collect all modaloperators (e.g @modaloperators HSRELATIONS 1)
macro modaloperators(R, d::Int)
    quote
        rels = vec(collect(Iterators.product([$(R) for _ in 1:$(d)]...)))
        if "=" in $(R)
            rels = rels[1:end-1]
        end
        exrels = [EXMODOP(r) for r in rels]
        univrels = [UNIVMODOP(r) for r in rels]
        Operators(vcat(exrels, univrels))
    end
end
