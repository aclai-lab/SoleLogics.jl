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
    print(io, "⟨$(reltype(op))⟩")
end

function show(io::IO, op::AbstractUniversalModalOperator{T}) where {T}
    print(io, "[$(reltype(op))]")
end

#################################
#      `Operators` wrapper      #
#         and utilities         #
#################################
struct Operators <: AbstractArray{AbstractOperator,1}
    ops::AbstractArray{AbstractOperator,1}
end

Base.size(ops::Operators) = (length(ops.ops))
Base.IndexStyle(::Type{<:Operators}) = IndexLinear()
Base.getindex(ops::Operators, i::Int) = ops.ops[i]
Base.setindex!(ops::Operators, op::AbstractOperator, i::Int) = ops.ops[i] = op

const NEGATION = UNOP("¬")
const DIAMOND = EXMODOP("◊")
const BOX = UNIVMODOP("□")

const CONJUNCTION = BINOP("∧")
const DISJUNCTION = BINOP("∨")
const IMPLICATION = BINOP("→")

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

# Macro to collect all modaloperators
# TODO: also if this works perfectly, an error is thrown in REPL
# See "a = @modaloperators HSRELATIONS 2"
# Anyway, a.ops is correct and typeof(a) is Operators as expected
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

#################################
# TESTING
# println("\toperators.jl testing")
# exop = EXMODOP("L,L")
# univop = UNIVMODOP("LABDE,DBE")

# println("\tsingle operators")

# @show NEGATION
# @show CONJUNCTION
# @show DISJUNCTION
# @show IMPLICATION
# @show exop
# @show univop

# println("\tvector of d-tuples of relations")

# d = 2
# @show d
# rels = vec(collect(Iterators.product([HS₇RELATIONS for _ in 1:d]...)))
# @show rels
# @show size(rels)
# @show typeof(rels)
# @show EXMODOP(rels[50])
# @show UNIVMODOP(rels[300])
# @show @operators HSRELATIONS 2
# R = HSRELATIONS
# rels = vec(collect(Iterators.product([R for _ in 1:d]...)))
# exrels = [EXMODOP(r) for r in rels]
# univrels = [UNIVMODOP(r) for r in rels]
# @show exrels
# @show univrels
# ops = Operators(vcat(exrels, univrels))
# @show ops

# ops = @modaloperators HS₃RELATIONS 3
# @show ops
# @show reltype(ops[1])
#################################
