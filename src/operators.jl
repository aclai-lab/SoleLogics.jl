using IterTools

abstract type AbstractOperator{T} end

abstract type AbstractUnaryOperator{T}      <: AbstractOperator{T} end
abstract type AbstractBinaryOperator{T}     <: AbstractOperator{T} end

abstract type AbstractModalOperator{T}      <: AbstractUnaryOperator{T} end

struct UnaryOperator{T} <: AbstractUnaryOperator{T} end
UnaryOperator(s::AbstractString)   = UnaryOperator{Symbol(s)}()
# UnaryOperator(s::Symbol)           = UnaryOperator{s}()

const NEGATION = UnaryOperator("¬")

abstract type AbstractExistentialModalOperator{T}   <: AbstractModalOperator{T} end
abstract type AbstractUniversalModalOperator{T}     <: AbstractModalOperator{T} end

struct ExistentialModalOperator{T}  <: AbstractExistentialModalOperator{T} end
struct UniversalModalOperator{T}    <: AbstractUniversalModalOperator{T} end
ExistentialModalOperator(s::AbstractString) = ExistentialModalOperator{Symbol(s)}()
function ExistentialModalOperator(t::NTuple{N,AbstractString}) where N
    s = *(["$(x)," for x in t[1:end-1]]...) * "$(t[end])"
    return ExistentialModalOperator(s)
end
# ExistentialModalOperator(s::Symbol)         = ExistentialModalOperator{s}()
UniversalModalOperator(s::AbstractString)   = UniversalModalOperator{Symbol(s)}()
function UniversalModalOperator(t::NTuple{N,AbstractString}) where N
    s = *(["$(x)," for x in t[1:end-1]]...) * "$(t[end])"
    return UniversalModalOperator(s)
end
# UniversalModalOperator(s::Symbol)           = UniversalModalOperator{s}()

const EXMODOP(op)    = ExistentialModalOperator(op)
const UNIVMODOP(op)  = UniversalModalOperator(op)

reltype(::AbstractOperator{T}) where T    = T

function show(io::IO, op::AbstractExistentialModalOperator{T}) where T
    print(io, "⟨$(reltype(op))⟩")
end

function show(io::IO, op::AbstractUniversalModalOperator{T})   where T
    print(io, "[$(reltype(op))]")
end

struct BinaryOperator{T} <: AbstractBinaryOperator{T} end
BinaryOperator(s::AbstractString)   = BinaryOperator{Symbol(s)}()
# BinaryOperator(s::Symbol)           = BinaryOperator{s}()

const CONJUNCTION = BinaryOperator("∧")
const DISJUNCTION = BinaryOperator("∧")
const IMPLICATION = BinaryOperator("→")

show(io::IO, op::AbstractOperator{T}) where T = print(io, "$(reltype(op))")

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

struct Operators <: AbstractArray{AbstractOperator, 1}
    ops::AbstractArray{AbstractOperator, 1}
end

Base.size(ops::Operators) = (length(ops.ops),)
Base.IndexStyle(::Type{<:Operators}) = IndexLinear()
Base.getindex(ops::Operators, i::Int) = ops.ops[i]
Base.setindex!(ops::Operators, op::AbstractOperator, i::Int) = ops.ops[i] = op



# TESTING
println("\toperators.jl testing")
exop = EXMODOP("L,L")
univop = UNIVMODOP("LABDE,DBE")

println("\tsingle operators")

@show NEGATION
@show CONJUNCTION
@show DISJUNCTION
@show IMPLICATION
@show exop
@show univop

println("\tvector of d-tuples of relations")

d = 3
@show d
rels = vec(collect(Iterators.product([HS₇RELATIONS for _ in 1:d]...)))
@show rels
@show size(rels)
@show typeof(rels)
@show EXMODOP(rels[50])
@show UNIVMODOP(rels[300])
