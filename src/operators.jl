using IterTools

abstract type AbstractOperator{T} end

abstract type AbstractUnaryOperator{T} <: AbstractOperator{T} end
abstract type AbstractBinaryOperator{T} <: AbstractOperator{T} end

abstract type AbstractModalOperator{T} <: AbstractUnaryOperator{T} end

struct UnaryOperator{T} <: AbstractUnaryOperator{T} end
UnaryOperator(s::AbstractString) = UnaryOperator{Symbol(s)}()
# UnaryOperator(s::Symbol)           = UnaryOperator{s}()

const NEGATION = UnaryOperator("¬")
const DIAMOND = UnaryOperator("◊")
const BOX = UnaryOperator("□")

abstract type AbstractExistentialModalOperator{T} <: AbstractModalOperator{T} end
abstract type AbstractUniversalModalOperator{T} <: AbstractModalOperator{T} end

struct ExistentialModalOperator{T} <: AbstractExistentialModalOperator{T} end
struct UniversalModalOperator{T} <: AbstractUniversalModalOperator{T} end
ExistentialModalOperator(s::AbstractString) = ExistentialModalOperator{Symbol(s)}()
function ExistentialModalOperator(t::NTuple{N,AbstractString}) where {N}
    if length(t) > 1
        s = *(["$(x)," for x in t[1:end-1]]...) * "$(t[end])"
    else
        s = "$(t[1])"
    end
    return ExistentialModalOperator(s)
end
# ExistentialModalOperator(s::Symbol)         = ExistentialModalOperator{s}()
UniversalModalOperator(s::AbstractString) = UniversalModalOperator{Symbol(s)}()
function UniversalModalOperator(t::NTuple{N,AbstractString}) where {N}
    if length(t) > 1
        s = *(["$(x)," for x in t[1:end-1]]...) * "$(t[end])"
    else
        s = "$(t[1])"
    end
    return UniversalModalOperator(s)
end
# UniversalModalOperator(s::Symbol)           = UniversalModalOperator{s}()

const EXMODOP(op) = ExistentialModalOperator(op)
const UNIVMODOP(op) = UniversalModalOperator(op)

reltype(::AbstractOperator{T}) where {T} = T

function show(io::IO, op::AbstractExistentialModalOperator{T}) where {T}
    print(io, "⟨$(reltype(op))⟩")
end

function show(io::IO, op::AbstractUniversalModalOperator{T}) where {T}
    print(io, "[$(reltype(op))]")
end

struct BinaryOperator{T} <: AbstractBinaryOperator{T} end
BinaryOperator(s::AbstractString) = BinaryOperator{Symbol(s)}()
# BinaryOperator(s::Symbol)           = BinaryOperator{s}()

const CONJUNCTION = BinaryOperator("∧")
const DISJUNCTION = BinaryOperator("∨")
const IMPLICATION = BinaryOperator("→")

show(io::IO, op::AbstractOperator{T}) where {T} = print(io, "$(reltype(op))")

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

struct Operators <: AbstractArray{AbstractOperator,1}
    ops::AbstractArray{AbstractOperator,1}
end

Base.size(ops::Operators) = (length(ops.ops))
Base.IndexStyle(::Type{<:Operators}) = IndexLinear()
Base.getindex(ops::Operators, i::Int) = ops.ops[i]
Base.setindex!(ops::Operators, op::AbstractOperator, i::Int) = ops.ops[i] = op

const unary_operators = Operators(AbstractUnaryOperator[NEGATION, DIAMOND, BOX])
const binary_operators = Operators(AbstractBinaryOperator[CONJUNCTION, DISJUNCTION, IMPLICATION])
isunaryoperator(s::Symbol) = s in unary_operator
isbinaryoperator(s::Symbol) = s in binary_operators

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

# Could be an ImmutableDict instead
# Symbol("(") is needed for parsing
const operators_precedence = Dict{Union{AbstractOperator,Symbol}, Int}(
    NEGATION => 30,
    DIAMOND => 20,
    BOX => 20,
    CONJUNCTION => 10,
    DISJUNCTION => 10,
    Symbol("(") => 0
)

const operator = Dict{Symbol,AbstractOperator}()
for op in [unary_operators.ops..., binary_operators.ops...]
    pair = (reltype(op), op)
    setindex!(operator, pair[2], pair[1])
end

# # TESTING
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
# # @show EXMODOP(rels[50])
# # @show UNIVMODOP(rels[300])
# #@show @operators HSRELATIONS 2
# R = HSRELATIONS
# rels = vec(collect(Iterators.product([R for _ in 1:d]...)))
# exrels = [EXMODOP(r) for r in rels]
# univrels = [UNIVMODOP(r) for r in rels]
# # @show exrels
# # @show univrels
# ops = Operators(vcat(exrels, univrels))
# @show ops

# ops = @modaloperators HS₃RELATIONS 3
# @show ops
# @show reltype(ops[1])
