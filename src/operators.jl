using IterTools

abstract type AbstractOperator{T} end

abstract type AbstractUnaryOperator{T}      <: AbstractOperator{T} end
abstract type AbstractBinaryOperator{T}     <: AbstractOperator{T} end

abstract type AbstractModalOperator{T}      <: AbstractUnaryOperator{T} end

struct UnaryOperator{T} <: AbstractUnaryOperator{T} end
UnaryOperator(s::AbstractString)   = UnaryOperator{Symbol(s)}()
#UnaryOperator(s::Symbol)           = UnaryOperator{s}()

const NEGATION = UnaryOperator("¬")

abstract type AbstractExistentialModalOperator{T}   <: AbstractModalOperator{T} end
abstract type AbstractUniversalModalOperator{T}     <: AbstractModalOperator{T} end

struct ExistentialModalOperator{T}  <: AbstractExistentialModalOperator{T} end
struct UniversalModalOperator{T}    <: AbstractUniversalModalOperator{T} end
ExistentialModalOperator(s::AbstractString) = ExistentialModalOperator{Symbol(s)}()
#ExistentialModalOperator(s::Symbol)         = ExistentialModalOperator{s}()
UniversalModalOperator(s::AbstractString)   = UniversalModalOperator{Symbol(s)}()
#UniversalModalOperator(s::Symbol)           = UniversalModalOperator{s}()

const EXMODOP(s::AbstractString)    = ExistentialModalOperator(s)
const EXMODOP(s::Symbol)            = ExistentialModalOperator(s)
const UNIVMODOP(s::AbstractString)  = UniversalModalOperator(s)
const UNIVMODOP(s::Symbol)          = UniversalModalOperator(s)

reltype(::AbstractOperator{T}) where T    = T

function show(io::IO, op::AbstractExistentialModalOperator{T}) where T
    print(io, "⟨$(reltype(op))⟩")
end

function show(io::IO, op::AbstractUniversalModalOperator{T})   where T
    print(io, "[$(reltype(op))]")
end

struct BinaryOperator{T} <: AbstractBinaryOperator{T} end
BinaryOperator(s::AbstractString)   = BinaryOperator{Symbol(s)}()
#BinaryOperator(s::Symbol)           = BinaryOperator{s}()

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

# TESTING
exop = EXMODOP("L,L")
univop = UNIVMODOP("LABDE,DBE")

@show NEGATION
@show CONJUNCTION
@show DISJUNCTION
@show IMPLICATION
@show exop
@show univop

# working on the generator
d = 3
# returns a vector of d-tuples where each value is an element of the set of relations
rels = vec(collect(Iterators.product([HSRELATIONS for _ in 1:d]...)))
@show rels
@show size(vec(rels))
