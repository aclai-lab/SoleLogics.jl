module SoleLogics

"""
GENERAL TODOs:
    - Define the `Operators`` interface, that is subtype of AbstractArray?
    - Define the logic with:
        1) an `Alphabet``
        2) a set of `Operators` (based on the previous TODO)
        3) a name
    - Define a generator function that uses @eval at runtime
"""

import Base: show

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

# TESTING
exop = EXMODOP("L,L")
univop = UNIVMODOP("LABDE,DBE")

@show NEGATION
@show CONJUNCTION
@show DISJUNCTION
@show IMPLICATION
@show exop
@show univop

end
