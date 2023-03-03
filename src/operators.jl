using IterTools

############################################################################################
#       Abstract Types
############################################################################################
"""Root of Operator abstract-types tree"""
abstract type AbstractOperator{T} end

abstract type AbstractModalOperator{T} <: AbstractOperator{T} end
abstract type AbstractExistentialModalOperator{T} <: AbstractModalOperator{T} end
abstract type AbstractUniversalModalOperator{T} <: AbstractModalOperator{T} end

############################################################################################
#     Definition utilities
############################################################################################
"""
    ariety(op)
Return the ariety associated with an operator.

# Example
```jldoctest
julia> ariety(CONJUNCTION)
2
julia> my_op = OP(:m)
julia> SoleLogics.ariety(my_op) = 1
julia> ariety(my_op)
1
```
"""
ariety(op) = error(
    "No ariety associated with $op.\n
    Please, follow the example in the documentation to set an ariety."
)

############################################################################################
#       Concrete Types
############################################################################################

struct Operator{T} <: AbstractOperator{T} end
Operator(s::AbstractString) = Operator{Symbol(s)}()
Operator(s::Symbol) = Operator{s}()

"""
    OP(op::Union{AbstractString,Symbol})
Return a new operator as a singleton.

# Example
```jldoctest
julia> myop = OP(:℘)
℘
julia> SoleLogics.ariety(myop) = 1
```
"""
const OP(op::Union{AbstractString,Symbol}) = Operator(op)

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

"""
    EXMODOP(op)
Return a new existential modal operator as a singleton.

# Example
```jldoctest
julia> myop = EXMODOP(:℘)
⟨℘⟩
julia> is_modal_operator(myop)
true
julia> is_existential_modal_operator(myop)
true
```
"""
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
UniversalModalOperator(s::Symbol) = UniversalModalOperator{s}()

"""
    UNIVMODOP(op)
Return a new universal modal operator as a singleton.

# Example
```jldoctest
julia> myop = EXMODOP(:℘)
[℘]
julia> is_modal_operator(myop)
true
julia> is_universal_modal_operator(myop)
true
```
"""
const UNIVMODOP(op) = UniversalModalOperator(op)

"""Extract the symbol wrapped by an operator."""
reltype(::AbstractOperator{T}) where {T} = T

Base.show(io::IO, op::AbstractOperator{T}) where {T} = print(io, "$(reltype(op))")

function Base.show(io::IO, op::AbstractExistentialModalOperator{T}) where {T}
    # "⟨" and "⟩" delimeters should not be printed in the simplest case where T is :◊
    delim = ["⟨", "⟩"]
    if reltype(op) == Symbol("◊")
        delim = ["", ""]
    end

    print(io, delim[1] * "$(reltype(op))" * delim[2])
end

function Base.show(io::IO, op::AbstractUniversalModalOperator{T}) where {T}
    # "[" and "]" delimeters should not be printed in the simplest case where T is :□
    delim = ["[", "]"]
    if reltype(op) == :□
        delim = ["", ""]
    end

    print(io, delim[1] * "$(reltype(op))" * delim[2])
end

############################################################################################
#            Traits
############################################################################################
is_unary_operator(op::AbstractOperator) = return (ariety(op) == 1)
is_binary_operator(op::AbstractOperator) = return (ariety(op) == 2)

is_modal_operator(::AbstractModalOperator) = true
is_existential_modal_operator(::AbstractExistentialModalOperator) = true
is_universal_modal_operator(::AbstractUniversalModalOperator) = true

############################################################################################
#      `Operators` wrapper
#         and utilities
############################################################################################
"""Operators interface."""
const Operators = Vector{AbstractOperator}

############################################################################################
#    More on modal operators
#   and modal logic extensions
############################################################################################
"""Legal strings to generate HS opearators."""
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
    "=",     # equals/identity
]

"""Legal strings to generate HS₃ opearators."""
const HS₃RELATIONS = [
    "L",    # later
    "L̅",    # before
    "I",     # intersects
]

"""Legal strings to generate HS₇ opearators."""
const HS₇RELATIONS = [
    "L",    # later
    "AO",   # after or overlaps
    "DBE",  # during or begins or ends
    "L̅",    # before
    "A̅O̅",   # met by or overlapped by
    "D̅B̅E̅",  # contains or begun by or ended by
    "=",     # equals/identity
]

# Macro to collect all modaloperators (e.g @modaloperators HSRELATIONS 1)
"""
    modaloperators(R, d::Int)
Collect all the valid modal operators -both existential and universal- from a collection
of strings or symbols.

# Example
```jldoctest
julia> @modaloperators HSRELATIONS 1
⟨L⟩
⟨A⟩
⟨O⟩
⟨E⟩
⋮
[E̅]
[D̅]
[B̅]
julia> @modaloperators HS₃RELATIONS 2
⟨L,L⟩
⟨L̅,L⟩
⟨I,L⟩
⟨L,L̅⟩
⋮
[L,I]
[L̅,I]
[I,I]
```
"""
macro modaloperators(R, d::Int)
    quote
        rels = vec(collect(Iterators.product([$(R) for _ = 1:$(d)]...)))
        if "=" in $(R)
            rels = rels[1:end-1]
        end
        exrels = [EXMODOP(r) for r in rels]
        univrels = [UNIVMODOP(r) for r in rels]
        Operators(vcat(exrels, univrels))
    end
end

############################################################################################
#     Definitions and
#       behaviours
############################################################################################
# TODO: when SoleModelChecking will be merged here,
# write each operator behaviour in this section here.

"""Negation operator."""
const NEGATION = OP("¬")
SoleLogics.ariety(::typeof(NEGATION)) = 1
precedence(::typeof(NEGATION)) = 30

"""Diamond operator."""
const DIAMOND = EXMODOP("◊")
SoleLogics.ariety(::typeof(DIAMOND)) = 1
precedence(::typeof(DIAMOND)) = 21

"""Box operator."""
const BOX = UNIVMODOP("□")
SoleLogics.ariety(::typeof(BOX)) = 1
precedence(::typeof(BOX)) = 20

"""Conjunction operator."""
const CONJUNCTION = OP("∧")
SoleLogics.ariety(::typeof(CONJUNCTION)) = 2
precedence(::typeof(CONJUNCTION)) = 12

"""Disjunction operator."""
const DISJUNCTION = OP("∨")
SoleLogics.ariety(::typeof(DISJUNCTION)) = 2
precedence(::typeof(DISJUNCTION)) = 11

"""Implication operator."""
const IMPLICATION = OP("→")
SoleLogics.ariety(::typeof(IMPLICATION)) = 2
precedence(::typeof(IMPLICATION)) = 10

"""Equality operator."""
const EQUAL = OP("=")
SoleLogics.ariety(::typeof(EQUAL)) = 2
precedence(::typeof(EQUAL)) = 10

"""Greater than operator."""
const GREATER = OP(">")
SoleLogics.ariety(::typeof(GREATER)) = 2
precedence(::typeof(GREATER)) = 10

"""Greater than or equal operator."""
const GREATER_EQUAL = OP("≥")
SoleLogics.ariety(::typeof(GREATER_EQUAL)) = 2
precedence(::typeof(GREATER_EQUAL)) = 10

"""Lower than operator."""
const LOWER = OP("<")
SoleLogics.ariety(::typeof(LOWER)) = 2
precedence(::typeof(LOWER)) = 10

"""Lower than or equal operator."""
const LOWER_EQUAL = OP("≤")
SoleLogics.ariety(::typeof(LOWER_EQUAL)) = 2
precedence(::typeof(LOWER_EQUAL)) = 10

is_commutative(::typeof(CONJUNCTION)) = true
is_commutative(::typeof(DISJUNCTION)) = true
is_commutative(::typeof(EQUAL)) = true
