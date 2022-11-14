############################################################################################
#       Logic definitions
#     and abstract hierarchy
############################################################################################

"""Root of Logic abstract-types tree"""
abstract type AbstractLogic end

abstract type AbstractPropositionalLogic <: AbstractLogic end
abstract type AbstractModalLogic <: AbstractLogic end

"""A structure representing a certain logic."""
struct Logic{T} <: AbstractLogic
    ops::Operators

    Logic{T}(ops::Operators) where {T} = new{T}(ops)
end

"""
    Logic(s::AbstractString, ops::Operators, alphabet::LetterAlphabet)
    Logic(s::Symbol, ops::Operators, alphabet::LetterAlphabet)
Logic constructors.
A custom name can be provided as AbstractString or Symbol.
"""
Logic(s::AbstractString, ops::Operators) = Logic{Symbol(s)}(ops)
Logic(s::Symbol, ops::Operators) = Logic{s}(ops)

"""
    operators(l::Logic)
Return the operators contained in a certain logic.
"""
operators(l::Logic) = values(l.ops)

"""
    alphabet(l::Logic)
Return the propositional letters contained in a certain logic.
"""

############################################################################################
#       Available logics
############################################################################################

propositional_lops = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION])
"""
Default definition of PL.
"""
const PROPOSITIONAL_LOGIC =
    Logic("PropositionalLogic", propositional_lops)

modal_lops = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION, DIAMOND, BOX])
"""
Default definition of archetypal ML.
"""
const MODAL_LOGIC = Logic("ModalLogic", modal_lops)

"""
Chosen logic, usually utilized whenever a custom argument of type `::AbstractLogic`
is not specified.

Currently this is set to be [`MODAL_LOGIC`](@ref)
"""
const DEFAULT_LOGIC = MODAL_LOGIC
