#################################
#       Logic definitions       #
#     and abstract hierarchy    #
#################################

"""Root of Logic abstract-types tree"""
abstract type AbstractLogic end

abstract type AbstractPropositionalLogic <: AbstractLogic end
abstract type AbstractModalLogic <: AbstractLogic end

"""A structure representing a certain logic."""
struct Logic{T} <: AbstractLogic
    ops::Operators
    alphabet::LetterAlphabet

    Logic{T}(ops::Operators, alphabet::LetterAlphabet) where {T} = new{T}(ops, alphabet)
end


"""
    Logic(s::AbstractString, ops::Operators, alphabet::LetterAlphabet)
    Logic(s::Symbol, ops::Operators, alphabet::LetterAlphabet)
Logic constructors.
A custom name can be provided as AbstractString or Symbol.
"""
Logic(s::AbstractString, ops::Operators, alphabet::LetterAlphabet) =
    Logic{Symbol(s)}(ops, alphabet)
Logic(s::Symbol, ops::Operators, alphabet::LetterAlphabet) = Logic{s}(ops, alphabet)

"""
    operators(l::Logic)
Return the operators contained in a certain logic.
"""
operators(l::Logic) = values(l.ops)

"""
    alphabet(l::Logic)
Return the propositional letters contained in a certain logic.
"""
alphabet(l::Logic) = l.alphabet

#################################
#       Available logics        #
#################################

propositional_lops = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION])
propositional_lalphabet = LetterAlphabet(string.(collect('a':'z')))
"""
Default definition of PL.
"""
const PROPOSITIONAL_LOGIC =
    Logic("PropositionalLogic", propositional_lops, propositional_lalphabet)

modal_lops = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION, DIAMOND, BOX])
modal_lalphabet = LetterAlphabet(string.(collect('a':'z')))
"""
Default definition of archetypal ML.
"""
const MODAL_LOGIC = Logic("ModalLogic", modal_lops, modal_lalphabet)

"""
Chosen logic, usually utilized whenever a custom argument of type `::AbstractLogic`
is not specified.

Currently this is set to be [`MODAL_LOGIC`](@ref)
"""
const DEFAULT_LOGIC = MODAL_LOGIC
