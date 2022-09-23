abstract type AbstractLogic end

abstract type AbstractPropositionalLogic <: AbstractLogic end
abstract type AbstractModalLogic <: AbstractLogic end

struct Logic{T} <: AbstractLogic
    ops::Operators
    alphabet::LetterAlphabet

    Logic{T}(ops::Operators, alphabet::LetterAlphabet) where {T} = new{T}(ops, alphabet)
end

Logic(s::AbstractString, ops::Operators, alphabet::LetterAlphabet) =
    Logic{Symbol(s)}(ops, alphabet)
Logic(s::Symbol, ops::Operators, alphabet::LetterAlphabet) = Logic{s}(ops, alphabet)

operators(l::Logic) = values(l.ops)
alphabet(l::Logic) = l.alphabet

propositional_lops = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION])
propositional_lalphabet = LetterAlphabet(string.(collect('a':'z')))
const PROPOSITIONAL_LOGIC =
    Logic("PropositionalLogic", propositional_lops, propositional_lalphabet)

modal_lops = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION, DIAMOND, BOX])
modal_lalphabet = LetterAlphabet(string.(collect('a':'z')))
const MODAL_LOGIC = Logic("ModalLogic", modal_lops, modal_lalphabet)

const DEFAULT_LOGIC = MODAL_LOGIC
