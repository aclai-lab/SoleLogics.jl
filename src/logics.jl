abstract type AbstractLogic end
abstract type CrispLogic <: AbstractLogic end
abstract type FuzzyLogic <: AbstractLogic end

struct Logic <: AbstractLogic
    name::String
    ops::Operators
    alphabet::LetterAlphabet
end
operators(l::Logic) = values(l.ops)
alphabet(l::Logic) = l.alphabet

modal_operators = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION, DIAMOND, BOX])
modal_alphabet = string.(collect('p':'z'))
const MODAL_LOGIC = Logic("Modal Logic", modal_operators, modal_alphabet)
