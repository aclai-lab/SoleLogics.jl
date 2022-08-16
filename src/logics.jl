# Currently, only modal logic is defined for testing purpose
# `alphabet` has to be taken from SoleAlphabets

struct Logic
    name::String
    ops::Operators
    alphabet::Vector{String}
end
operators(l::Logic) = values(l.ops)
alphabet(l::Logic) = l.alphabet

modal_operators = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION, DIAMOND, BOX])
modal_alphabet = string.(collect('p':'z'))
const MODAL_LOGIC = Logic(
    "Modal Logic",
    modal_operators,
    modal_alphabet
)
