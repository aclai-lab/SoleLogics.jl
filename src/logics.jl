# Currently, only modal logic is defined for testing purpose
# `alphabet` has to be taken from SoleAlphabets

struct Logic{T,A}
    name::String
    ops::T
    alphabet::A
end
operators(l::Logic) = l.ops
alphabet(l::Logic) = l.alphabet

modal_operators = Operators([CONJUNCTION, DISJUNCTION, IMPLICATION, NEGATION, DIAMOND, BOX])
modal_alphabet = string.(collect('p':'z'))
const MODAL_LOGIC = Logic{Operators, Vector{String}}(
    "Modal Logic",
    modal_operators,
    modal_alphabet
)
