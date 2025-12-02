
@testset "randlogiset" begin

_myrng = 238
_alphabet = ExplicitAlphabet(Atom.('p':'z'))
_operators = [CONJUNCTION, DISJUNCTION, NEGATION, DIAMOND, BOX]
_operator_weights = [1, 2, 2, 5, 1]

_nformulas = 5

_earlystoppingthreshold = 0.3
_formulas = [
    randformula(
        _myrng,
        _alphabet,
        _operators;
        opweights=_operator_weights,
        earlystoppingthreshold=_earlystoppingthreshold
    )
    for _ in 1:_nformulas
]

_conjunction = CONJUNCTION(_formulas...)

mylogisets, _ = randlogiset(_myrng, ((_conjunction,)), 100; silent=false, checksat=true)

end
