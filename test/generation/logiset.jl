
@testset "randlogiset" begin

_myrng = 42
_alphabet = ExplicitAlphabet(Atom.('p':'z'))
_operators = [CONJUNCTION, DISJUNCTION, NEGATION, DIAMOND, BOX]

_nformulas = 10

_earlystoppingthreshold = 0.2
_formulas = [
    randformula(
        _myrng,
        _alphabet,
        _operators;
        earlystoppingthreshold =_earlystoppingthreshold
    )
    for _ in 1:_nformulas
]

_conjunction = CONJUNCTION(_formulas...)

mylogiset = randlogiset(_myrng, ((_conjunction,)), 5; silent=false)

end
