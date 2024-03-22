using SoleLogics: Literal, dual, LeftmostConjunctiveForm, LeftmostDisjunctiveForm
using SoleLogics: CNF, DNF

p1 = @test_nowarn Atom(1)
p2 = @test_nowarn Atom(2)
p100 = @test_nowarn Atom(100)
p1_float = @test_nowarn Atom{Float64}(1.0)
p1_number_float = @test_nowarn Atom{Number}(1.4)
p1_number = @test_nowarn Atom{Number}(1)
p_string = @test_nowarn Atom{String}("1")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Literal ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

l1 = @test_nowarn Literal(true, p1)
l1_neg = @test_nowarn Literal(false, p1)

l2 = @test_nowarn Literal(true, p2)
l2_neg = @test_nowarn dual(l2)

l100 = @test_nowarn Literal(true, p100)
l100_neg = @test_nowarn Literal(false, p100)

l1_float = @test_nowarn Literal(true, p1_float)
l1_float_neg = @test_nowarn dual(l1_float)

l1_number_float = @test_nowarn Literal(true, p1_number_float)
l1_number_float_neg = @test_nowarn Literal(false, p1_number_float)

l1_number = @test_nowarn Literal(true, p1_number)
l1_number_neg = @test_nowarn dual(l1_number)

l_string = @test_nowarn Literal(true, p_string)
l_string_neg = @test_nowarn Literal(false, p_string)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Syntax Forms ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lfcf1 = @test_nowarn LeftmostConjunctiveForm{Literal}([l1, l2_neg, l1_float])
lfcf2 = @test_nowarn LeftmostConjunctiveForm{Literal}([l2, l100_neg, l1_number_neg])

lfdf1 = @test_nowarn LeftmostDisjunctiveForm{Literal}([l1_number_float, l_string_neg])
lfdf2 = @test_nowarn LeftmostDisjunctiveForm{Literal}([l1_number_float, l_string_neg])

cnf1 = @test_nowarn CNF{Literal}([lfdf1, lfdf2])
dnf1 = @test_nowarn DNF{Literal}([lfcf1, lfcf2])

cnf1 = @test_nowarn CNF([lfdf1, lfdf2])
dnf1 = @test_nowarn DNF([lfcf1, lfcf2])

@test (lfdf1 ∧ lfdf2) isa LeftmostConjunctiveForm
@test (lfdf1 ∨ lfdf2) isa LeftmostDisjunctiveForm

@test_nowarn ∧(lfdf1, lfdf1, lfdf2)
@test_nowarn ∨(lfdf1, lfdf1, lfdf2)

@test ∧(lfdf1, lfdf1, lfdf2) isa LeftmostConjunctiveForm
@test ∨(lfdf1, lfdf1, lfdf2) isa LeftmostDisjunctiveForm

@test_nowarn SoleLogics.composeformulas(∧, (lfdf1, lfdf1, lfdf2))
@test_nowarn SoleLogics.composeformulas(∧, lfdf1, lfdf1, lfdf2)
@test_nowarn SoleLogics.composeformulas(∨, (lfdf1, lfdf1, lfdf2))
@test_nowarn SoleLogics.composeformulas(∨, lfdf1, lfdf1, lfdf2)

@test SoleLogics.composeformulas(∧, (lfdf1, lfdf1, lfdf2)) isa LeftmostConjunctiveForm
@test SoleLogics.composeformulas(∧, lfdf1, lfdf1, lfdf2) isa LeftmostConjunctiveForm
@test SoleLogics.composeformulas(∨, (lfdf1, lfdf1, lfdf2)) isa LeftmostDisjunctiveForm
@test SoleLogics.composeformulas(∨, lfdf1, lfdf1, lfdf2) isa LeftmostDisjunctiveForm

@test_nowarn SoleLogics.composeformulas(∧, (lfcf1, lfdf1))
@test_nowarn SoleLogics.composeformulas(∧, lfcf1, lfdf1)
@test_nowarn SoleLogics.composeformulas(∨, (lfcf1, lfdf1))
@test_nowarn SoleLogics.composeformulas(∨, lfcf1, lfdf1)

@test_nowarn SoleLogics.composeformulas(∧, (lfcf1, lfcf1, lfdf1))
@test_nowarn SoleLogics.composeformulas(∧, lfcf1, lfcf1, lfdf1)
@test_nowarn SoleLogics.composeformulas(∨, (lfcf1, lfcf1, lfdf1))
@test_nowarn SoleLogics.composeformulas(∨, lfcf1, lfcf1, lfdf1)

@test_nowarn SoleLogics.composeformulas(∧, (lfcf1, lfdf1, lfdf1))
@test_nowarn SoleLogics.composeformulas(∧, lfcf1, lfdf1, lfdf1)
@test_nowarn SoleLogics.composeformulas(∨, (lfcf1, lfdf1, lfdf1))
@test_nowarn SoleLogics.composeformulas(∨, lfcf1, lfdf1, lfdf1)

@test_nowarn ∧(lfcf1, lfdf1)
@test_nowarn ∨(lfcf1, lfdf1)

@test_nowarn ∧(lfcf1, lfcf1, lfdf1)
@test_nowarn ∨(lfcf1, lfcf1, lfdf1)

@test_nowarn ∧(lfcf1, lfdf1, lfdf1)
@test_nowarn ∨(lfcf1, lfdf1, lfdf1)

@test_nowarn ∧(lfdf1, lfdf1, lfdf2)
@test_nowarn ∨(lfdf1, lfdf1, lfdf2)

@test ∧(lfdf1, lfdf1, lfdf2) isa LeftmostConjunctiveForm
@test ∨(lfdf1, lfdf1, lfdf2) isa LeftmostDisjunctiveForm

@test ∧(cnf1, cnf1, cnf1) isa CNF

@test_nowarn LeftmostDisjunctiveForm(tree(dnf1 ∨ lfdf1))

@test_broken DNF(tree(dnf1 ∨ lfdf1)) isa DNF
@test_broken DNF{Literal}(tree(dnf1 ∨ lfdf1)) isa DNF

@test !(dnf1 ∧ lfdf1 isa DNF)
@test_broken dnf1 ∨ lfdf1 isa DNF
@test_broken dnf1 ∨ lfdf1 ∨ lfdf1 isa DNF
@test_broken lfdf1 ∨ dnf1 ∨ lfdf1 ∨ lfdf1 isa DNF
@test_broken lfdf1 ∨ dnf1 isa DNF
@test_broken dnf1 ∨ lfdf1 isa DNF


@test LeftmostConjunctiveForm(parseformula.(["¬p", "q", "¬r"])) ∧ LeftmostConjunctiveForm(parseformula.(["¬p", "q", "¬r"])) isa LeftmostConjunctiveForm
@test LeftmostDisjunctiveForm(parseformula.(["¬p", "q", "¬r"])) ∨ LeftmostDisjunctiveForm(parseformula.(["¬p", "q", "¬r"])) isa LeftmostDisjunctiveForm



@test cnf(parseformula("⊤")) isa CNF
@test cnf(parseformula("p")) isa CNF
@test cnf(parseformula("p ∧ q")) isa CNF
@test cnf(parseformula("p ∧ p ∧ q")) isa CNF
@test cnf(parseformula("p ∧ p ∨ q")) isa CNF
@test cnf(parseformula("(p ∧ ¬s) ∨ (¬q ∧ p)")) isa CNF
@test cnf(parseformula("(p ∧ ¬s) ∨ (¬q ∧ p)")) isa CNF

alpha = Atom.(["p", "q", "r"])
@test all(isa.([cnf(randformula(4, alpha, [∨, ∧, ¬])) for i in 1:100], CNF))

@test_nowarn [(cnf(randformula(4, alpha, [∨, ∧, ¬])) |> cnf) for i in 1:10]

@test_nowarn cnf(normalize(parseformula("(p ∧ ¬s) ∨ (¬q ∧ p)"); profile = :nnf); allow_atom_flipping = true)

for i in 1:50
    local φ = randformula(4, alpha, [∨, ∧, ¬])
    φ2 = cnf(φ)
    _tdict = TruthDict(Dict([p => rand([true, false]) for p in alpha]))
    # i == 1 && println(_tdict)
    @test check(φ, _tdict) == check(φ2, _tdict)
end
