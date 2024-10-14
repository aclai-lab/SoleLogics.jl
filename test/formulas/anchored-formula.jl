

# @test_nowarn AnchoredFormula(Base.RefValue(logic_int), t1_int)
# @test_nowarn AnchoredFormula(logic_int, p1)
# @test_nowarn AnchoredFormula(logic_int, p1; check_atoms = true)
# @test_nowarn AnchoredFormula(logic_int, p100)
# @test_throws AssertionError AnchoredFormula(logic_int, p100; check_atoms = true)

# f_int = @test_nowarn AnchoredFormula(logic_int, t1_int)
# @test_throws MethodError 1 in f_int
# @test p1 in f_int
# @test p1 in grammar(f_int)
# @test ! (p1_number in f_int)
# @test ! (p100 in f_int)
# @test ! (Atom("1") in f_int)
# @test_throws ErrorException f_int ∨ ⊤
# @test_throws ErrorException ⊥ ∨ f_int
# @test_nowarn ¬(f_int)
# @test_nowarn f_int ∨ f_int
# @test_nowarn ¬(f_int) ∨ f_int
# @test_nowarn p1 ∨ f_int
# @test_nowarn f_int ∨ p1
# @test_nowarn t2_int ∨ f_int
# @test_nowarn f_int ∨ t2_int
# # @test atoms(f_int ∨ (p1 ∨ p100)) == [p1, p1, p100]
# @test unique(atoms(f_int ∨ (p1 ∨ p100))) == [p1, p100]
# @test all(isa.(atoms(f_int ∨ (p1 ∨ p100)), atomstype(logic(f_int))))
# f_conj_int = @test_throws AssertionError SoleLogics.composeformulas(CONJUNCTION, (f_int, f_int, f_int))
# f_conj_int = @test_nowarn SoleLogics.composeformulas(CONJUNCTION, (f_int, f_int))
# f_conj_int = @test_nowarn CONJUNCTION(f_int, f_int, f_int, f_int)
# f_conj_int = @test_nowarn CONJUNCTION(f_int, f_int, f_int)
# @test_nowarn DISJUNCTION(f_int, f_int, f_conj_int)
# @test_nowarn CONJUNCTION(f_int, f_int, p1)
# @test_nowarn CONJUNCTION(p1, f_int, p1)
# @test_nowarn CONJUNCTION(t2_int, f_int, p1)
# @test_nowarn CONJUNCTION(f_int, t2_int, p1)
# @test typeof(¬(f_int)) == typeof(f_int)
# @test_nowarn ∧((¬(f_int), f_int),)
# # @test promote_type(typeof(f_int), typeof(t2_int)) == typeof(f_int)
# @test_nowarn ∧((¬(f_int), f_int),)
# @test_nowarn ∧((¬(f_int), t2_int),)
# @test_nowarn ∧((t2_int, ¬(f_int)),)
# @test_nowarn f_int(p1 ∧ p100)
# # @test f_int(p1 ∧ p100) isa AnchoredFormula
# @test_throws ErrorException f_int(p1 ∧ p100 ∧ p1_float)
# @test_throws ErrorException f_int(⊥ ∨ (p1 ∧ p100 ∧ p2 ∧ ⊤))
# anch_φ_int = f_int(p1 ∧ p100 ∧ p2)
# anch2_φ_int = f_int(p1 ∧ p100 → p2)

# for i in 1:10
#     _tdict = TruthDict(Dict([p => rand([true, false]) for p in unique(atoms(anch_φ_int))]))
#     # i == 1 && println(_tdict)
#     check(anch_φ_int, _tdict) && @test all(istop, collect(values(_tdict.truth)))
#     !check(anch_φ_int, _tdict) && @test !all(istop, collect(values(_tdict.truth)))
#     check(anch2_φ_int, _tdict)

#     @test_nowarn _tdict[anch_φ_int]
#     @test_nowarn anch_φ_int(_tdict)
#     @test anch_φ_int(_tdict) == _tdict[anch_φ_int]
# end

# tdict = TruthDict(Dict([p => true for p in unique(atoms(anch_φ_int))]))
# @test check(anch_φ_int, tdict)

# tdict = TruthDict(Dict([p => false for p in unique(atoms(anch_φ_int))]))
# @test !check(anch_φ_int, tdict)

# @test check(anch_φ_int, DefaultedTruthDict([], true))
# @test check(anch_φ_int, DefaultedTruthDict(true))
# @test !check(anch_φ_int, DefaultedTruthDict(false))


# # parsebaseformula ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# using SoleLogics: parsebaseformula

# @test_throws ErrorException parsebaseformula("")
# @test_broken parsebaseformula("⊤")
# @test_broken parsebaseformula("⊤ ∧ ⊤")
# @test_broken parsebaseformula("⊤ ∧ p")
# @test_broken parsebaseformula("⊥ ∧ □¬((p∧¬q)→r)")
# @test_broken parsebaseformula("□¬((p∧¬q)→r) ∧ ⊤")
# @test_broken parsebaseformula("⊤ ∧ (⊥∧¬⊤→⊤)")
# @test_nowarn parsebaseformula("□¬((p∧¬q)→r)")

# @test_nowarn parsebaseformula("p")
# @test_nowarn ¬parsebaseformula("p")
# @test_nowarn ¬parsebaseformula("p", propositionallogic())

# @test_nowarn ¬parseformula("p")
# @test_nowarn ¬parseformula("(s∧z)", propositionallogic())

# @test operatorstype(
#     logic(parsebaseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <: SoleLogics.BaseModalConnectives
# @test !(operatorstype(
#     logic(parsebaseformula("¬p∧q∧(¬s∧¬z)", [BOX]))) <:
#         SoleLogics.BasePropositionalConnectives)
# @test !(operatorstype(logic(
#     parsebaseformula("¬p∧q∧(¬s∧¬z)", modallogic()))) <:
#         SoleLogics.BasePropositionalConnectives)
# @test (@test_nowarn operatorstype(
#     logic(parsebaseformula("¬p∧q∧(¬s∧¬z)"))) <: SoleLogics.BasePropositionalConnectives)

# @test alphabet(logi =#c(parsebaseformula("p→q"))) == AlphabetOfAny{String}()
