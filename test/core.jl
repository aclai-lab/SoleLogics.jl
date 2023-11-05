# julia
# using Revise
# using Test
# using SoleLogics

using SoleLogics: Literal, dual, LeftmostConjunctiveForm, LeftmostDisjunctiveForm
using SoleLogics: CNF, DNF

# @testset "General" begin

p1 = @test_nowarn Atom(1)
p2 = @test_nowarn Atom(2)
p100 = @test_nowarn Atom(100)
@test_nowarn Atom{Int}(1)
p1_float = @test_nowarn Atom{Float64}(1.0)
p1_number_float = @test_nowarn Atom{Number}(1.4)
p1_number = @test_nowarn Atom{Number}(1)
p_string = @test_nowarn Atom{String}("1")

@test Atom(Atom(1)) == Atom(1)
@test_throws AssertionError Atom(parsetree("¬p"))
@test_throws AssertionError Atom(¬)

@test arity(p1) == 0
@test Atom(1.0) != Atom(1)
@test atomstype(SoleLogics.AbstractAlphabet{Int}) == Atom{Int}

@test_nowarn ExplicitAlphabet(Atom.([1,2]))
@test_nowarn ExplicitAlphabet([1,2])
@test Atom(1) in ExplicitAlphabet([1,2])
@test Atom(2) in ExplicitAlphabet([1,2])
@test !(Atom(3) in ExplicitAlphabet([1,2]))

@test_nowarn ExplicitAlphabet(1:10)
alphabet_int = @test_nowarn ExplicitAlphabet(Atom.(1:10))
@test atomstype(alphabet_int) == @test_nowarn Atom{Int}
@test_nowarn ExplicitAlphabet(Atom{Number}.(1:10))
alphabet_number = @test_nowarn ExplicitAlphabet{Number}(Atom.(1:10))
@test atoms(alphabet_number) isa Vector{Atom{Number}}

@test alphabet_int(1) isa Atom{Int}
@test alphabet_number(1) isa Atom{Number}
@test alphabet_number(Float64(1.0)) isa Atom{Number}

p_vec_number = @test_nowarn Atom{Vector{<:Number}}([1])
p_vec_int = @test_nowarn Atom{Vector{Int}}([1])
@test_throws MethodError Atom{<:Vector{Int}}([1.0])
p_vec = @test_nowarn Atom{Vector}([1.0])

alphabet_mixed = AlphabetOfAny{Union{String,Number}}()
@test (@test_logs (:warn,) 1 in alphabet_mixed)
@test (@test_logs (:warn,) "1" in alphabet_mixed)

# @test_throws ErrorException "My string" in AlphabetOfAny{String}()
# @test_throws ErrorException 1 in AlphabetOfAny{Number}()
@test Atom("My string") in AlphabetOfAny{String}()
@test Atom(1) in AlphabetOfAny{Number}()
@test Atom(1.0) in AlphabetOfAny{Number}()
@test !(Atom(1) in AlphabetOfAny{String}())

@test_nowarn convert(SyntaxTree, p1)
@test_nowarn SyntaxTree(p1)
@test_throws MethodError SyntaxBranch(p1)
@test_throws MethodError SyntaxBranch(p1, ())
@test_throws MethodError SyntaxBranch(p100, ())

t1_int = p1
t100_int = p100
@test tokenstype(t1_int) == tokentype(t1_int)
@test_throws MethodError SyntaxBranch(3, ())

@test p1 in t1_int

@test_nowarn SyntaxBranch(¬, (p1,))
@test_nowarn SyntaxBranch(¬, p1)
@test_nowarn SyntaxBranch(¬, t1_int)
t1n_int = @test_nowarn SyntaxBranch(¬, (t1_int,))
@test p1 in t1n_int
@test (¬) in t1n_int
@test tokenstype(t1n_int) == Union{typeof(¬),tokentype(t1_int)}
@test_nowarn SyntaxBranch(∧, (t1_int, t1n_int))
t2_int = @test_nowarn SyntaxBranch(∧, (t1_int, t1_int))
@test tokenstype(SyntaxBranch(∧, (t2_int, t1n_int))) == Union{typeof(∧),tokenstype(t1n_int)}

grammar_int = SoleLogics.CompleteFlatGrammar(alphabet_int, SoleLogics.BASE_OPERATORS)

@test Atom(1) in grammar_int
@test ! (Atom(11) in grammar_int)
@test ! (Atom(1.0) in grammar_int)
@test t1_int in grammar_int
@test ! (t100_int in grammar_int)
@test t1_int in alphabet(grammar_int)

@test_nowarn formulas(grammar_int; maxdepth = 2, nformulas = 100)

@test repr(SoleLogics.BASE_LOGIC) == repr(propositionallogic())

logic_int = BaseLogic(grammar_int, SoleLogics.BooleanAlgebra())

@test_throws MethodError "aoeu" in propositionallogic()
@test Atom("aoeu") in propositionallogic()
@test ! (Atom(1) in propositionallogic())

@test_nowarn AnchoredFormula(Base.RefValue(logic_int), t1_int)
f_int = @test_nowarn AnchoredFormula(logic_int, t1_int)
@test_nowarn AnchoredFormula(logic_int, p1)
@test_nowarn AnchoredFormula(logic_int, p1; check_atoms = true)
@test_nowarn AnchoredFormula(logic_int, p100)
@test_throws AssertionError AnchoredFormula(logic_int, p100; check_atoms = true)

@test_throws MethodError 1 in f_int
@test p1 in f_int
@test p1 in grammar(f_int)
@test ! (p1_number in f_int)
@test ! (p100 in f_int)
@test ! (Atom("1") in f_int)


t2_int = @test_nowarn ¬(t1_int)
@test_nowarn ⊥()
@test_nowarn ¬(p1)
@test_nowarn ∨(p1, p1)
@test_nowarn p1 ∨ p1_number
@test_nowarn ∨(p1, p1, p1_number)
@test_nowarn ¬(∨(p1, p1, p1_number))
@test_nowarn p1 ∨ p100
@test_nowarn ¬(p1) ∨ p1
@test_nowarn ¬(p1) ∨ ¬(p1)
@test_nowarn SyntaxTree(⊤)
@test_nowarn ⊤ ∨ ⊤
@test_nowarn p1 ∨ ⊤
@test_nowarn ⊥ ∨ p1 ∨ ⊤

@test atomstype(p1 ∨ p1_number) != Atom{Int}
@test atomstype(p1 ∨ p1_number_float) == Union{Atom{Int}, Atom{Number}}
@test atomstype(p1 ∨ p1_float) == Union{Atom{Int}, Atom{Float64}}
@test atoms(p1 ∨ p100) == [p1, p100]

@test_nowarn p1 ∨ t2_int
@test_nowarn t2_int ∨ p1
@test_nowarn t2_int ∨ t2_int
@test_nowarn ⊥ ∨ t2_int ∨ ⊤
@test_nowarn t2_int ∨ ⊤
@test_nowarn ¬(t2_int) ∧ t2_int
@test_nowarn ¬(¬(t2_int) ∧ t2_int)
@test_nowarn ∧(¬(t2_int), t2_int)
@test_nowarn ∧((¬(t2_int), t2_int),)
@test_nowarn ∧(¬(t2_int), t2_int, ¬(t2_int) ∧ t2_int)
@test_nowarn ¬(¬(p1))

@test_throws ErrorException f_int ∨ ⊤
@test_throws ErrorException ⊥ ∨ f_int
@test_nowarn ¬(f_int)
@test_nowarn f_int ∨ f_int
@test_nowarn ¬(f_int) ∨ f_int
@test_nowarn p1 ∨ f_int
@test_nowarn f_int ∨ p1
@test_nowarn t2_int ∨ f_int
@test_nowarn f_int ∨ t2_int
# @test atoms(f_int ∨ (p1 ∨ p100)) == [p1, p1, p100]
@test unique(atoms(f_int ∨ (p1 ∨ p100))) == [p1, p100]
@test all(isa.(atoms(f_int ∨ (p1 ∨ p100)), atomstype(logic(f_int))))

f_conj_int = @test_throws AssertionError SoleLogics.joinformulas(CONJUNCTION, (f_int, f_int, f_int))
f_conj_int = @test_nowarn SoleLogics.joinformulas(CONJUNCTION, (f_int, f_int))
f_conj_int = @test_nowarn CONJUNCTION(f_int, f_int, f_int, f_int)
f_conj_int = @test_nowarn CONJUNCTION(f_int, f_int, f_int)
@test_nowarn DISJUNCTION(f_int, f_int, f_conj_int)
@test_nowarn CONJUNCTION(f_int, f_int, p1)
@test_nowarn CONJUNCTION(p1, f_int, p1)
@test_nowarn CONJUNCTION(t2_int, f_int, p1)
@test_nowarn CONJUNCTION(f_int, t2_int, p1)
@test_nowarn CONJUNCTION(t2_int, t2_int)
@test_nowarn CONJUNCTION(t2_int, t2_int, p1)
@test_nowarn CONJUNCTION(t2_int, p1, p1)
@test_nowarn CONJUNCTION(p1, p1)
@test_nowarn CONJUNCTION(p1, p1, p1)

@test_nowarn p1 ∨ t2_int
@test typeof(¬(f_int)) == typeof(f_int)
@test_nowarn ∧((¬(f_int), f_int),)

# @test promote_type(typeof(f_int), typeof(t2_int)) == typeof(f_int)
# @test promote_type(AnchoredFormula, SyntaxBranch) == AnchoredFormula
# @test promote_type(SyntaxBranch, AnchoredFormula) == AnchoredFormula

@test_nowarn ∧((¬(f_int), f_int),)
@test_nowarn ∧((¬(f_int), t2_int),)
@test_nowarn ∧((t2_int, ¬(f_int)),)

@test_nowarn f_int(p1 ∧ p100)
@test f_int(p1 ∧ p100) isa AnchoredFormula
@test_throws ErrorException f_int(p1 ∧ p100 ∧ p1_float)
@test_throws ErrorException f_int(⊥ ∨ (p1 ∧ p100 ∧ p2 ∧ ⊤))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


@test_nowarn TruthDict()
@test_nowarn TruthDict([])
@test_throws ErrorException TruthDict((2,3),)
@test_nowarn TruthDict((2,0),)
@test_nowarn TruthDict((2,true),)
@test_nowarn TruthDict((p1, true),)
@test_nowarn TruthDict([(p1, true),])
@test_nowarn TruthDict(p1 => true)
@test_nowarn TruthDict([p1 => true])
@test_nowarn TruthDict(Dict([p1 => true]))

anch_φ_int = f_int(p1 ∧ p100 ∧ p2)
anch2_φ_int = f_int(p1 ∧ p100 → p2)

for i in 1:10
    _tdict = TruthDict(Dict([p => rand([true, false]) for p in unique(atoms(anch_φ_int))]))
    # i == 1 && println(_tdict)
    check(anch_φ_int, _tdict) && @test all(istop, collect(values(_tdict.truth)))
    !check(anch_φ_int, _tdict) && @test !all(istop, collect(values(_tdict.truth)))
    check(anch2_φ_int, _tdict)
end

tdict = TruthDict(Dict([p => true for p in unique(atoms(anch_φ_int))]))
@test check(anch_φ_int, tdict)

tdict = TruthDict(Dict([p => false for p in unique(atoms(anch_φ_int))]))
@test !check(anch_φ_int, tdict)

@test check(anch_φ_int, DefaultedTruthDict([], true))
@test check(anch_φ_int, DefaultedTruthDict(true))
@test !check(anch_φ_int, DefaultedTruthDict(false))

φ_int = (⊥ ∨ (p1 ∧ p100 ∧ p2 ∧ ⊤))

for i in 1:10
    _tdict = TruthDict(Dict([p => rand([true, false]) for p in unique(atoms(φ_int))]))
    check(φ_int, _tdict) && @test all(istop, collect(values(_tdict.truth)))
    !check(φ_int, _tdict) && @test !all(istop, collect(values(_tdict.truth)))

    @test_nowarn _tdict[φ_int]
    @test_nowarn φ_int(_tdict)
    @test φ_int(_tdict) == _tdict[φ_int]
end

tdict = TruthDict(Dict([p => true for p in unique(atoms(φ_int))]))
@test check(φ_int, tdict)

tdict = TruthDict(Dict([p => false for p in unique(atoms(φ_int))]))
@test !check(φ_int, tdict)

@test check(φ_int, DefaultedTruthDict([], true))
@test check(φ_int, DefaultedTruthDict(true))
@test !check(φ_int, DefaultedTruthDict(false))

@test_nowarn propositionallogic(; operators = SoleLogics.Operator[])
emptylogic = @test_nowarn propositionallogic(; operators = SoleLogics.Operator[], alphabet = ExplicitAlphabet([]))
@test length(formulas(emptylogic, maxdepth = 2, nformulas = 2)) == 0


@test propositionallogic() isa SoleLogics.BasePropositionalLogic
@test propositionallogic(; operators = [¬, ∨]) isa SoleLogics.BasePropositionalLogic

@test_throws AssertionError propositionallogic(; operators = [¬, ∨])(¬ p1)
@test_nowarn propositionallogic(; operators = [¬, ∨])(¬ p_string)
@test propositionallogic(; alphabet = ["p", "q"]) isa SoleLogics.BasePropositionalLogic

@test modallogic() isa SoleLogics.BaseModalLogic
@test (@test_logs (:warn,) modallogic(; operators = [¬, ∨]) isa SoleLogics.BasePropositionalLogic)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

include("check/propositional.jl")

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

@test_nowarn joinformulas(∧, (lfdf1, lfdf1, lfdf2))
@test_nowarn joinformulas(∧, lfdf1, lfdf1, lfdf2)
@test_nowarn joinformulas(∨, (lfdf1, lfdf1, lfdf2))
@test_nowarn joinformulas(∨, lfdf1, lfdf1, lfdf2)

@test joinformulas(∧, (lfdf1, lfdf1, lfdf2)) isa LeftmostConjunctiveForm
@test joinformulas(∧, lfdf1, lfdf1, lfdf2) isa LeftmostConjunctiveForm
@test joinformulas(∨, (lfdf1, lfdf1, lfdf2)) isa LeftmostDisjunctiveForm
@test joinformulas(∨, lfdf1, lfdf1, lfdf2) isa LeftmostDisjunctiveForm

@test_nowarn ∧(lfdf1, lfdf1, lfdf2)
@test_nowarn ∨(lfdf1, lfdf1, lfdf2)

@test ∧(lfdf1, lfdf1, lfdf2) isa LeftmostConjunctiveForm
@test ∨(lfdf1, lfdf1, lfdf2) isa LeftmostDisjunctiveForm

@test ∧(cnf1, cnf1, cnf1) isa CNF

@test !(dnf1 ∧ lfdf1 isa DNF)
@test dnf1 ∨ lfdf1 isa DNF
@test dnf1 ∨ lfdf1 ∨ lfdf1 isa DNF
@test lfdf1 ∨ dnf1 ∨ lfdf1 ∨ lfdf1 isa DNF
@test lfdf1 ∨ dnf1 isa DNF
@test dnf1 ∨ lfdf1 isa DNF
