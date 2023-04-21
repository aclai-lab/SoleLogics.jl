# julia
# using Revise
# using Test
# using SoleLogics

# @testset "General" begin

p1 = @test_nowarn Proposition(1)
p2 = @test_nowarn Proposition(2)
p100 = @test_nowarn Proposition(100)
@test_nowarn Proposition{Int}(1)
p1_float = @test_nowarn Proposition{Float64}(1.0)
p1_number_float = @test_nowarn Proposition{Number}(1.4)
p1_number = @test_nowarn Proposition{Number}(1)
p_string = @test_nowarn Proposition{String}("1")

@test Proposition(Proposition(1)) == Proposition(1)
@test_throws AssertionError Proposition(parseformulatree("¬p"))
@test_throws AssertionError Proposition(¬)

@test arity(p1) == 0
@test Proposition(1.0) != Proposition(1)
@test propositionstype(SoleLogics.AbstractAlphabet{Int}) == Proposition{Int}

@test_nowarn ExplicitAlphabet(Proposition.([1,2]))
@test_nowarn ExplicitAlphabet([1,2])
@test Proposition(1) in ExplicitAlphabet([1,2])
@test Proposition(2) in ExplicitAlphabet([1,2])
@test !(Proposition(3) in ExplicitAlphabet([1,2]))

@test_nowarn ExplicitAlphabet(1:10)
alphabet_int = @test_nowarn ExplicitAlphabet(Proposition.(1:10))
@test propositionstype(alphabet_int) == @test_nowarn Proposition{Int}
@test_nowarn ExplicitAlphabet(Proposition{Number}.(1:10))
alphabet_number = @test_nowarn ExplicitAlphabet{Number}(Proposition.(1:10))
@test propositions(alphabet_number) isa Vector{Proposition{Number}}

@test alphabet_int(1) isa Proposition{Int}
@test alphabet_number(1) isa Proposition{Number}
@test alphabet_number(Float64(1.0)) isa Proposition{Number}

p_vec_number = @test_nowarn Proposition{Vector{<:Number}}([1])
p_vec_int = @test_nowarn Proposition{Vector{Int}}([1])
@test_throws MethodError Proposition{<:Vector{Int}}([1.0])
p_vec = @test_nowarn Proposition{Vector}([1.0])

alphabet_mixed = AlphabetOfAny{Union{String,Number}}()
@test (@test_logs (:warn,) 1 in alphabet_mixed)
@test (@test_logs (:warn,) "1" in alphabet_mixed)

# @test_throws ErrorException "My string" in AlphabetOfAny{String}()
# @test_throws ErrorException 1 in AlphabetOfAny{Number}()
@test Proposition("My string") in AlphabetOfAny{String}()
@test Proposition(1) in AlphabetOfAny{Number}()
@test Proposition(1.0) in AlphabetOfAny{Number}()
@test !(Proposition(1) in AlphabetOfAny{String}())

@test_nowarn convert(SyntaxTree, p1)
@test_nowarn SyntaxTree(p1)
@test_nowarn SyntaxTree{typeof(p1)}(p1)
@test_nowarn SyntaxTree(p1)
t1_int = @test_nowarn SyntaxTree(p1, ())
t100_int = @test_nowarn SyntaxTree(p100, ())
@test tokenstype(t1_int) == tokentype(t1_int)
@test_throws MethodError SyntaxTree(3, ())

@test p1 in t1_int

@test_nowarn SyntaxTree(¬, (p1,))
@test_nowarn SyntaxTree(¬, p1)
@test_nowarn SyntaxTree(¬, t1_int)
t1n_int = @test_nowarn SyntaxTree(¬, (t1_int,))
@test p1 in t1n_int
@test (¬) in t1n_int
@test tokenstype(t1n_int) == Union{typeof(¬),tokentype(t1_int)}
@test_nowarn SyntaxTree(∧, (t1_int, t1n_int))
t2_int = @test_nowarn SyntaxTree(∧, (t1_int, t1_int))
@test tokenstype(SyntaxTree(∧, (t2_int, t1n_int))) == Union{typeof(∧),tokenstype(t1n_int)}

grammar_int = SoleLogics.CompleteFlatGrammar(alphabet_int, SoleLogics.BASE_OPERATORS)

@test Proposition(1) in grammar_int
@test ! (Proposition(11) in grammar_int)
@test ! (Proposition(1.0) in grammar_int)
@test t1_int in grammar_int
@test ! (t100_int in grammar_int)
@test_throws ErrorException t1_int in alphabet(grammar_int)

@test_nowarn formulas(grammar_int; maxdepth = 2, nformulas = 100)

@test repr(SoleLogics.BASE_LOGIC) == repr(propositionallogic())

logic_int = BaseLogic(grammar_int, SoleLogics.BooleanAlgebra())

@test_throws MethodError "aoeu" in propositionallogic()
@test Proposition("aoeu") in propositionallogic()
@test ! (Proposition(1) in propositionallogic())

@test_nowarn Formula(Base.RefValue(logic_int), t1_int)
f_int = @test_nowarn Formula(logic_int, t1_int)
@test_nowarn Formula(logic_int, p1)
@test_nowarn Formula(logic_int, p1; check_propositions = true)
@test_nowarn Formula(logic_int, p100)
@test_throws AssertionError Formula(logic_int, p100; check_propositions = true)

@test_throws MethodError 1 in f_int
@test p1 in f_int
@test p1 in grammar(f_int)
@test ! (p1_number in f_int)
@test ! ( p100 in f_int )
@test ! ( Proposition("1") in f_int )


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

@test propositionstype(p1 ∨ p1_number) != Proposition{Int}
@test propositionstype(p1 ∨ p1_number_float) == Union{Proposition{Int}, Proposition{Number}}
@test propositionstype(p1 ∨ p1_float) == Union{Proposition{Int}, Proposition{Float64}}
@test propositions(p1 ∨ p100) == [p1, p100]

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

@test_nowarn f_int ∨ ⊤
@test_nowarn ⊥ ∨ f_int
@test_nowarn ¬(f_int)
@test_nowarn f_int ∨ f_int
@test_nowarn ¬(f_int) ∨ f_int
@test_nowarn p1 ∨ f_int
@test_nowarn f_int ∨ p1
@test_nowarn t2_int ∨ f_int
@test_nowarn f_int ∨ t2_int
@test propositions(f_int ∨ (p1 ∨ p100)) == [p1, p1, p100]
@test all(isa.(propositions(f_int ∨ (p1 ∨ p100)), propositionstype(logic(f_int))))

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
# @test promote_type(Formula, SyntaxTree) == Formula
# @test promote_type(SyntaxTree, Formula) == Formula

@test_nowarn ∧((¬(f_int), f_int),)
@test_nowarn ∧((¬(f_int), t2_int),)
@test_nowarn ∧((t2_int, ¬(f_int)),)

@test_throws AssertionError f_int(p1 ∧ p100 ∧ p1_float)
f3_int = f_int(⊥ ∨ (p1 ∧ p100 ∧ p2 ∧ ⊤))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@test_nowarn TruthDict()
@test_nowarn TruthDict([])
@test_nowarn TruthDict((2,3),)
@test_nowarn TruthDict((p1,true),)
@test_nowarn TruthDict([(p1,true),])
@test_nowarn TruthDict(p1 => true)
@test_nowarn TruthDict([p1 => true])
@test_nowarn TruthDict(Dict([p1 => true]))

for i in 1:10
    _tdict = TruthDict(Dict([p => rand([true, false]) for p in propositions(f3_int)]))
    check(f3_int, _tdict) && @test all(collect(values(_tdict.truth)))
    !check(f3_int, _tdict) && @test !all(collect(values(_tdict.truth)))
end

tdict = TruthDict(Dict([p => true for p in propositions(f3_int)]))
@test check(f3_int, tdict)

tdict = TruthDict(Dict([p => false for p in propositions(f3_int)]))
@test !check(f3_int, tdict)

@test check(f3_int, DefaultedTruthDict([], true))
@test check(f3_int, DefaultedTruthDict(true))
@test !check(f3_int, DefaultedTruthDict(false))

@test_nowarn propositionallogic(; operators = SoleLogics.AbstractOperator[])
emptylogic = @test_nowarn propositionallogic(; operators = SoleLogics.AbstractOperator[], alphabet = ExplicitAlphabet([]))
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
