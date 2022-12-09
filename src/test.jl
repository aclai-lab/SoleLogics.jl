# julia
using Revise
using Test
include("logic.jl")
include("base-semantics.jl")

p1 = @test_nowarn Proposition(1)
p100 = @test_nowarn Proposition(100)
@test_nowarn Proposition{Int}(1)
p1_float = @test_nowarn Proposition{Float64}(1.0)
p1_number_float = @test_nowarn Proposition{Number}(1.4)
p1_number = @test_nowarn Proposition{Number}(1)
p_string = @test_nowarn Proposition{String}("1")

@test arity(p1) == 0
@test propositiontype(AbstractAlphabet{Int}) == Proposition{Int}

@test_nowarn ExplicitAlphabet(Proposition.([1,2]))
@test_nowarn ExplicitAlphabet([1,2])
@test Proposition(1) in ExplicitAlphabet([1,2])
@test Proposition(2) in ExplicitAlphabet([1,2])
@test !(Proposition(3) in ExplicitAlphabet([1,2]))

@test_nowarn ExplicitAlphabet(1:10)
alphabet_int = @test_nowarn ExplicitAlphabet(Proposition.(1:10))
@test propositiontype(alphabet_int) == @test_nowarn Proposition{Int}
@test_nowarn ExplicitAlphabet(Proposition{Number}.(1:10))
alphabet_number = @test_nowarn ExplicitAlphabet{Number}(Proposition.(1:10))
@test propositions(alphabet_number) isa Vector{Proposition{Number}}

@test alphabet_int(1) isa Proposition{Int}
@test alphabet_number(1) isa Proposition{Int}
@test alphabet_number(Float64(1.0)) isa Proposition{Float64}

p_vec_number = @test_nowarn Proposition{Vector{<:Number}}([1])
p_vec_int = @test_nowarn Proposition{Vector{Int}}([1])
@test_throws MethodError Proposition{<:Vector{Int}}([1.0])
p_vec = @test_nowarn Proposition{Vector}([1.0])

@test "My string" in AlphabetOfAny{String}()
@test 1 in AlphabetOfAny{Number}()
@test 1.0 in AlphabetOfAny{Number}()
@test !(1 in AlphabetOfAny{String}())

@test_nowarn convert(SyntaxTree, p1)
@test_nowarn SyntaxTree(p1)
@test_nowarn SyntaxTree{typeof(p1), typeof(p1)}(p1)
@test_nowarn SyntaxTree(p1)
t1_int = @test_nowarn SyntaxTree(p1, ())
t100_int = @test_nowarn SyntaxTree(p100, ())
@test tokentypes(t1_int) == tokentype(t1_int)
@test_throws MethodError SyntaxTree(3, ())

@test p1 in t1_int

@test_nowarn SyntaxTree(¬, (p1,))
@test_nowarn SyntaxTree(¬, p1)
@test_nowarn SyntaxTree(¬, t1_int)
t1n_int = @test_nowarn SyntaxTree(¬, (t1_int,))
@test p1 in t1n_int
@test (¬) in t1n_int
@test tokentypes(t1n_int) == Union{typeof(¬), tokentype(t1_int)}
@test_nowarn SyntaxTree(∧, (t1_int, t1n_int))
t2_int = @test_nowarn SyntaxTree(∧, (t1_int, t1_int))
@test tokentypes(SyntaxTree(∧, (t2_int, t1n_int))) == Union{typeof(∧), tokentypes(t1n_int)}

grammar_int = CompleteGrammar(alphabet_int, base_operators)

@test Proposition(1) in grammar_int
@test ! (Proposition(11) in grammar_int)
@test ! (Proposition(1.0) in grammar_int)
@test t1_int in grammar_int
@test ! (t100_int in grammar_int)
@test_throws ErrorException t1_int in alphabet(grammar_int)

logic_int = BaseLogic(grammar_int, BooleanAlgebra())

@test_throws MethodError "aoeu" in base_logic
@test Proposition("aoeu") in base_logic
@test ! (Proposition(1) in base_logic)

@test_nowarn Formula(Base.RefValue(logic_int), t1_int)
f_int = @test_nowarn Formula(logic_int, t1_int)

@test_throws MethodError 1 in f_int
@test p1 in f_int
@test p1 in grammar(f_int)
@test ! (p1_number in f_int)
@test ! ( p100 in f_int )
@test ! ( Proposition("1") in f_int )


t2_int = @test_nowarn ¬(t1_int)
@test_nowarn ¬(p1)
@test propositiontypes(p1 ∨ p1_number) != Proposition{Int}
@test propositiontypes(p1 ∨ p1_number_float) == Union{Proposition{Int}, Proposition{Number}}
@test propositiontypes(p1 ∨ p1_float) == Union{Proposition{Int}, Proposition{Float64}}
@test_nowarn p1 ∨ p100
@test_nowarn ¬(p1) ∨ p1
@test_nowarn ¬(p1) ∨ ¬(p1)

@test_nowarn p1 ∨ t2_int
@test_nowarn t2_int ∨ p1
@test_nowarn t2_int ∨ t2_int
@test_nowarn ¬(t2_int) ∧ t2_int
@test_nowarn ¬(¬(t2_int) ∧ t2_int)
@test_nowarn ∧(¬(t2_int), t2_int)
@test_nowarn ∧((¬(t2_int), t2_int),)
@test_nowarn ¬(¬(p1))

@test_nowarn ¬(f_int)
@test_nowarn f_int ∨ f_int
@test_nowarn ¬(f_int) ∨ f_int
@test_nowarn p1 ∨ f_int
@test_nowarn f_int ∨ p1
@test_nowarn t2_int ∨ f_int
@test_nowarn f_int ∨ t2_int

@test_nowarn p1 ∨ t2_int
@test typeof(¬(f_int)) == typeof(f_int)
@test_nowarn ∧((¬(f_int), f_int),)

# @test promote_type(typeof(f_int), typeof(t2_int)) == typeof(f_int)
# @test promote_type(Formula, SyntaxTree) == Formula
# @test promote_type(SyntaxTree, Formula) == Formula

@test_nowarn ∧((¬(f_int), f_int),)
@test_nowarn ∧((¬(f_int), t2_int),)
@test_nowarn ∧((t2_int, ¬(f_int)),)
