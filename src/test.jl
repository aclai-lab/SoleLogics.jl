# julia
using Revise
using Test
include("logic.jl")
include("base-semantics.jl")

p = @test_nowarn Proposition(1)
p_integer = @test_nowarn Proposition{Int}(1)
p_number = @test_nowarn Proposition{Number}(1)
p_string = @test_nowarn Proposition{String}("1")

@test arity(p) == 0
@test propositiontype(AbstractAlphabet{Int}) == Proposition{Int}

@test_nowarn ExplicitAlphabet(Proposition.([1,2]))
@test_nowarn ExplicitAlphabet([1,2])
@test Proposition(1) in ExplicitAlphabet([1,2])
@test Proposition(2) in ExplicitAlphabet([1,2])
@test !(Proposition(3) in ExplicitAlphabet([1,2]))

@test_nowarn ExplicitAlphabet(1:10)
alphabet_integer = @test_nowarn ExplicitAlphabet(Proposition.(1:10))
propositiontype(alphabet_integer) == @test_nowarn Proposition{Int}
alphabet_number = @test_nowarn ExplicitAlphabet(Proposition{Number}.(1:10))
alphabet_number = @test_nowarn ExplicitAlphabet{Number}(Proposition.(1:10))
@test propositions(alphabet_number) isa Vector{Proposition{Number}}

@test alphabet_integer(1) isa Proposition{Int}
@test alphabet_number(1) isa Proposition{Int}
@test alphabet_number(Float64(1.0)) isa Proposition{Float64}

p_vec_number = @test_nowarn Proposition{Vector{<:Number}}([1])
p_vec_integer = @test_nowarn Proposition{Vector{Int}}([1])
@test_throws MethodError Proposition{<:Vector{Int}}([1.0])
p_vec = @test_nowarn Proposition{Vector}([1.0])

@test "My string" in AlphabetOfAny{String}()
@test 1 in AlphabetOfAny{Number}()
@test 1.0 in AlphabetOfAny{Number}()
@test !(1 in AlphabetOfAny{String}())

# TODO from here

@test_nowarn SyntaxTree(Proposition(3))
t1_integer = @test_nowarn SyntaxTree(Proposition(3), ())
@test_throws MethodError SyntaxTree(3, ())

grammar_integer = CompleteGrammar(alphabet_integer, base_operators)

@test Proposition(1) in grammar_integer
@test ! (Proposition(11) in grammar_integer)
@test ! (Proposition(1.0) in grammar_integer)
@test t1_integer in grammar_integer
@test_throws ErrorException t1_integer in alphabet(grammar_integer)

logic_integer = BaseLogic(grammar_integer, BooleanAlgebra())

@test_throws MethodError "aoeu" in base_logic
@test Proposition("aoeu") in base_logic
@test ! (Proposition(1) in base_logic)

@test_nowarn Formula(Base.RefValue(logic_integer), t1_integer)
f_integer = @test_nowarn Formula(logic_integer, t1_integer)

@test_throws MethodError 1 in f_integer
@test Proposition(3) in f_integer
@test ! ( Proposition(1) in f_integer )
@test Proposition(1) in grammar(f_integer)
@test ! ( Proposition("1") in f_integer )


@test_nowarn ¬(t1_integer)
@test_nowarn SyntaxTree(¬, t1_integer)
t2 = @test_nowarn SyntaxTree(¬, (t1_integer, ))
@test_nowarn ¬(t2) ∧ t2
@test_nowarn ¬(¬(t2) ∧ t2)
@test_nowarn ¬(¬(Proposition(3)))
