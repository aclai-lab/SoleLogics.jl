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
@test propositiontype(AbstractAlphabet{Int}) isa Proposition{Int}

@test_nowarn ExplicitAlphabet([1,2])
@test_nowarn ExplicitAlphabet(1:10)
alphabet_integer = @test_nowarn ExplicitAlphabet(Proposition.(1:10))
propositiontype(alphabet_integer) == @test_nowarn Proposition{Int}
alphabet_number = @test_nowarn ExplicitAlphabet(Proposition{Number}.(1:10))
alphabet_number = @test_nowarn ExplicitAlphabet{Number}(Proposition.(1:10))
@test propositions(alphabet_number) isa Vector{Proposition{Number}}

p_vec_number = @test_nowarn Proposition{Vector{<:Number}}([1])
p_vec_integer = @test_nowarn Proposition{Vector{Int}}([1])
@test_throws MethodError Proposition{<:Vector{Int}}([1.0])
p_vec = @test_nowarn Proposition{Vector}([1.0])

@test "My string" in AlphabetOfAll{String}()
@test !(1 in AlphabetOfAll{String}())

# TODO from here

@test ¬(SyntaxTree(¬, (SyntaxTree(Proposition(3)),)))
@test ¬((¬, (SyntaxTree(Proposition{Int}(3)),)))
@test ¬(¬(Proposition(3)))

