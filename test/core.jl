# julia
# using Test
# using SoleLogics

@test_nowarn Atom{Int}(1)

p1 = @test_nowarn Atom(1)
p2 = @test_nowarn Atom(2)
p100 = @test_nowarn Atom(100)
p1_float = @test_nowarn Atom{Float64}(1.0)
p1_number_float = @test_nowarn Atom{Number}(1.4)
p1_number = @test_nowarn Atom{Number}(1)
p_string = @test_nowarn Atom{String}("1")

@test Atom(Atom(1)) == Atom(1)
@test_throws ArgumentError Atom(parseformula("¬p"))
@test_throws ArgumentError Atom(¬)

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
alphabet2_int = @test_nowarn ExplicitAlphabet(Atom.(11:20))
@test atomstype(alphabet_int) == @test_nowarn Atom{Int}
@test_nowarn ExplicitAlphabet(Atom{Number}.(1:10))
alphabet_number = @test_nowarn ExplicitAlphabet{Number}(Atom.(1:10))
@test atoms(alphabet_number) isa Vector{Atom{Number}}

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

@test_nowarn UnionAlphabet{Int,ExplicitAlphabet{Int}}([alphabet_int, alphabet2_int])
union_alphabet_int = @test_nowarn UnionAlphabet([alphabet_int, alphabet2_int])
@test atomstype(union_alphabet_int) == @test_nowarn Atom{Int}
@test valuetype(union_alphabet_int) == @test_nowarn Int
@test collect(atoms(union_alphabet_int)) isa Vector{Atom{Int}}
@test Atom(1) in union_alphabet_int
@test !(Atom("My string") in union_alphabet_int)

@test_nowarn UnionAlphabet([alphabet_number, alphabet2_int])

union_alphabet_ofany = @test_nowarn UnionAlphabet([AlphabetOfAny{String}(), AlphabetOfAny{Int}()])
@test Atom("My string") in union_alphabet_ofany
@test valuetype(union_alphabet_ofany) == Union{Int, String}

@test_nowarn UnionAlphabet{Real, AlphabetOfAny}([AlphabetOfAny{Int64}(), AlphabetOfAny{Float64}()])
union_union_alphabet_ofany = @test_nowarn UnionAlphabet{Union{Int,String},AlphabetOfAny{Union{Int,String}}}([AlphabetOfAny{Union{Int,String}}()])
@test valuetype(union_union_alphabet_ofany) == @test_nowarn Union{Int, String}
@test Atom("My string") in union_union_alphabet_ofany
@test Atom(1) in union_union_alphabet_ofany

@test_nowarn convert(SyntaxTree, p1)
@test_nowarn SyntaxTree(p1)
@test_nowarn SyntaxTree(p1, ())
@test_nowarn SyntaxTree(p100, ())
@test_throws MethodError SyntaxBranch(p1)
@test_throws MethodError SyntaxBranch(p1, ())
@test_throws MethodError SyntaxBranch(p100, ())

t1_int = p1
t100_int = p100
@test_throws MethodError SyntaxBranch(3, ())

@test p1 in t1_int

@test_nowarn SyntaxTree(¬, (p1,))
@test_nowarn SyntaxTree(¬, p1)
@test_nowarn SyntaxBranch(¬, (p1,))
@test_nowarn SyntaxBranch(¬, p1)
@test_nowarn SyntaxBranch(¬, t1_int)
t1n_int = @test_nowarn SyntaxBranch(¬, (t1_int,))
@test p1 in t1n_int
@test (¬) in t1n_int
@test_nowarn SyntaxBranch(∧, (t1_int, t1n_int))
t2_int = @test_nowarn SyntaxBranch(∧, (t1_int, t1_int))

grammar_int = SoleLogics.CompleteFlatGrammar(alphabet_int, SoleLogics.BASE_CONNECTIVES)

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

@test propositionallogic(; alphabet = ["p", "q"]) isa SoleLogics.BasePropositionalLogic

@test modallogic() isa SoleLogics.BaseModalLogic
@test (@test_logs (:warn,) modallogic(; operators = [¬, ∨]) isa SoleLogics.BasePropositionalLogic)

@test syntaxstring(SoleLogics._Topo_TPPi()) == "T̅P̅P̅"

@test syntaxstring(diamond(IA_A)) == "⟨A⟩"
@test syntaxstring(diamond(IA_A); use_modal_notation=:superscript) == "◊ᴬ"
@test syntaxstring(diamond(IA_A); use_modal_notation=nothing) == "⟨A⟩"
@test syntaxstring(diamond(IA_A); use_modal_notation=:superscript) != syntaxstring(diamond(IA_A)(⊤); use_modal_notation=nothing)

@test syntaxstring(diamond(SoleLogics._Topo_TPPi()); use_modal_notation=:superscript) == "◊ᵀ̅ᴾ̅ᴾ̅"
@test_broken syntaxstring(diamond(SoleLogics._Topo_TPPi()); use_modal_notation=:subscript) == "◊TODO"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

include("check/propositional.jl")

@test_nowarn SoleLogics.collatetruth(CONJUNCTION, (⊤, ⊤))
@test_nowarn SoleLogics.collatetruth(TOP, ())
@test_nowarn SoleLogics.collatetruth(TOP, ())
@test_nowarn TOP()
@test_nowarn TOP(())
@test_nowarn TOP((),)
