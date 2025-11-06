p1 = @test_nowarn Atom(1)
p2 = @test_nowarn Atom(2)
p1_float = @test_nowarn Atom{Float64}(1.0)
p1_number = @test_nowarn Atom{Number}(1)

@testset "Syntax tree construction" begin

  t1 = @test_nowarn ¬p1 ∨ (¬p2 ∧ (¬p1 ∨ ¬p2))
  t2 = @test_nowarn ¬p1

  @test_nowarn ⊥()
  @test_nowarn ¬(p1)
  @test_nowarn ∨(p1, p1)
  @test_nowarn p1 ∨ p1_number
  @test_nowarn ∨(p1, p1, p1_number)
  @test_nowarn ¬(∨(p1, p1, p1_number))
  @test_nowarn p1 ∨ p2
  @test_nowarn ¬(p1) ∨ p1
  @test_nowarn SyntaxTree(⊤)
  @test_nowarn ⊤ ∨ ⊤
  @test_nowarn p1 ∨ ⊤
  @test_nowarn ⊥ ∨ p1 ∨ ⊤
  @test_nowarn p1 ∨ p2

  @test_nowarn p1 ∨ t2
  @test_nowarn t2 ∨ p1
  @test_nowarn t2 ∨ t2
  @test_nowarn ⊥ ∨ t2 ∨ ⊤
  @test_nowarn t2 ∨ ⊤
  @test_nowarn ¬(t2) ∧ t2
  @test_nowarn ¬(¬(t2) ∧ t2)
  @test_nowarn ∧(¬(t2), t2)
  @test_nowarn ∧((¬(t2), t2),)
  @test_nowarn ∧(¬(t2), t2, ¬(t2) ∧ t2)
  @test_nowarn ¬(¬(p1))


  @test_nowarn CONJUNCTION(t2, t2)
  @test_nowarn CONJUNCTION(t2, t2, p1)
  @test_nowarn CONJUNCTION(t2, p1, p1)
  @test_nowarn CONJUNCTION(p1, p1)
  @test_nowarn CONJUNCTION(p1, p1, p1)
end

@testset "Syntax tree construction" begin
  t1 = @test_nowarn ¬p1 ∨ (¬p2 ∧ (¬p1 ∨ ¬p2))
  t2 = @test_nowarn ¬p1

  @test tokens(t1) == SyntaxToken[p1, ¬, p2, ¬, p1, ¬, p2, ¬, ∨, ∧, ∨]
  @test atoms(t1) == Atom[p1, p2, p1, p2]
  @test truths(t1) == Truth[]
  @test leaves(t1) == SyntaxLeaf[p1, p2, p1, p2]
  @test connectives(t1) == Connective[¬, ¬, ¬, ¬, ∨, ∧, ∨]
  @test operators(t1) == Operator[¬, ¬, ¬, ¬, ∨, ∧, ∨]

  @test appendtokens!([], t1) == [p1, ¬, p2, ¬, p1, ¬, p2, ¬, ∨, ∧, ∨]
  @test appendatoms!([], t1) == [p1, p2, p1, p2]
  @test appendtruths!([], t1) == []
  @test appendleaves!([], t1) == [p1, p2, p1, p2]
  @test appendconnectives!([], t1) == [¬, ¬, ¬, ¬, ∨, ∧, ∨]
  @test appendoperators!([], t1) == [¬, ¬, ¬, ¬, ∨, ∧, ∨]

  @test ntokens(t1) == 11
  @test natoms(t1) == 4
  @test ntruths(t1) == 0
  @test nleaves(t1) == 4
  @test nconnectives(t1) == 7
  @test noperators(t1) == 7

  @test !(all(isa(atoms(p1 ∨ p1_number), Atom{Int})))
  @test all(isa.(atoms(p1 ∨ p1_number), Union{Atom{Int}, Atom{Number}}))
  @test all(isa.(atoms(p1 ∨ p1_float), Union{Atom{Int}, Atom{Float64}}))
  @test atoms(p1 ∨ p2) == [p1, p2]

end

@testset "SyntaxLeaf interface" begin
  p1 = Atom(1)
  
  @test syntaxstring(p1) == "1"
  @test !hasdual(p1)

  @test syntaxstring(⊥) == "⊥"
  @test syntaxstring(⊤) == ⊤
  @test dual(⊥) == ⊤
  @test !hasdual(⊥)
  
  @test !istop(⊥)
  @test isbot(⊥)
  @test ⊥ < ⊤
  @test ⊥ <= ⊤
  @test min(⊥, ⊤)
  @test max(⊥, ⊤)
  @test precedes(⊥, ⊤)
  @test truthjoin(⊥, ⊤)

  @test !istop(⊤ ∧ ⊤)

end
