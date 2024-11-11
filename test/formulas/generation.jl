# TODO @Mauro?
# # Pool of valid propositional letters used those
# letters = [SoleLogics.Atom(letter) for letter in string.(collect('a':'z'))]

# @testset "Formulas fundamental checks" begin
#     function fxtest(h::Integer, letters::Vector{<:SoleLogics.AbstractAtom})
#         formula = gen_formula(h, letters)

#         # Height check
#         @test SoleLogics.height(tree(formula)) >= 0
#         @test SoleLogics.height(tree(formula)) <= h

#         # General size check (size between height and (2^(height+1)-1) )
#         @test SoleLogics.size(tree(formula)) > SoleLogics.height(tree(formula))
#         @test SoleLogics.size(tree(formula)) <= 2^(SoleLogics.height(tree(formula)) + 1) - 1

#         # Size check (for each internal node)
#         for node in subformulas(tree(formula), sorted = false)
#             lsize = isdefined(node, :leftchild) ? SoleLogics.size(leftchild(node)) : 0
#             rsize = isdefined(node, :rightchild) ? SoleLogics.size(rightchild(node)) : 0
#             @test SoleLogics.size(node) == lsize + rsize + 1
#         end
#     end

#     function fxtest_modal(height::Integer, max_modepth::Integer)
#         root = gen_formula(height, max_modepth = max_modepth).tree
#         @test SoleLogics.modaldepth(root) <= max_modepth
#     end

#     for i = 1:20
#         fxtest(i, letters)
#     end
# end

# @testset "Modal depth regulation" begin
#     function fxtest_modal(
#         height::Integer,
#         letters::Vector{<:SoleLogics.AbstractAtom},
#         max_modepth::Integer
#     )
#         root = tree(gen_formula(height, letters, max_modepth = max_modepth))
#         @test SoleLogics.modaldepth(root) <= max_modepth
#     end

#     for i = 1:25
#         fxtest_modal(i, letters, i - rand(1:i))
#     end
# end

alp = ExplicitAlphabet(["p", "q", "r"])
ops = [∧,∨]
gr = SoleLogics.CompleteFlatGrammar(alp, ops)

@test length(formulas(gr; maxdepth=3)) == 1631721

using Random

@testset "maxheight parameter" begin
rng = MersenneTwister(1)
found_not_full = false
for i in 1:100
  φ = randformula(rng, 4, SoleLogics.CompleteFlatGrammar(alp, [∨, ∧, ¬, □]), mode = :exactheight)
  @test height(φ) == 4
  φs = (φ |> subformulas)
  ls = children.(φs) .|> x->map(height, x)
  !all(allequal, ls) && (found_not_full = true) && break
end
@test found_not_full

rng = MersenneTwister(1)
found_shorter_than_maxheight = false
for i in 1:100
  maxφ = randformula(4, SoleLogics.CompleteFlatGrammar(alp, [∨, ∧, ¬, □]), mode = :maxheight)
  @test height(maxφ) <= 4
  height(maxφ) < 4 && (found_shorter_than_maxheight = true) && break
end
@test found_shorter_than_maxheight


rng = MersenneTwister(1)
for i in 1:50
  # Full syntax trees
  fullφ = randformula(4, SoleLogics.CompleteFlatGrammar(alp, [∨, ∧, ¬, □]), mode = :full)
  @test height(fullφ) == 4
  φs = (fullφ |> subformulas)
  ls = children.(φs) .|> x->map(height, x)
  @test all(allequal, ls)
end
end
