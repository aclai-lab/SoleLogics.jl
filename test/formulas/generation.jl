# TODO adjust
# # Pool of valid propositional letters used those
# letters = [SoleLogics.Letter{Int64}(letter) for letter in string.(collect('a':'z'))]

# @testset "Formulas fundamental checks" begin
#     function fxtest(h::Integer, letters::Vector{<:AbstractPropositionalLetter})
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
#         letters::Vector{<:AbstractPropositionalLetter},
#         max_modepth::Integer
#     )
#         root = tree(gen_formula(height, letters, max_modepth = max_modepth))
#         @test SoleLogics.modaldepth(root) <= max_modepth
#     end

#     for i = 1:25
#         fxtest_modal(i, letters, i - rand(1:i))
#     end
# end

alphabet = ExplicitAlphabet(["p", "q"])
operators = [⊥,∧,∨]
gr = SoleLogics.CompleteFlatGrammar(alphabet, operators)

@test length(formulas(gr; maxdepth=3)) == 1631721
