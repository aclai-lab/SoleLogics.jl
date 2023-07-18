# using Revise
using Test
using SoleLogics
using Graphs
using Random

@test (normalize(parsetree("◊((¬(□(q))) → ⊥)")) |> syntaxstring) == "◊□q"

p = Proposition("p")
q = Proposition("q")
r = Proposition("r")

φ = randformula(MersenneTwister(1), 4, [p,q,r], SoleLogics.BASE_MODAL_OPERATORS)

worlds = SoleLogics.World.(1:10)
fr = SoleLogics.ExplicitCrispUniModalFrame(worlds, SimpleDiGraph(length(worlds), 40))
w0 = first(worlds)
accessibles(fr, w0)

K = KripkeStructure(fr, Dict([w => TruthDict([p => rand(Bool) for p in propositions(φ)]) for w in worlds]))

check(φ, K, w0)

[check(φ, K, w) for w in worlds]

for i in 1:200
    φ = randformula(MersenneTwister(1), 4, [p,q,r], rand([SoleLogics.BASE_MODAL_OPERATORS, setdiff(SoleLogics.BASE_MODAL_OPERATORS, [⊤, ⊥])]))
    nφ = normalize(φ)
    # @show syntaxstring(φ)
    # @show syntaxstring(nφ)
    @test [check(φ, K, w; perform_normalization = false) for w in worlds] ==
        # [check(nφ, K, w; perform_normalization = false) for w in worlds] ==
        [check(φ, K, w; perform_normalization = true) for w in worlds]
end

# φ = parsetree("(¬((q → p) → (q ∨ q))) → ⊤")
# [check(φ, K, w; perform_normalization = false) for w in worlds]
# φ = parsetree("¬(¬(¬(¬q ∨ p) ∨ (q ∨ q))) ∨ ⊤")
# [check(φ, K, w; perform_normalization = false) for w in worlds]
# φ = parsetree("¬(¬(¬(¬q ∨ p) ∨ (q ∨ q)))")
# [check(φ, K, w; perform_normalization = false) for w in worlds]
# nφ = parsetree("(¬(q)) ∧ ((¬(q)) ∨ p)")
# [check(nφ, K, w; perform_normalization = false) for w in worlds]
#

# φ = parsetree("¬p → ⊤")

# syntaxstring(φ) = "◊(((q → r) → (r ∨ r)) ∧ ((¬(p)) → ⊤))"
# syntaxstring(nφ) = "◊(((q ∧ (¬(r))) ∨ r) ∧ (¬(p)))"
