# using Revise
using Test
using SoleLogics
using Graphs
using Random

@test (normalize(parsetree("◊((¬(□(q))) → ⊥)")) |> syntaxstring) == "◊□q"

p = Atom("p")
q = Atom("q")
r = Atom("r")

alph_vector = [p,q,r]

φ = randformula(MersenneTwister(1), 4, alph_vector, SoleLogics.BASE_MODAL_OPERATORS)

@test issubset(atoms(φ), alph_vector)

worlds = SoleLogics.World.(1:10)
fr = SoleLogics.ExplicitCrispUniModalFrame(worlds, SimpleDiGraph(length(worlds), 40))
w0 = first(worlds)
accessibles(fr, w0)

K0 = KripkeStructure(fr, Dict([w => TruthDict([p => rand(Bool) for p in alph_vector]) for w in worlds]))

check(φ, K0, w0)
[check(φ, K0, w) for w in worlds]

K1 = KripkeStructure(fr, Dict([w => DefaultedTruthDict([p => true for p in alph_vector[randperm(length(alph_vector))][1:rand(1:length(alph_vector))]], false) for w in worlds]))

check(φ, K1, w0)
[check(φ, K1, w) for w in worlds]

N = 200
for K in [K0, K1]
    for i in 1:N
        φ = randformula(MersenneTwister(1), 4, alph_vector, rand([SoleLogics.BASE_MODAL_OPERATORS, setdiff(SoleLogics.BASE_MODAL_OPERATORS, [⊤, ⊥])]))
        nφ = normalize(φ)
        @show syntaxstring(φ)
        @show syntaxstring(nφ)
        @test [check(φ, K, w; perform_normalization = false) for w in worlds] ==
            # [check(nφ, K, w; perform_normalization = false) for w in worlds] ==
            [check(φ, K, w; perform_normalization = true) for w in worlds]
    end
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
