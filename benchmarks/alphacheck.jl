using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: G3
using StatsBase
using Random

alphabet = @atoms p q r
rng = MersenneTwister(1)

aot = vcat(alphabet,getdomain(G3)) # atoms or truths
aotweights = StatsBase.uweights(length(alphabet)+length(getdomain(G3)))
aotpicker = (rng)->StatsBase.sample(rng, aot, aotweights)

@time for i in 1:100000
    alphacheck(
        rand(getdomain(G3)),
        randformula(rng, 128, alphabet, [∧, ∨, →], basecase=aotpicker),
        TruthDict([p=>rand(getdomain(G3)), q=>rand(getdomain(G3)), r=>rand(getdomain(G3))]),
        G3
    )
end

using SoleLogics.ManyValuedLogics: FiniteIndexTruth, BinaryIndexOperation, IndexMonoid, CommutativeIndexMonoid, FiniteIndexLattice, FiniteIndexFLewAlgebra
using StaticArrays
α = FiniteIndexTruth(3)
domain = SVector{3, FiniteIndexTruth}([⊤, ⊥, α])
meettruthtable = SMatrix{3, 3, FiniteIndexTruth}([⊤, ⊥, α, ⊥, ⊥, ⊥, α, ⊥, α])
jointruthtable = SMatrix{3, 3, FiniteIndexTruth}([⊤, ⊤, ⊤, ⊤, ⊥, α, ⊤, α, α])
idxjoin = BinaryIndexOperation{3}(domain, jointruthtable)
idxmeet = BinaryIndexOperation{3}(domain, meettruthtable)
IG3 = FiniteIndexFLewAlgebra{3}(idxjoin, idxmeet, idxmeet, ⊥, ⊤)

rng = MersenneTwister(1)

idxaot = vcat(alphabet,getdomain(IG3)) # atoms or truths
idxaotweights = StatsBase.uweights(length(alphabet)+length(getdomain(IG3)))
idxaotpicker = (rng)->StatsBase.sample(rng, idxaot, idxaotweights)

@time for i in 1:100000
    alphacheck(
        rand(getdomain(IG3)),
        randformula(rng, 128, alphabet, [∧, ∨, →], basecase=idxaotpicker),
        TruthDict([p=>rand(getdomain(IG3)), q=>rand(getdomain(IG3)), r=>rand(getdomain(IG3))]),
        IG3
    )
end
