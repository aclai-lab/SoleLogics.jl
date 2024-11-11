using Test
using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: G3
using StatsBase
using BenchmarkTools
using Random

nsamples = 10000

alphabet = @atoms p q r

############################################################################################

using SoleLogics.ManyValuedLogics: FiniteIndexTruth, BinaryIndexOperation, IndexMonoid, CommutativeIndexMonoid, FiniteIndexLattice, FiniteIndexFLewAlgebra

t, b, a = FiniteIndexTruth.([1:3]...)
jointruthtable = [t, t, t, t, b, a, t, a, a]
meettruthtable = [t, b, a, b, b, b, a, b, a]
idxjoin = BinaryIndexOperation{3}(jointruthtable)
idxmeet = BinaryIndexOperation{3}(meettruthtable)
IG3 = FiniteIndexFLewAlgebra{3}(idxjoin, idxmeet, idxmeet, b, t)
_IG3_domain = getdomain(IG3)[[2,3,1]]
t, a, b = _IG3_domain

_G3_domain = getdomain(G3)
⊤, α, ⊥ = _G3_domain
@btime [G3.meet(α, ⊤), G3.meet(α, α), G3.meet(⊤, α), G3.meet(⊤, ⊤)];
@btime [IG3.meet(a, t), IG3.meet(a, a), IG3.meet(t, a), IG3.meet(t, t)];

@code_warntype G3.meet(α, ⊤)
@code_warntype IG3.meet(a, t)

Test.@inferred G3.meet(α, ⊤)
Test.@inferred IG3.meet(a, t)

############################################################################################

problems_naivedict = begin
    rng = MersenneTwister(1)

    aot = vcat(alphabet,_G3_domain) # atoms or truths
    aotweights = StatsBase.uweights(length(alphabet)+length(_G3_domain))
    aotpicker = (rng)->StatsBase.sample(rng, aot, aotweights)

    problems_naivedict = @test_nowarn [begin
        tv = rand(rng, _G3_domain)
        φ = randformula(rng, 128, alphabet, [∧, ∨, →], basecase=aotpicker)
        I = TruthDict([p=>rand(rng, _G3_domain), q=>rand(rng, _G3_domain), r=>rand(rng, _G3_domain)])
        (tv, φ, I) end for i in 1:nsamples];
    @test_nowarn println(problems_naivedict[1])
    problems_naivedict
end

problems_matrix = begin
    rng = MersenneTwister(1)

    idxaot = vcat(alphabet,_IG3_domain) # atoms or truths
    idxaotweights = StatsBase.uweights(length(alphabet)+length(_IG3_domain))
    idxaotpicker = (rng)->StatsBase.sample(rng, idxaot, idxaotweights)

    problems_matrix = @test_nowarn [begin
        tv = rand(rng, _IG3_domain)
        φ = randformula(rng, 128, alphabet, [∧, ∨, →], basecase=idxaotpicker)
        I = TruthDict([p=>rand(rng, _IG3_domain), q=>rand(rng, _IG3_domain), r=>rand(rng, _IG3_domain)])
        (tv, φ, I) end for i in 1:nsamples];
    @test_nowarn println(problems_matrix[1])
    problems_matrix
end

############################################################################################

checksum_naivedict = Bool[]

@btime for (tv, φ, I) in problems_naivedict
    ret = alphacheck(tv, φ, I, G3)
    push!(checksum_naivedict, ret)
end

checksum_matrix = Bool[]

@btime for (tv, φ, I) in problems_matrix
    ret = alphacheck(tv, φ, I, IG3)
    push!(checksum_matrix, ret)
end



@test all(checksum_naivedict .== checksum_matrix)
