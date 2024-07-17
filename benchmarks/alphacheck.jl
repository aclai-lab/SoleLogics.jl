using SoleLogics
using SoleLogics.ManyValuedLogics
using SoleLogics.ManyValuedLogics: H9
using StatsBase
using Random

alphabet = @atoms p q r
rng = MersenneTwister(1)

aot = vcat(alphabet,getdomain(H9)) # atoms or truths
aotweights = StatsBase.uweights(length(alphabet)+length(getdomain(H9)))
aotpicker = (rng)->StatsBase.sample(rng, aot, aotweights)

@time for i in 1:100000
    alphacheck(
        rand(getdomain(H9)),
        randformula(rng, 10, alphabet, [∧, ∨, →], basecase=aotpicker),
        TruthDict([p=>rand(getdomain(H9)), q=>rand(getdomain(H9)), r=>rand(getdomain(H9))]),
        H9
    )
end

# 4.888668 seconds (20.27 M allocations: 1001.929 MiB, 2.92% gc time, 43.43% compilation time)
