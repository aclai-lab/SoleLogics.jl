using Graphs
using Random

"""$(randframe_docstring)"""
@__rng_dispatch function randframe(
    rng::Union{Integer,AbstractRNG},
    nworlds::Int64,
    nedges::Int64
)
    worlds = World.(1:nworlds)
    graph = Graphs.SimpleDiGraph(nworlds, nedges, rng=initrng(rng))
    return SoleLogics.ExplicitCrispUniModalFrame(worlds, graph)
end

"""$(randmodel_docstring)"""
@__rng_dispatch function randmodel(
    rng::Union{Integer,AbstractRNG},
    nworlds::Int64,
    nedges::Int64,
    facts::Vector{<:SyntaxLeaf},
    truthvalues::Union{AbstractAlgebra,AbstractVector{<:Truth}}
)
    truthvalues = inittruthvalues(truthvalues)
    fr = randframe(rng, nworlds, nedges)

    valuation = Dict(
        [w => TruthDict([f => rand(truthvalues) for f in facts]) for w in fr.worlds]
    )

    return KripkeStructure(fr, valuation)
end
