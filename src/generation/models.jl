using Graphs
using Random

"""
    function randframe(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        nworlds::Int64,
        nedges::Int64,
        facts::Vector{SyntaxLeaf}
    end

Return a random Kripke Frame, which is a directed graph interpreted as a
[`SoleLogics.ExplicitCrispUniModalFrame`](@ref). The underlying graph is generated using
[`Graphs.SimpleGraphs.SimpleDiGraph`](@ref).

# Arguments:
* `rng` is a random number generator, or the seed used to create one;
* `nworld` is the number of worlds (nodes) in the frame. Worlds are numbered from `1`
    to `nworld` included.
* `nedges` is the number of relations (edges) in the frame;
* `facts` is a vector of generic [`SyntaxLeaf`](@ref).

See also [`SyntaxLeaf`](@ref), [`Graphs.SimpleGraphs.SimpleDiGraph`](@ref),
[`SoleLogics.ExplicitCrispUniModalFrame`](@ref).
"""
function randframe(
    nworlds::Int64,
    nedges::Int64;
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
)
    randframe(rng, nworlds, nedges)
end

function randframe(
    rng::Union{Integer,AbstractRNG},
    nworlds::Int64,
    nedges::Int64
)
    worlds = World.(1:nworlds)
    graph = Graphs.SimpleDiGraph(nworlds, nedges, rng=initrng(rng))
    return SoleLogics.ExplicitCrispUniModalFrame(worlds, graph)
end

"""
    function randmodel(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        nworlds::Int64,
        nedges::Int64,
        facts::Vector{SyntaxLeaf};
        truthvalues::Union{AbstractAlgebra,AbstractVector{<:Truth}} = BooleanAlgebra();
        rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
    )
"""
function randmodel(
    nworlds::Int64,
    nedges::Int64,
    facts::Vector{<:SyntaxLeaf},
    truthvalues::Union{AbstractAlgebra,AbstractVector{<:Truth}} = BooleanAlgebra();
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
)
    truthvalues = inittruthvalues(truthvalues)
    randmodel(initrng(rng), nworlds, nedges, facts, truthvalues)
end

function randmodel(
    rng::AbstractRNG,
    nworlds::Int64,
    nedges::Int64,
    facts::Vector{<:SyntaxLeaf},
    truthvalues::AbstractVector{<:Truth}
)
    fr = randframe(rng, nworlds, nedges)
    valuation = Dict(
        [w => TruthDict([f => rand(truthvalues) for f in facts]) for w in fr.worlds]
    )

    return KripkeStructure(fr, valuation)
end
