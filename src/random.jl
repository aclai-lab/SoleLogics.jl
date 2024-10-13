using Graphs
using Random
using StatsBase

import Random: rand
import StatsBase: sample

#= ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Formulas ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ =#

doc_rand = """
    Base.rand(
        [rng::AbstractRNG = Random.GLOBAL_RNG,]
        alphabet::AbstractAlphabet,
        args...;
        kwargs...
    )::Atom

    Base.rand(
        [rng::AbstractRNG = Random.GLOBAL_RNG],
        height::Integer,
        l::AbstractLogic,
        args...;
        kwargs...
    )::Formula

    Base.rand(
        [rng::AbstractRNG = Random.GLOBAL_RNG,]
        height::Integer,
        g::CompleteFlatGrammar,
        args...
    )::Formula

    Base.rand(
        height::Integer,
        connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
        atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
        truthvalues::Union{Nothing,AbstractAlgebra,AbstractVector{<:Truth}} = nothing,
        args...;
        rng::AbstractRNG = Random.GLOBAL_RNG,
        kwargs...
    )::Formula

Randomly generate an [`Atom`](@ref) from an [`AbstractAlphabet`](@ref) according to a
uniform distribution. If a [`CompleteFlatGrammar`](@ref) is provided together with an
`height` a [`Formula`](@ref) could also be generated.

# Implementation
If the `alphabet` is finite, the function defaults to `rand(rng, atoms(alphabet))`;
otherwise, it must be implemented, and additional keyword arguments should be provided
in order to limit the (otherwise infinite) sampling domain.

See also
[`AbstractAlphabet`](@ref), [`Atom`](@ref), [`CompleteFlatGrammar`](@ref),
[`Formula`](@ref), [`randformula`](@ref).
"""

"""$(doc_rand)"""
function Base.rand(a::AbstractAlphabet, args...; kwargs...)
    Base.rand(Random.GLOBAL_RNG, a, args...; kwargs...)
end

function Base.rand(
    rng::AbstractRNG,
    a::AbstractAlphabet,
    args...;
    kwargs...
)
    randatom(rng, a, args...; kwargs...)
end

function Base.rand(height::Integer, l::AbstractLogic, args...; kwargs...)
    Base.rand(Random.GLOBAL_RNG, height, l, args...; kwargs...)
end

function Base.rand(
    rng::AbstractRNG,
    height::Integer,
    l::AbstractLogic,
    args...;
    kwargs...
)
    Base.rand(rng, grammar(l), args...; kwargs...)
end

# For the case of a CompleteFlatGrammar, the alphabet and the operators suffice.
function Base.rand(
    height::Integer,
    g::CompleteFlatGrammar,
    args...
)
    Base.rand(Random.GLOBAL_RNG, height, g, args...)
end

function Base.rand(
    rng::AbstractRNG,
    height::Integer,
    g::CompleteFlatGrammar,
    args...;
    kwargs...
)
    randformula(rng, height, alphabet(g), operators(g), args...; kwargs...)
end

function Base.rand(
    height::Integer,
    atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
    connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
    truthvalues::Union{Nothing,AbstractAlgebra,AbstractVector{<:Truth}} = nothing,
    args...;
    kwargs...
)
    Base.rand(Random.GLOBAL_RNG, height, atoms, connectives, truthvalues, args...;
        kwargs...)
end

function Base.rand(
    rng::AbstractRNG,
    height::Integer,
    atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
    connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
    truthvalues::Union{Nothing,AbstractAlgebra,AbstractVector{<:Truth}} = nothing,
    args...;
    kwargs...
)
    # If Truth's are specified as `operators`, then they cannot be simultaneously
    #  provided as `truthvalues`
    @assert (connectives isa AbstractVector{<:Connective} ||
            !(truthvalues isa AbstractVector{<:Truth})
        ) "Unexpected connectives and truth values: $(connectives) and $(truthvalues)."

    atoms = atoms isa AbstractAlphabet ? SoleLogics.atoms(atoms) : atoms
    ops = connectives
    if !isnothing(truthvalues)
        truthvalues = inittruthvalues(truthvalues)
        @assert typejoin(typeof.(truthvalues)...) != Truth "Truth values " *
            "$(truthvalues) must belong to the same algebra " *
            "(and have a common supertype that is not Truth)."
        ops = vcat(ops, truthvalues)
    end

    randformula(height, ops, atoms, args...; rng=rng, kwargs...)
end

doc_sample = """
    function StatsBase.sample(
        [rng::AbstractRNG = Random.GLOBAL_RNG,]
        alphabet::AbstractAlphabet,
        weights::AbstractWeights,
        args...;
        kwargs...
    )

    function StatsBase.sample(
        rng::AbstractRNG,
        l::AbstractLogic,
        weights::AbstractWeights,
        args...;
        kwargs...
    )

    StatsBase.sample(
        [rng::AbstractRNG = Random.GLOBAL_RNG,]
        height::Integer,
        g::AbstractGrammar,
        [opweights::Union{Nothing,AbstractWeights} = nothing,]
        args...;
        kwargs...
    )::Formula

Randomly sample an [`Atom`](@ref) from an `alphabet`, or a logic formula of given `height`
from a grammar `g`.
Sampling is weighted, thus, for example, if the first weight in `weights` is higher than
the others, then the first atom in the alphabet is selected more frequently.

See also [`AbstractAlphabet`](@ref), [`AbstractWeights`](@ref), [`Atom`](@ref).
"""
function StatsBase.sample(
    alphabet::AbstractAlphabet,
    weights::AbstractWeights,
    args...;
    kwargs...
)::Atom
    StatsBase.sample(Random.GLOBAL_RNG, alphabet, weights, args...; kwargs...)
end

function StatsBase.sample(
    rng::AbstractRNG,
    alphabet::AbstractAlphabet,
    weights::AbstractWeights,
    args...;
    kwargs...
)::Atom
    if isfinite(alphabet)
        StatsBase.sample(rng, atoms(alphabet), weights, args...; kwargs...)
    else
        error("Please, provide method StatsBase.sample(rng::AbstractRNG, " *
            "alphabet::$(typeof(alphabet)), args...; kwargs...).")
    end
end

function StatsBase.sample(
    l::AbstractLogic,
    atomweights::AbstractWeights,
    opweights::AbstractWeights,
    args...;
    kwargs...
)
    StatsBase.sample(Random.GLOBAL_RNG, l, atomweights, opweights, args...; kwargs...)
end

function StatsBase.sample(
    rng::AbstractRNG,
    l::AbstractLogic,
    atomweights::AbstractWeights,
    opweights::AbstractWeights,
    args...;
    kwargs...
)
    StatsBase.sample(init(rng), grammar(l), atomweights, opweights, args...; kwargs...)
end

"""$(doc_sample)"""
function StatsBase.sample(
    height::Integer,
    g::AbstractGrammar,
    atomweights::Union{Nothing,AbstractWeights} = nothing,
    opweights::Union{Nothing,AbstractWeights} = nothing,
    args...;
    kwargs...
)
    StatsBase.sample(Random.GLOBAL_RNG, height, g, atomweights, opweights, args...;
        kwargs...)
end

"""$(doc_sample)"""
function StatsBase.sample(
    rng::AbstractRNG,
    height::Integer,
    g::AbstractGrammar,
    atomweights::Union{Nothing,AbstractWeights} = nothing,
    opweights::Union{Nothing,AbstractWeights} = nothing,
    args...;
    kwargs...
)
    randformula(
        rng, height, alphabet(g), operators(g), args...;
        # atompicker=(rng,dom)->StatsBase.sample(rng, dom, atomweights), kwargs...)
        atompicker = atomweights, opweights = opweights, kwargs...)
end

#= ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CompleteFlatGrammar ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ =#

# TODO
# - make rng first (optional) argument of randformula (see above)
# - in randformula, keyword argument alphabet_sample_kwargs that are unpacked upon sampling atoms, as in: Base.rand(rng, a; alphabet_sample_kwargs...). This would allow to sample from infinite alphabets, so when this parameter, !isfinite(alphabet) is allowed!

# TODO @Mauro implement this method.
doc_randformula = """
    randformula(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG, ]
        height::Integer,
        alphabet,
        operators::AbstractVector;
        kwargs...
    )::SyntaxTree

    randformula(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG, ]
        height::Integer,
        g::AbstractGrammar;
        kwargs...
    )::SyntaxTree

Return a pseudo-randomic `SyntaxTree`.

# Arguments
- `rng::Union{Intger,AbstractRNG} = Random.GLOBAL_RNG`: random number generator;
- `height::Integer`: height of the generated structure;
- `alphabet::AbstractAlphabet`: collection from which atoms are chosen randomly;
- `operators::AbstractVector`: vector from which legal operators are chosen;
- `g::AbstractGrammar`: alternative to passing alphabet and operators separately. (TODO explain?)

# Keyword Arguments
- `modaldepth::Integer`: maximum modal depth
- `atompicker::Function`: method used to pick a random element. For example, this could be
    Base.rand or StatsBase.sample.
- `opweights::AbstractWeights`: weight vector over the set of operators (see `StatsBase`).

# Examples

```julia-repl
julia> syntaxstring(randformula(4, ExplicitAlphabet([1,2]), [NEGATION, CONJUNCTION, IMPLICATION]))
"¬((¬(¬(2))) → ((1 → 2) → (1 → 2)))"
```

See also [`AbstractAlphabet`](@ref), [`SyntaxBranch`](@ref).
"""

"""$(doc_randformula)"""
function randformula(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    alphabet::Union{AbstractVector,AbstractAlphabet},
    operators::AbstractVector;
    modaldepth::Integer = height,
    atompicker::Union{Function,AbstractWeights,AbstractVector{<:Real},Nothing} = randatom,
    opweights::Union{AbstractWeights,AbstractVector{<:Real},Nothing} = nothing
)::SyntaxTree

    rng = initrng(rng)
    alphabet = convert(AbstractAlphabet, alphabet)
    @assert all(x->x isa Operator, operators) "Unexpected object(s) passed as" *
        " operator:" * " $(filter(x->!(x isa Operator), operators))"

    if (isnothing(opweights))
        opweights = StatsBase.uweights(length(operators))
    elseif (opweights isa AbstractVector)
        @assert length(opweights) == length(operators) "Mismatching numbers of operators " *
                "($(length(operators))) and opweights ($(length(opweights)))."
        opweights = StatsBase.weights(opweights)
    end

    if (isnothing(atompicker))
        atompicker = StatsBase.uweights(natoms(alphabet))
    elseif (atompicker isa AbstractVector)
        @assert length(atompicker) == natoms(alphabet) "Mismatching numbers of atoms " *
                "($(natoms(alphabet))) and atompicker ($(length(atompicker)))."
        atompicker = StatsBase.weights(atompicker)
    end

    if !(atompicker isa Function)
        atomweights = atompicker
        atompicker = (rng, dom)->StatsBase.sample(rng, dom, atomweights)
    end

    nonmodal_operators = findall(!ismodal, operators)

    # recursive call
    function _randformula(
        rng::AbstractRNG,
        height::Integer,
        modaldepth::Integer;
    )::SyntaxTree

        if height == 0
            return atompicker(rng, alphabet)
        else
            # Sample operator and generate children (modal connectives only if modaldepth > 0)
            ops, ops_w = begin
                if modaldepth > 0
                    operators, opweights
                else
                    operators[nonmodal_operators], opweights[nonmodal_operators]
                end
            end

            # op = rand(rng, ops)
            op = sample(rng, ops, ops_w)
            ch = Tuple([
                    _randformula(rng, height-1, modaldepth-(ismodal(op) ? 1 : 0))
                    for _ in 1:arity(op)])
            return SyntaxTree(op, ch)
        end
    end

    # If the alphabet is not iterable, this function should not work.
    if !isfinite(alphabet)
        @warn "Attempting to generate random formulas from " *
            "(infinite) alphabet of type $(typeof(alphabet))!"
    end

    return _randformula(rng, height, modaldepth)
end

function randformula(
    rng::AbstractRNG,
    height::Integer,
    g::AbstractGrammar,
    args...;
    kwargs...
)
    randformula(rng, height, alphabet(g), operators(g), args...; kwargs...)
end

# Helper
function randformula(
    height::Integer,
    args...;
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
    kwargs...
)
    randformula(initrng(rng), height, args...; kwargs...)
end

#= ~~~~~~~~~~~~~~~~~~~~~~~~~~ Kripke Models generation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ =#
"""
    function randframe(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        nworlds::Int,
        nedges::Int,
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
    nworlds::Int,
    nedges::Int;
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
)
    randframe(rng, nworlds, nedges)
end

function randframe(
    rng::Union{Integer,AbstractRNG},
    nworlds::Int,
    nedges::Int
)
    worlds = World.(1:nworlds)
    graph = Graphs.SimpleDiGraph(nworlds, nedges, rng=initrng(rng))
    return SoleLogics.ExplicitCrispUniModalFrame(worlds, graph)
end

"""
    function randmodel(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        nworlds::Int,
        nedges::Int,
        facts::Vector{SyntaxLeaf};
        truthvalues::Union{AbstractAlgebra,AbstractVector{<:Truth}} = BooleanAlgebra();
        rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
    )
"""
function randmodel(
    nworlds::Int,
    nedges::Int,
    facts::Vector{<:SyntaxLeaf},
    truthvalues::Union{AbstractAlgebra,AbstractVector{<:Truth}} = BooleanAlgebra();
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
)
    truthvalues = inittruthvalues(truthvalues)
    randmodel(initrng(rng), nworlds, nedges, facts, truthvalues)
end

function randmodel(
    rng::AbstractRNG,
    nworlds::Int,
    nedges::Int,
    facts::Vector{<:SyntaxLeaf},
    truthvalues::AbstractVector{<:Truth}
)
    fr = randframe(rng, nworlds, nedges)
    valuation = Dict(
        [w => TruthDict([f => rand(truthvalues) for f in facts]) for w in fr.worlds]
    )

    return KripkeStructure(fr, valuation)
end
