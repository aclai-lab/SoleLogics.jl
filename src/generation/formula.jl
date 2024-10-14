using Random
using StatsBase

import Random: rand
import StatsBase: sample

include("docstrings.jl")
include("utils.jl")

"""$(randatom_docstring)"""
@__rng_dispatch function randatom(
    rng::Union{Random.AbstractRNG,Integer},
    a::AbstractAlphabet,
    args...;
    kwargs...
)
    if isfinite(a)
        # commented because otherwise this is getting spammed
        # @warn "Consider implementing a specific `randatom` dispatch for your alphabet " *
        #     "type ($(typeof(a))) to increase performances."

        return Base.rand(initrng(rng), atoms(a), args...; kwargs...)
    else
        error("Please provide method randatom(rng::$(typeof(rng)), " *
            "alphabet::$(typeof(a)), args...; kwargs...)")
    end
end

"""$(randatom_unionalphabet_docstring)"""
@__rng_dispatch function randatom(
        rng::Union{Integer,AbstractRNG},
        a::UnionAlphabet,
        args...;
        atompicking_mode::Symbol=:uniform,
        subalphabets_weights::Union{
            Nothing,AbstractWeights,AbstractVector{<:Real}}=nothing,
        kwargs...
)
    _atompicking_modes = [:uniform, :uniform_subalphabets, :weighted]
    if !(atompicking_mode in _atompicking_modes)
        throw(ArgumentError("Invalid value for `atompicking_mode` ($(atompicking_mode))." *
                "Chosee between $(atompicking_modes)."))
    end

    rng = initrng(rng)
    alphs = subalphabets(a)

    if atompicking_mode == :weighted
        if isnothing(subalphabets_weights)
            throw(ArgumentError("`:weighted` picking_mode requires weights in " *
                "`subalphabets_weights` "))
        end

        if length(subalphabets_weights) != length(alphs)
            throw(ArgumentError("Mismatching numbers of alphabets " *
                "($(length(alphs))) and weights ($(length(subalphabets_weights)))."))
        end

            subalphabets_weights = StatsBase.weights(subalphabets_weights)
        pickedalphabet = StatsBase.sample(rng, alphs, subalphabets_weights)
    else
        subalphabets_weights = begin
            # this atomatically excludes subalphabets with empty threshold vector
            if atompicking_mode == :uniform_subalphabets
                # set the weight of the empty alphabets to zero
                weights = Weights(ones(Int, length(alphs)))
                weights[natoms.(alphs) .== 0] .= 0
            elseif atompicking_mode == :uniform
                weights = Weights(natoms.(alphs))
            end
            weights
        end
        pickedalphabet = sample(rng, alphs, subalphabets_weights)
    end

    return randatom(rng, pickedalphabet)
end



"""$(rand_abstractalphabet_docstring)"""
@__rng_dispatch function Base.rand(
    rng::Union{Integer,AbstractRNG},
    a::AbstractAlphabet,
    args...;
    kwargs...
)
    randatom(initrng(rng), a, args...; kwargs...)
end


"""$(rand_abstractlogic_docstring)"""
@__rng_dispatch function Base.rand(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    l::AbstractLogic,
    args...;
    kwargs...
)
    Base.rand(initrng(rng), height, grammar(l), args...; kwargs...)
end

"""$(rand_completeflatgrammar_docstring)"""
@__rng_dispatch function Base.rand(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    g::CompleteFlatGrammar,
    args...;
    kwargs...
)
    randformula(initrng(rng), height, alphabet(g), operators(g), args...; kwargs...)
end

"""$(rand_granular_docstring)"""
@__rng_dispatch function Base.rand(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
    connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
    args...;
    truthvalues::Union{Nothing,AbstractAlgebra,AbstractVector{<:Truth}}=nothing,
    kwargs...
)
    # If Truth's are specified as `operators`, then they cannot be simultaneously
    #  provided as `truthvalues`
    if !(connectives isa AbstractVector{<:Connective} ||
            !(truthvalues isa AbstractVector{<:Truth})
        )
        thorw(ArgumentError("Unexpected connectives and truth values: " *
            "$(connectives) and $(truthvalues)."))
    end

    atoms = atoms isa AbstractAlphabet ? SoleLogics.atoms(atoms) : atoms
    ops = connectives
    if !isnothing(truthvalues)
        truthvalues = inittruthvalues(truthvalues)
        if typejoin(typeof.(truthvalues)...) == Truth
            throw(ArgumentError("Truth values " *
                "$(truthvalues) must belong to the same algebra " *
                "(and have a common supertype that is not Truth)."))
        end

        ops = vcat(ops, truthvalues)
    end

    randformula(height, ops, atoms, args...; rng=initrng(rng), kwargs...)
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
    if !(all(x->x isa Operator, operators))
        throw(ArgumentError("Unexpected object(s) passed as" *
            " operator:" * " $(filter(x->!(x isa Operator), operators))"))
    end

    if (isnothing(opweights))
        opweights = StatsBase.uweights(length(operators))
    elseif (opweights isa AbstractVector)
        if !(length(opweights) == length(operators))
            throw(ArgumentError("Mismatching numbers of operators " *
                "($(length(operators))) and opweights ($(length(opweights)))."))
        end

        opweights = StatsBase.weights(opweights)
    end

    if (isnothing(atompicker))
        atompicker = StatsBase.uweights(natoms(alphabet))
    elseif (atompicker isa AbstractVector)
        if !(length(atompicker) == natoms(alphabet))
            throw(ArgumentError("Mismatching numbers of atoms " *
                "($(natoms(alphabet))) and atompicker ($(length(atompicker)))."))
        end

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
