using Random
using StatsBase

import Base: rand
import StatsBase: sample

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
@__rng_dispatch function SoleLogics.rand(
    rng::Union{Integer,AbstractRNG},
    a::AbstractAlphabet,
    args...;
    kwargs...
)
    randatom(initrng(rng), a, args...; kwargs...)
end
function rand(rng::Random.AbstractRNG, a::SoleLogics.AbstractAlphabet, args...; kwargs...)
    # This is needed to avoid a dispatch ambiguity caused by @__rng_dispatch:
    # rand(rng::Union{Integer, Random.AbstractRNG}, a::SoleLogics.AbstractAlphabet, args...; kwargs...)
    # @ SoleLogics ~/.julia/fork/SoleLogics.jl/src/generation/formula.jl:82
    # rand(rng::Random.AbstractRNG, X)
    # @ Random ~/.julia/julia_exec/julia-1.9.0/share/julia/stdlib/v1.9/Random/src/Random.jl:256
    randatom(rng, a, args...; kwargs...)
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
        throw(ArgumentError("Unexpected connectives and truth values: " *
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

    randformula(height, atoms, ops, args...; kwargs...)
end



"""$(sample_aw_docstring)"""
@__rng_dispatch function StatsBase.sample(
    rng::Union{Integer,AbstractRNG},
    alphabet::AbstractAlphabet,
    weights::AbstractWeights,
    args...;
    kwargs...
)
    if isfinite(alphabet)
        StatsBase.sample(initrng(rng), atoms(alphabet), weights, args...; kwargs...)
    else
        error("Please, provide method StatsBase.sample(rng::AbstractRNG, " *
            "alphabet::$(typeof(alphabet)), args...; kwargs...).")
    end
end

"""$(sample_lao_docstring)"""
@__rng_dispatch function StatsBase.sample(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    l::AbstractLogic,
    atomweights::AbstractWeights,
    opweights::AbstractWeights,
    args...;
    kwargs...
)
    StatsBase.sample(
        initrng(rng), height, grammar(l), atomweights, opweights, args...; kwargs...)
end

"""$(sample_hgao_docstring)"""
@__rng_dispatch function StatsBase.sample(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    g::AbstractGrammar,
    args...;
    atomweights::Union{Nothing,AbstractWeights}=nothing,
    opweights::Union{Nothing,AbstractWeights}=nothing,
    kwargs...
)
    randformula(
        initrng(rng), height, alphabet(g), operators(g), args...;
        atompicker = atomweights, opweights = opweights, kwargs...)
end



"""$(randformula_docstring)"""
@__rng_dispatch function randformula(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    alphabet::Union{AbstractVector,AbstractAlphabet},
    operators::AbstractVector{<:Operator},
    args...;
    modaldepth::Integer=height,
    atompicker::Union{Nothing,Function,AbstractWeights,AbstractVector{<:Real}}=randatom,
    opweights::Union{Nothing,AbstractWeights,AbstractVector{<:Real}}=nothing,
    alphabet_sample_kwargs::Union{Nothing,AbstractVector}=nothing,
    kwargs...
)
    rng = initrng(rng)
    alphabet = convert(AbstractAlphabet, alphabet)

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
            # sample operator and generate children
            # (modal connectives only if modaldepth is set > 0)
            ops, ops_w = (modaldepth > 0) ?
                (operators, opweights) :
                (operators[nonmodal_operators], opweights[nonmodal_operators])

            # op = rand(rng, ops)
            op = sample(rng, ops, ops_w)
            ch = Tuple([
                    _randformula(rng, height-1, modaldepth-(ismodal(op) ? 1 : 0))
                    for _ in 1:arity(op)])
            return SyntaxTree(op, ch)
        end
    end

    # if the alphabet is not iterable, this function should not work.
    if !isfinite(alphabet) && isnothing(alphabet_sample_kwargs)
        throw(ArgumentError("Attempting to generate random formulas from " *
            "(infinite) alphabet of type $(typeof(alphabet))!"))
    end

    return _randformula(rng, height, modaldepth)
end

"""$(randformula_hg_docstring)"""
@__rng_dispatch function randformula(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    g::AbstractGrammar,
    args...;
    kwargs...
)
    randformula(rng, height, alphabet(g), operators(g), args...; kwargs...)
end

@__rng_dispatch function randformula(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    args...;
    kwargs...
)
    randformula(initrng(rng), height, args...; kwargs...)
end


"""$(randbaseformula_docstring)"""
@__rng_dispatch function randbaseformula(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    alphabet::Union{AbstractVector,AbstractAlphabet},
    operators::AbstractVector{<:Operator},
    args...;
    kwargs...
)
    alphabet = convert(AbstractAlphabet, alphabet)
    baseformula(
        randformula(height, alphabet, operators, args...; kwargs...);
        alphabet=alphabet,
        additional_operators=operators,
    )
end

@__rng_dispatch function randbaseformula(
    rng::Union{Integer,AbstractRNG},
    height::Integer,
    g::AbstractGrammar,
    args...;
    kwargs...
)
    randbaseformula(rng, height, alphabet(g), operators(g), args...; kwargs...)
end
