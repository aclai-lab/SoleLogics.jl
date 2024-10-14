using Random
using StatsBase

import Random: rand
import StatsBase: sample

include("docstrings.jl")

macro __rng_dispatch(ex)
    if ex.head != :function
        throw(ArgumentError("Expected a function definition"))
    end

    fsignature = ex.args[1]
    fname = fsignature.args[1]

    @show fsignature.args

    fargs = fsignature.args[2:end]
    # At this point, fargs is shaped similar to:
    # Any[
    #   :($(Expr(:parameters, :(kwargs...)))),
    #   :(rng::Union{Random.AbstractRNG, Integer}), :(a::AbstractAlphabet), :(args...)
    # ]

    # Extract the arguments after the first one;
    # the first one must be an Union{Random.AbstractRNG,Integer},
    # otherwise, this macro makes no sense.
    if length(fargs) > 0
        quote
            if !isa($fargs[1], Union{Random.AbstractRNG,Integer})
                throw(ArgumentError("Expected function's first argument to be of type " *
                    "Union{Random.AbstractRNG,Integer}.")
                )
            end
        end

        # From fargs, we would like to isolate the first field (kwargs),
        # skip the second field (rng), and go on.
        newargs = Any[fargs[1], fargs[3:end]...]
    else
        throw(ArgumentError("Expected function's argument to be atleast 2, the first of " *
            "which of type Union{Random.AbstractRNG,Integer}."))
    end

    # Define both dispatches;
    # the names are the same, and the new dispatches (the one without rng)
    # also gets the docstring written just before the macro invocation.
    quote
        Core.@__doc__ function $(esc(fname))($(newargs...))
            $fname(Random.GLOBAL_RNG, $(newargs))
        end

        $(esc(ex))
    end
end

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

        rng = initrng(rng)
        return Base.rand(rng, atoms(a), args...; kwargs...)
    else
        error("Please provide method randatom(rng::$(typeof(rng)), " *
            "alphabet::$(typeof(a)), args...; kwargs...)")
    end
end

# TODO - remove this dispatch if test works
# function randatom(a::AbstractAlphabet, args...; kwargs...)
#     randatom(Random.GLOBAL_RNG, a, args...; kwargs...)
# end

# @__rng_dispatch

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
    @assert atompicking_mode in _atompicking_modes "Invalid value for `atompicking_mode` " *
        "($(atompicking_mode)). Chosee between $(atompicking_modes)."

    rng = initrng(rng)
    alphs = subalphabets(a)

    if atompicking_mode == :weighted
        if isnothing(subalphabets_weights)
            error("`:weighted` picking_mode requires weights in `subalphabets_weights` ")
        end
        @assert length(subalphabets_weights)==length(alphs) "Mismatching numbers of "*
        "alphabets ($(length(alphs))) and weights ($(length(subalphabets_weights)))."
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



# TODO - remove this dispatch if test works
# function Base.rand(a::AbstractAlphabet, args...; kwargs...)
#     Base.rand(Random.GLOBAL_RNG, a, args...; kwargs...)
# end
"""$(rand_abstractalphabet_docstring)"""
@__rng_dispatch function Base.rand(
    rng::Union{Integer,AbstractRNG},
    a::AbstractAlphabet,
    args...;
    kwargs...
)
    randatom(initrng(rng), a, args...; kwargs...)
end


# TODO - remove this dispatch if test works
# function Base.rand(height::Integer, l::AbstractLogic, args...; kwargs...)
#     Base.rand(Random.GLOBAL_RNG, height, l, args...; kwargs...)
# end
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




# TODO - remove this dispatch if test works
# function Base.rand(
#     height::Integer,
#     g::CompleteFlatGrammar,
#     args...
# )
#     Base.rand(Random.GLOBAL_RNG, height, g, args...)
# end
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



# TODO - remove this dispatch if test works
# function Base.rand(
#     height::Integer,
#     atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
#     connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
#     truthvalues::Union{Nothing,AbstractAlgebra,AbstractVector{<:Truth}} = nothing,
#     args...;
#     kwargs...
# )
#     Base.rand(
#         Random.GLOBAL_RNG, height, atoms, connectives, truthvalues, args...; kwargs...)
# end
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
    rng = initrng(rng)

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
