using Random

export generate

"""
    generate(
        height::Integer,
        alphabet::AbstractAlphabet,
        operators::Vector{<:AbstractOperator};
        rng::Union{Integer, AbstractRNG}=Random.GLOBAL_RNG
    )

Return a pseudo-randomic `SyntaxTree`.

See also [`SyntaxTree`](@ref)
"""
function generate(
    height::Integer,
    alphabet::AbstractAlphabet,
    operators::Vector{<:AbstractOperator};
    rng::Union{Integer, AbstractRNG} = Random.GLOBAL_RNG
)
    # If the alphabet is not iterable, this function should not work.
    # NOTE: the error message here is the same as in general.jl.
    if !isiterable(alphabet)
        return error("Please, provide method propositions(::$(typeof(a)))" *
        " to allow formula generation.")
    end

    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng

    return _generate(height, alphabet, operators, rng=rng)
end

function _generate(
    height::Integer,
    alphabet::AbstractAlphabet,
    operators::Vector{<:AbstractOperator};
    rng::Union{Integer, AbstractRNG} = Random.GLOBAL_RNG
)
    if height == 0
        return SyntaxTree(rand(rng, propositions(alphabet)))
    end

    op = rand(rng, operators)

    return SyntaxTree(op,
        Tuple([_generate(height-1, alphabet, operators; rng=rng) for _ in 1:arity(op)]))
end
