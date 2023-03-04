using Random

doc_randformula = """
    randformulatree(
        height::Integer,
        alphabet::AbstractAlphabet,
        operators::Vector{<:AbstractOperator};
        rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG
    )::SyntaxTree

    function randformula(
        height::Integer,
        alphabet::AbstractAlphabet,
        operators::Vector{<:AbstractOperator};
        rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
    )::Formula

Return a pseudo-randomic `SyntaxTree` or `Formula`.

# Examples
```julia-repl
julia> syntaxstring(randformulatree(4, ExplicitAlphabet(Proposition.([1,2])), [NEGATION, CONJUNCTION, IMPLICATION]))
"¬((¬(¬(2))) → ((1 → 2) → (1 → 2)))"
```
See also [`randformula`](@ref), [`SyntaxTree`](@ref).
"""

"""$(doc_randformula)"""
function randformula(
    height::Integer,
    alphabet::AbstractAlphabet,
    operators::Vector{<:AbstractOperator};
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
)::Formula
    baseformula(
        randformulatree(height, alphabet, operators; rng = rng);
        alphabet = alphabet,
        additional_operators = operators,
    )
end

"""$(doc_randformula)"""
function randformulatree(
    height::Integer,
    alphabet::AbstractAlphabet,
    operators::Vector{<:AbstractOperator};
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
)::SyntaxTree
    
    function _randformulatree(
        height::Integer,
        alphabet::AbstractAlphabet,
        operators::Vector{<:AbstractOperator};
        rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
    )::SyntaxTree
        if height == 0
            return SyntaxTree(rand(rng, propositions(alphabet)))
        end

        op = rand(rng, operators)

        return SyntaxTree(
            op,
            Tuple([_randformulatree(height-1, alphabet, operators; rng=rng) for _ in 1:arity(op)])
        )
    end

    # If the alphabet is not iterable, this function should not work.
    # NOTE: the error message here is the same as in general.jl.
    if !isiterable(alphabet)
        return error("Please, provide method propositions(::$(typeof(a)))" *
        " to allow formula generation.")
    end

    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng

    return _randformulatree(height, alphabet, operators, rng=rng)
end
