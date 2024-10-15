randatom_docstring = """
    randatom(
        [rng::Union{Random.AbstractRNG,Integer},]
        a::AbstractAlphabet,
        args...;
        kwargs...
    )

Randomly generate an [`Atom`](@ref) from a *finite* [`AbstractAlphabet`](@ref) according to
a uniform distribution.

# Examples
```julia-repl
julia> alphabet = ExplicitAlphabet(1:5)
ExplicitAlphabet{Int64}(Atom{Int64}[Atom{Int64}: 1, Atom{Int64}: 2, Atom{Int64}: 3, Atom{Int64}: 4, Atom{Int64}: 5])

julia> randatom(42, alphabet)
Atom{Int64}: 4
```

See also [`natoms`](@ref), [`AbstractAlphabet`](@ref).
"""

randatom_unionalphabet_docstring = """
    randatom(
        [rng::Union{Random.AbstractRNG,Integer},]
        a::UnionAlphabet;
        atompicking_mode::Symbol=:uniform,
        subalphabets_weights::Union{AbstractWeights,AbstractVector{<:Real},Nothing}=nothing
    )::Atom

Sample an atom from a `UnionAlphabet`.
By default, the sampling is uniform with respect to the atoms.

By setting `atompicking_mode = :uniform_subalphabets` one can force a uniform sampling with
respect to the sub-alphabets.

Moreover, one can specify a `:weighted` `atompicking_mode`, together with a
`subalphabets_weights` vector.

# Examples
```julia-repl
julia> alphabet1 = ExplicitAlphabet(Atom.(1:10));
julia> alphabet2 = ExplicitAlphabet(Atom.(11:20));
julia> union_alphabet = UnionAlphabet([alphabet1, alphabet2]);

julia> randatom(42, union_alphabet)
Atom{Int64}: 11

julia> randatom(42, union_alphabet; atompicking_mode=:uniform_subalphabets)
Atom{Int64}: 11

julia> for i in 1:10
            randatom(
                union_alphabet;
                atompicking_mode=:weighted,
                subalphabets_weights=[0.8,0.2]
            ) |> syntaxstring |> vcat |> print
        end
["6"]["3"]["10"]["7"]["2"]["2"]["6"]["9"]["20"]["16"]
```

See also [`UnionAlphabet`](@ref).
"""

rand_abstractalphabet_docstring = """
    Base.rand(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        alphabet::AbstractAlphabet,
        args...;
        kwargs...
    )::Atom

Synonym for [`randatom(::AbstractAlphabet)`](@ref).

Randomly generate an [`Atom`](@ref) from a *finite* [`AbstractAlphabet`](@ref) according to
a uniform distribution.

See also [`AbstractAlphabet`], [`randatom(::AbstractAlphabet)`](@ref).
"""

rand_abstractlogic_docstring = """
    function Base.rand(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        height::Integer,
        l::AbstractLogic,
        args...;
        kwargs...
    )

Generate a random formula of height `height` and belonging to logic `l`.

See also [`AbstractLogic`](@ref).
"""

rand_completeflatgrammar_docstring = """
    Base.rand(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        height::Integer,
        g::CompleteFlatGrammar,
        args...
    )::Formula

Generate a random formula of height `height`, honoring the grammar `g`.

See also [`CompleteFlatGrammar`](@ref).
"""

rand_granular_docstring = """
    Base.rand(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        height::Integer,
        connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
        atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
        truthvalues::Union{Nothing,AbstractAlgebra,AbstractVector{<:Truth}}=nothing,
        args...;
        rng::AbstractRNG = Random.GLOBAL_RNG,
        kwargs...
    )::Formula

See also [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref), [`Connective`](@ref),
[`Operator`](@ref).
"""

sample_aw_docstring = """
    function StatsBase.sample(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        alphabet::AbstractAlphabet,
        weights::AbstractWeights,
        args...;
        kwargs...
    )

Sample an [`Atom`](@ref) from an `alphabet`, with probabilities proportional to the weights
given in `weights`.

See also [`AbstractAlphabet`](@ref), [`AbstractWeights`](@ref), [`Atom`](@ref).
"""

sample_lao_docstring = """
    function StatsBase.sample(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        l::AbstractLogic,
        weights::AbstractWeights,
        args...;
        kwargs...
    )

Sample from the [`grammar`](@ref) of logic `l`, with probabilities proportional to the
weights given in `weights`.

See also [`AbstractLogic`](@ref), [`AbstractWeights`](@ref),
[`grammar(::AbstractLogic{G}) where {G}`](@ref).
"""

sample_hgao_docstring = """
    function StatsBase.sample(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        height::Integer,
        g::AbstractGrammar,
        atomweights::Union{Nothing,AbstractWeights}=nothing,
        opweights::Union{Nothing,AbstractWeights}=nothing,
        args...;
        kwargs...
    )

Sample a formula from grammar `g`.
[`Atom`](@ref)s and [`Operator`](@ref)s sampling probabilities are proportional
respectively to `atomweights` and `opweights`.

See also [`AbstractGrammar`](@ref), [`AbstractWeights`](@ref), [`Atom`](@ref),
[`Operator`](@ref).
"""

randformula_docstring = """
    function randformula(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        height::Integer,
        alphabet::Union{AbstractVector,AbstractAlphabet},
        operators::AbstractVector,
        args...;
        modaldepth::Integer=height,
        atompicker::Union{Function,AbstractWeights,AbstractVector{<:Real},Nothing}=randatom,
        opweights::Union{AbstractWeights,AbstractVector{<:Real},Nothing}=nothing,
        kwargs...
    )

Return a pseudo-randomic [`SyntaxTree`](@ref).

# Arguments
- `rng::Union{Intger,AbstractRNG}=Random.GLOBAL_RNG`: random number generator;
- `height::Integer`: height of the generated structure;
- `alphabet::AbstractAlphabet`: collection from which atoms are chosen randomly;
- `operators::AbstractVector`: vector from which legal operators are chosen.

# Keyword Arguments
- `modaldepth::Integer`: maximum modal depth;
- `atompicker::Function`: method used to pick a random element. For example, this could be
    Base.rand or StatsBase.sample;
- `opweights::AbstractWeights`: operators are sampled with probabilities proportional to
    this vector vector (see [`AbstractWeights`](@ref) of StatsBase package).

# Examples

```julia-repl
julia> syntaxstring(randformula(4, ExplicitAlphabet([1,2]), [NEGATION, CONJUNCTION, IMPLICATION]))
"¬((¬(¬(2))) → ((1 → 2) → (1 → 2)))"
```

See also [`AbstractAlphabet`](@ref), [`AbstractWeights`](@ref), [`Atom`](@ref),
[`Operator`](@ref), [`SyntaxBranch`](@ref), [`SyntaxTree`](@ref).
"""

randformula_hg_docstring = """
    function randformula(
        [rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,]
        height::Integer,
        [g::AbstractGrammar,]
        args...;
        kwargs...
    )

Fallback to `randformula`, specifying only the `height` (possibly also a `grammar`) of the
generated [`SyntaxTree`](@ref).

See also [`AbstractGrammar`](@ref),
[`randformula(::Integer, ::Union{AbstractVector,AbstractAlphabet}, ::AbstractVector)`](@ref).
"""
