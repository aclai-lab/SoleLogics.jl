randatom_docstring = """
    randatom(
        [rng::Union{Random.AbstractRNG,Integer},]
        a::AbstractAlphabet,
        args...;
        kwargs...
    )

Return a random atom from a *finite* alphabet.

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
        subalphabets_weights::Union{AbstractWeights,AbstractVector{<:Real},Nothing} = nothing
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
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        alphabet::AbstractAlphabet,
        args...;
        kwargs...
    )::Atom

Randomly generate an [`Atom`](@ref) from an [`AbstractAlphabet`](@ref) according to a
uniform distribution.

!!! details
If the `alphabet` is finite, the function defaults to `randatom(rng, alphabet)`, which
internally calls `Base.rand(rng, atoms(alphabet)`;
To limit the (otherwise infinite) sampling domain, a new dispatch must be implemented,
and additional keyword arguments should be provided: those will be forwarded to the innter
`randatom` call.

See also [`AbstractAlphabet`], [`randatom`](@ref).
"""

rand_abstractlogic_docstring = """
    function Base.rand(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
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
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        height::Integer,
        g::CompleteFlatGrammar,
        args...
    )::Formula

Generate a random formula of height `height`, honoring the grammar `g`.

See also [`CompleteFlatGrammar`](@ref).
"""

rand_granular_docstring = """
    Base.rand(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        height::Integer,
        connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
        atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
        truthvalues::Union{Nothing,AbstractAlgebra,AbstractVector{<:Truth}} = nothing,
        args...;
        rng::AbstractRNG = Random.GLOBAL_RNG,
        kwargs...
    )::Formula

See also [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref), [`Connective`](@ref),
[`Operator`](@ref).
"""
