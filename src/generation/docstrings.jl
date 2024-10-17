#formula.jl docstrings

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
        subalphabets_weights::Union{Nothing,AbstractWeights,AbstractVector{<:Real}}=nothing
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
        operators::AbstractVector{<:Operator},
        args...;
        modaldepth::Integer=height,
        atompicker::Union{Nothing,Function,AbstractWeights,AbstractVector{<:Real}}=randatom,
        opweights::Union{Nothing,AbstractWeights,AbstractVector{<:Real}}=nothing,
        alphabet_sample_kwargs::Union{Nothing,AbstractVector}=nothing,
        kwargs...
    )

Return a pseudo-randomic [`SyntaxTree`](@ref).

# Arguments
- `rng::Union{Intger,AbstractRNG}=Random.GLOBAL_RNG`: random number generator;
- `height::Integer`: height of the generated structure;
- `alphabet::AbstractAlphabet`: collection from which atoms are chosen randomly;
- `operators::AbstractVector{<:Operator}`: vector from which legal operators are chosen.

# Keyword Arguments
- `modaldepth::Integer`: maximum modal depth;
- `atompicker::Union{Nothing,Function,AbstractWeights,AbstractVector{<:Real}}`: method used
    to pick a random element. For example, this could be Base.rand, StatsBase.sample or
    an array of integers or an array of `StatsBase.AbstractWeights`;
- `opweights::Union{Nothing,AbstractWeights,AbstractVector{<:Real}}`: operators are sampled
    with probabilities proportional to this vector (see [`AbstractWeights`](@ref) of
    StatsBase package).
- `alphabet_sample_kwargs::AbstractVector`: pool of atoms to pull from if the given alphabet
    is not finite.

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

randbaseformula_docstring = """
    function randbaseformula(
        rng::Union{Integer,AbstractRNG},
        height::Integer,
        alphabet::Union{AbstractVector,AbstractAlphabet},
        operators::AbstractVector{<:Operator},
        args...;
        kwargs...
    )::AnchoredFormula

Similar to `randformula`, but generates an [`AnchoredFormula`](@ref) anchored to the logic
that includes [`Atom`](@ref)s in `alphabet` and [`Operator`](@ref)s in `operators`.

See also [`AnchoredFormula`](@ref), [`Atom`](@ref), [`Operator`](@ref),
[`randformula(::Integer, ::Union{AbstractVector,AbstractAlphabet}, ::AbstractVector)`](@ref)
"""



# models.jl docstrings

randframe_docstring = """
    function randframe(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        nworlds::Int64,
        nedges::Int64,
    end

Return a random Kripke Frame, which is a directed graph interpreted as a
[`SoleLogics.ExplicitCrispUniModalFrame`](@ref).
The underlying graph is generated using [`Graphs.SimpleGraphs.SimpleDiGraph`](@ref).

# Arguments
- `rng::Union{Intger,AbstractRNG}=Random.GLOBAL_RNG`: random number generator;
- `nworlds::Int64`: number of worlds (nodes) in the frame (numbered from `1` to `nworld`
    included).
- `nedges::Int64`: number of relations (edges) in the frame;

# Examples
```julia-repl
julia> randframe(Random.MersenneTwister(42),5,10)
SoleLogics.ExplicitCrispUniModalFrame{SoleLogics.World{Int64}, Graphs.SimpleGraphs.SimpleDiGraph{Int64}} with
- worlds = ["1", "2", "3", "4", "5"]
- accessibles =
        1 -> [2, 3, 5]
        2 -> [1, 4, 5]
        3 -> []
        4 -> [1, 2]
        5 -> [1, 2]
```

See also [`SoleLogics.ExplicitCrispUniModalFrame`](@ref), [`SyntaxLeaf`](@ref),
[`Graphs.SimpleGraphs.SimpleDiGraph`](@ref).
"""

randmodel_docstring = """
    function randmodel(
        [rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,]
        nworlds::Int64,
        nedges::Int64,
        facts::Vector{SyntaxLeaf};
        truthvalues::Union{AbstractAlgebra,AbstractVector{<:Truth}}
    )

# Arguments
- `rng::Union{Intger,AbstractRNG}=Random.GLOBAL_RNG`: random number generator;
- `nworlds::Int64`: number of worlds (nodes) in the frame (numbered from `1` to `nworld`
    included).
- `nedges::Int64`: number of relations (edges) in the frame;
- `facts::Int64`: vector of generic [`SyntaxLeaf`](@ref), representing facts to which a certain
    valuation function can associate a [`Truth`](@ref) value;
- `truthvalues::Union{AbstractAlgebra,AbstractVector{<:Truth}}`: [`Truth`](@ref) values to
    be associated for each element of `facts`.

# Examples
```julia-repl
julia> randmodel(Random.MersenneTwister(42),5,10, [Atom("s"), Atom("p")], BooleanAlgebra())
KripkeStructure{SoleLogics.ExplicitCrispUniModalFrame{SoleLogics.World{Int64}, Graphs.SimpleGraphs.SimpleDiGraph{Int64}}, Dict{SoleLogics.World{Int64}, TruthDict{Dict{Atom{String}, BooleanTruth}}}} with
- frame = SoleLogics.ExplicitCrispUniModalFrame{SoleLogics.World{Int64}, Graphs.SimpleGraphs.SimpleDiGraph{Int64}} with
- worlds = ["1", "2", "3", "4", "5"]
- accessibles =
        1 -> [2, 3, 5]
        2 -> [1, 4, 5]
        3 -> []
        4 -> [1, 2]
        5 -> [1, 2]
- valuations =
        1 -> TruthDict([s => ⊥, p => ⊤])
        2 -> TruthDict([s => ⊥, p => ⊥])
        3 -> TruthDict([s => ⊥, p => ⊥])
        4 -> TruthDict([s => ⊤, p => ⊤])
        5 -> TruthDict([s => ⊤, p => ⊤])
```

See also [`AbstractAlgebra`](@ref), [`SyntaxLeaf`](@ref), [`Truth`](@ref).
"""
