export propositional_logic

export Interpretation, TruthDict

"""
    propositional_logic(;
        alphabet = AlphabetOfAny{String}(),
        operators = [⊤, ⊥, ¬, ∧, ∨, →],
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), [⊤, ⊥, ¬, ∧, ∨, →]),
        algebra = BooleanAlgebra(),
    )

Instantiates a [propositional logic](https://simple.m.wikipedia.org/wiki/Propositional_logic)
given the values of (grammar, algebra) or (alphabet, operators, algebra).

# Examples
```julia-repl
julia> propositional_logic()
julia> propositional_logic(; operators = [¬, ∨])
julia> propositional_logic(; alphabet = ["p", "q"])
julia> propositional_logic(; alphabet = ExplicitAlphabet([Proposition("p"), Proposition("q")]))
```

See also [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref).
"""
function propositional_logic(;
    alphabet::Union{Vector, AbstractAlphabet} = base_alphabet,
    operators::Vector{<:AbstractOperator} = base_operators,
    grammar::AbstractGrammar = base_grammar,
    algebra::AbstractAlgebra = base_algebra,
)
    if alphabet isa Vector
        alphabet = ExplicitAlphabet(map(Proposition, alphabet))
    end
    grammar = CompleteFlatGrammar{typeof(alphabet)}(alphabet, operators)
    BaseLogic(grammar, algebra)
end

# A propositional logic based on the base operators
const BasePropositionalLogic = AbstractLogic{G, A} where {ALP, G<:AbstractGrammar{ALP, <:BaseOperators}, A<:AbstractAlgebra}

"""
    abstract type Interpretation{A, T<:TruthValue} <: AbstractLogicalModel{A, T} end

A propositional interpretation, encoding a mapping from `Proposition`'s of atom type `A`
to truth values of type `T`.

See also [`AbstractLogicalModel`](@ref).
"""
abstract type Interpretation{A, T<:TruthValue} <: AbstractLogicalModel{A, T} end

"""
    Base.getindex(m::Interpretation{A, T}, p::Proposition{A}, args...)::T where {A, T<:TruthValue}

Each interpretation must provide a method for accessing the truth of a proposition.
"""
function Base.getindex(m::Interpretation{A, T}, p::Proposition{A}, args...)::T where {A, T<:TruthValue}
    error("Please, provide method Base.getindex(m::$(typeof(m)), p::Proposition{$(atomtype(m))}, args...)::$(truthtype(m)) with args::$(typeof(args)).")
end

"""
    Base.getindex(m::Interpretation{A, T}, p::Proposition{A}, args...)::T where {A, T<:TruthValue}

Each interpretation must provide a method for expressing whether the model has a truth value
for a given proposition.
"""
function Base.in(p::Proposition{A}, m::Interpretation{A})::Bool where {A}
    error("Please, provide method Base.in(p::Proposition{$(atomtype(m))}, m::$(typeof(m)))::Bool where {A}.")
end

"""
    check(
        a::AbstractAlgebra,
        tree::SyntaxTree,
        m::Interpretation{A, T},
        args...
    )::T where {A, T<:TruthValue}

Checks a formula, represented as a syntax tree, on a model.
TODO Fix. can provide a check method. The fallback is:
"""
function check(
    a::AbstractAlgebra,
    tree::SyntaxTree,
    m::Interpretation{A, T},
    args...
)::T where {A, T<:TruthValue}
    if token(tree) isa Proposition
        Base.getindex(m, token(tree), args...)
    elseif token(tree) isa AbstractOperator
        ts = Tuple([check(a, childtree, m, args...) for childtree in children(tree)])
        collate_truth(a, token(tree), ts)
    else
        error("Unknown token type encountered when checking formula on model of type $(typeof(m)): $(typeof(token(tree))).")
    end
end

# Helpers (note how the fallback checking algorithm only involves the formula's algebra and its syntax tree)
check(f::AbstractFormula, m::Interpretation, args...) = check(algebra(f), tree(f), m, args...)
check(p::Proposition{A}, m::Interpretation{A}, args...) where {A} = Base.getindex(m, p, args...)

"""
    struct TruthDict{A, T<:TruthValue} <: Interpretation{A, T}
        truth::Dict{Proposition{A},T}
    end

A truth table instantiated as a dictionary, assigning truth values to a set of propositions.
If prompted for the value of an unknown proposition, it throws error.

See also [`Interpretation`](@ref), [`AbstractLogicalModel`](@ref).
"""
struct TruthDict{A, T<:TruthValue} <: Interpretation{A, T}
    truth::Dict{Proposition{A},T}

    TruthDict{A, T}(d::Dict{Proposition{A}, T}) where {A, T<:TruthValue} =
        new{A, T}(d)

    TruthDict(d::Dict{Proposition{A}, T}) where {A, T<:TruthValue} =
        TruthDict{A, T}(d)
    TruthDict(v::AbstractVector{Tuple{Proposition{A}, T}}) where {A, T<:TruthValue} =
        TruthDict(Dict(v))
    TruthDict(v::AbstractVector{Pair{Proposition{A}, T}}) where {A, T<:TruthValue} =
        TruthDict(Dict(v))
    TruthDict(p::Pair{Proposition{A}, T}) where {A, T<:TruthValue} =
        TruthDict([p])
    TruthDict(t::Tuple{Proposition{A}, T}) where {A, T<:TruthValue} =
        TruthDict(Pair(t...))
end

Base.getindex(m::TruthDict{A}, p::Proposition{A}) where {A} = m.truth[p]
Base.in(p::Proposition{A}, m::TruthDict{A}) where {A} = (p in keys(m.truth))

"""
    struct DefaultedTruthDict{A, T<:TruthValue} <: Interpretation{A, T}
        truth::Dict{Proposition{A},T}
        default_truth::T
    end

A truth table instantiated as a dictionary, plus a default value.
This structure assigns truth values to a set of propositions and,
when prompted for the value of a proposition that is not in the dictionary,
it returns `default_truth`.

See also [`TruthDict`](@ref), [`Interpretation`](@ref), [`AbstractLogicalModel`](@ref).
"""
struct DefaultedTruthDict{A, T<:TruthValue} <: Interpretation{A, T}
    truth::Dict{Proposition{A},T}
    default_truth::T

    DefaultedTruthDict{A, T}(d::Dict{Proposition{A}, T}, default_truth::T) where {A, T<:TruthValue} =
        new{A, T}(d, default_truth)

    DefaultedTruthDict(d::Dict{Proposition{A}, T}, default_truth::T) where {A, T<:TruthValue} =
        DefaultedTruthDict{A, T}(d, default_truth)
    DefaultedTruthDict(v::AbstractVector{Tuple{Proposition{A}, T}}, default_truth::T) where {A, T<:TruthValue} =
        DefaultedTruthDict(Dict(v), default_truth)
    DefaultedTruthDict(v::AbstractVector, default_truth::T) where {T<:TruthValue} =
        DefaultedTruthDict(Vector{Tuple{Proposition{TruthValue},T}}(v), default_truth)
    DefaultedTruthDict(v::AbstractVector{Pair{Proposition{A}, T}}, default_truth::T) where {A, T<:TruthValue} =
        DefaultedTruthDict(Dict(v), default_truth)
    DefaultedTruthDict(p::Pair{Proposition{A}, T}, default_truth::T) where {A, T<:TruthValue} =
        DefaultedTruthDict([p], default_truth)
    DefaultedTruthDict(t::Tuple{Proposition{A}, T}, default_truth::T) where {A, T<:TruthValue} =
        DefaultedTruthDict(Pair(t...), default_truth)
    DefaultedTruthDict(default_truth::T) where {T<:TruthValue} =
        DefaultedTruthDict([], default_truth)
end

Base.getindex(m::DefaultedTruthDict{AA}, p::Proposition{A}) where {AA, A<:AA} = (p in keys(m.truth)) ? m.truth[p] : m.default_truth
Base.in(p::Proposition{A}, m::DefaultedTruthDict{AA}) where {AA, A<:AA} = true
