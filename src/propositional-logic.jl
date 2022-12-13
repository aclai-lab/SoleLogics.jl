export propositional_logic

export Interpretation, TruthDict

"""
    propositional_logic(;
        alphabet = AlphabetOfAny{String}(),
        operators = [⊤, ⊥, ¬, ∧, ∨, ⟹],
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), [⊤, ⊥, ¬, ∧, ∨, ⟹]),
        algebra = BooleanAlgebra(),
    )

Instantiates a [propositional logic](https://simple.m.wikipedia.org/wiki/Propositional_logic)
given the values of (grammar, algebra) or (alphabet, operators, algebra).

TODO: the examples do not work (since the function is not exported?)
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
    alphabet::Union{Vector,AbstractAlphabet}=base_alphabet,
    operators::Vector{<:AbstractOperator}=base_operators,
    grammar::AbstractGrammar=base_grammar, # TODO: never used
    algebra::AbstractAlgebra=base_algebra
)
    if alphabet isa Vector
        alphabet = ExplicitAlphabet(map(Proposition, alphabet))
    end
    grammar = CompleteFlatGrammar{typeof(alphabet)}(alphabet, operators)
    BaseLogic(grammar, algebra)
end

# A propositional logic based on the base operators
const BasePropositionalLogic = AbstractLogic{G,A} where {ALP,G<:AbstractGrammar{ALP,<:BaseOperators},A<:AbstractAlgebra}

"""
    abstract type Interpretation{A, T<:TruthValue} <: AbstractLogicalModel{A, T} end

A propositional interpretation, encoding a mapping from `Proposition`'s of atom type `A`
to truth values of type `T`.

See also [`AbstractLogicalModel`](@ref).
"""
abstract type Interpretation{A,T<:TruthValue} <: AbstractLogicalModel{A,T} end

"""
    Base.getindex(m::Interpretation{A, T}, p::Proposition{A}, args...)::T where {A, T<:TruthValue}

Each interpretation must provide a method for accessing the truth of a proposition.
"""
function Base.getindex(
    m::Interpretation{A,T},
    ::Proposition{A},
    args...
)::T where {A,T<:TruthValue}
    return error("Please, provide method
        Base.getindex(m::$(typeof(m)),
        p::Proposition{$(atomtype(m))}, args...)::$(truthtype(m))
        with args::$(typeof(args)).")
end

# TODO: docstring out of bounds
# TODO: this docstring talks about getindex, but the following defineds Base.in -- please resolve
"""
    Base.getindex(m::Interpretation{A, T}, p::Proposition{A}, args...)::T where {A, T<:TruthValue}

Each interpretation must provide a method for expressing whether the model has a truth value
for a given proposition.
"""
function Base.in(::Proposition{A}, m::Interpretation{A})::Bool where {A}
    # TODO: control the error message to be homogeneous with the others (e.g., `p` and `m` are required in the message?)
    return error("Please, provide method
        Base.in(p::Proposition{$(atomtype(m))},
        m::$(typeof(m)))::Bool where {A}.")
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

TODO: Should I control it?
"""
function check(
    a::AbstractAlgebra,
    tree::SyntaxTree,
    m::Interpretation{A,T},
    args...
)::T where {A,T<:TruthValue}
    if token(tree) isa Proposition
        return Base.getindex(m, token(tree), args...)
    elseif token(tree) isa AbstractOperator
        ts = Tuple([check(a, childtree, m, args...) for childtree in children(tree)])
        return collate_truth(a, token(tree), ts)
    else
        return error("Unknown token type encountered when checking formula
            on model of type $(typeof(m)): $(typeof(token(tree))).")
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
If prompted for the value of an unknown proposition, it throws an error.

See also [`Interpretation`](@ref), [`AbstractLogicalModel`](@ref).
"""
struct TruthDict{A,T<:TruthValue} <: Interpretation{A,T}
    truth::Dict{Proposition{A},T}

    function TruthDict{A,T}(d::Dict{Proposition{A},T}) where {A,T<:TruthValue}
        return new{A,T}(d)
    end
    function TruthDict(d::Dict{Proposition{A},T}) where {A,T<:TruthValue}
        return TruthDict{A,T}(d) # TODO: why not new instead of TruthDict?
    end
    function TruthDict(v::AbstractVector{Tuple{Proposition{A},T}}) where {A,T<:TruthValue}
        return TruthDict(Dict(v)) # TODO: as above
    end
    function TruthDict(v::AbstractVector{Pair{Proposition{A},T}}) where {A,T<:TruthValue}
        return TruthDict(Dict(v)) # TODO: as above
    end
    function TruthDict(p::Pair{Proposition{A},T}) where {A,T<:TruthValue}
        return TruthDict([p]) # TODO: as above
    end
    function TruthDict(t::Tuple{Proposition{A},T}) where {A,T<:TruthValue}
        return TruthDict(Pair(t...)) # TODO: as above
    end
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
struct DefaultedTruthDict{A,T<:TruthValue} <: Interpretation{A,T}
    truth::Dict{Proposition{A},T}
    default_truth::T

    function DefaultedTruthDict{A,T}(
        d::Dict{Proposition{A},T},
        default_truth::T
    ) where {A,T<:TruthValue}
        return new{A,T}(d, default_truth)
    end
    function DefaultedTruthDict(
        d::Dict{Proposition{A},T},
        default_truth::T
    ) where {A,T<:TruthValue}
        return DefaultedTruthDict{A,T}(d, default_truth) # TODO: as above (i.e., why not new?)
    end
    function DefaultedTruthDict(
        v::AbstractVector{Tuple{Proposition{A},T}},
        default_truth::T
    ) where {A,T<:TruthValue}
        return DefaultedTruthDict(Dict(v), default_truth) # TODO: as above
    end
    function DefaultedTruthDict(
        v::AbstractVector,
        default_truth::T
    ) where {T<:TruthValue}
        return DefaultedTruthDict(
            Vector{Tuple{Proposition{TruthValue},T}}(v),
            default_truth
        ) # TODO: as above
        return DefaultedTruthDict(
            v::AbstractVector{Pair{Proposition{A},T}},
            default_truth::T
        ) where {A,T<:TruthValue}
        return DefaultedTruthDict(Dict(v), default_truth) # TODO: as above
    end
    function DefaultedTruthDict(
        p::Pair{Proposition{A},T},
        default_truth::T
    ) where {A,T<:TruthValue}
        return DefaultedTruthDict([p], default_truth) # TODO: as above
    end
    function DefaultedTruthDict(
        t::Tuple{Proposition{A},T},
        default_truth::T
    ) where {A,T<:TruthValue}
        return DefaultedTruthDict(Pair(t...), default_truth) # TODO: as above
    end
    function DefaultedTruthDict(default_truth::T) where {T<:TruthValue}
        return DefaultedTruthDict([], default_truth) # TODO: as above
    end
end

function Base.getindex(m::DefaultedTruthDict{AA}, p::Proposition{A}) where {AA,A<:AA}
    return (p in keys(m.truth)) ? m.truth[p] : m.default_truth
end
Base.in(p::Proposition{A}, m::DefaultedTruthDict{AA}) where {AA,A<:AA} = true
