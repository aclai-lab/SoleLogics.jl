export propositional_logic

export Interpretation, TruthDict, DefaultedTruthDict

const BASE_PROPOSITIONAL_OPERATORS = BASE_OPERATORS
const BasePropositionalOperators = Union{typeof.(BASE_PROPOSITIONAL_OPERATORS)...}

"""
    propositional_logic(;
        alphabet = AlphabetOfAny{String}(),
        operators = [⊤, ⊥, ¬, ∧, ∨, →],
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), [⊤, ⊥, ¬, ∧, ∨, →]),
        algebra = BooleanAlgebra(),
    )

Instantiates a [propositional logic](https://simple.m.wikipedia.org/wiki/Propositional_logic)
given a grammar and an algebra. Alternatively, an alphabet and a set of operators
can be specified instead of the grammar.


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
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing
)
    _base_logic(
        alphabet = alphabet,
        operators = operators,
        grammar = grammar,
        algebra = algebra;
        default_operators = BASE_PROPOSITIONAL_OPERATORS,
        logictypename = "propositional logic",
    )
end

# A propositional logic based on the base propositional operators
const BasePropositionalLogic = AbstractLogic{G,A} where {ALP,G<:AbstractGrammar{ALP,<:BasePropositionalOperators},A<:AbstractAlgebra}

############################################################################################

"""
    abstract type Interpretation{A,T<:TruthValue} <: AbstractLogicalModel{A,T} end

A propositional interpretation, encoding a mapping from `Proposition`s of atom type `A`
to truth values of type `T`.

See also [`AbstractLogicalModel`](@ref).
"""
abstract type Interpretation{A,T<:TruthValue} <: AbstractLogicalModel{A,T} end

"""
    Base.getindex(m::Interpretation{AA,T}, p::Proposition{AA}, args...)::T where {AA,A<:AA,T<:TruthValue}

Each interpretation must provide a method for accessing the truth of a proposition.
"""
function Base.getindex(
    m::Interpretation{AA,T},
    ::Proposition{A},
    args...
)::T where {AA,A<:AA,T<:TruthValue}
    return error("Please, provide method" *
                 " Base.getindex(::$(typeof(m))," *
                 " ::Proposition{$(atomtype(m))}, args...)::$(truthtype(m))" *
                 " with args::$(typeof(args)).")
end

"""
    Base.in(::Proposition{A}, m::Interpretation{A})::Bool where {A}

Each interpretation must provide a method for expressing whether the model has a truth value
for a given proposition.
"""
function Base.in(::Proposition{A}, m::Interpretation{AA})::Bool where {AA,A<:AA}
    return error("Please, provide method" *
                 " Base.in(::Proposition{$(atomtype(m))}," *
                 " ::$(typeof(m)))::Bool.")
end

"""
    check(f::AbstractFormula, m::Interpretation, args...)

Checks a formula, represented as a syntax tree, on a model. It returns a `TruthValue`.

# Extended help
The fallback method extracts the formula's syntax tree and checks it using the logic's
algebra.

    check(
        a::AbstractAlgebra,
        tree::SyntaxTree,
        m::Interpretation{A,T},
        args...
    )::T where {A,T<:TruthValue}

"""
check(f::AbstractFormula, m::Interpretation, args...) = check(algebra(f), tree(f), m, args...)

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
        return error("Unknown token type encountered when checking formula" *
                     " on model of type $(typeof(m)): $(typeof(token(tree))).")
    end
end

# Helper: a proposition can be checked on an interpretation; a simple lookup is performed.
check(p::Proposition{A}, m::Interpretation{AA}, args...) where {AA,A<:AA} = Base.getindex(m, p, args...)

"""
    struct TruthDict{A,T<:TruthValue} <: Interpretation{A,T}
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
        return TruthDict{A,T}(d)
    end
    function TruthDict(v::AbstractVector{Tuple{Proposition{A},T}}) where {A,T<:TruthValue}
        return TruthDict(Dict(v))
    end
    function TruthDict(v::AbstractVector{Pair{Proposition{A},T}}) where {A,T<:TruthValue}
        return TruthDict(Dict(v))
    end
    function TruthDict(p::Pair{Proposition{A},T}) where {A,T<:TruthValue}
        return TruthDict([p])
    end
    function TruthDict(t::Tuple{Proposition{A},T}) where {A,T<:TruthValue}
        return TruthDict(Pair(t...))
    end
end

Base.getindex(m::TruthDict{AA}, p::Proposition{A}) where {AA,A<:AA} = m.truth[p]
Base.in(p::Proposition{A}, m::TruthDict{AA}) where {AA,A<:AA} = (p in keys(m.truth))

"""
    struct DefaultedTruthDict{A,T<:TruthValue} <: Interpretation{A,T}
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
        return DefaultedTruthDict{A,T}(d, default_truth)
    end
    function DefaultedTruthDict(
        v::AbstractVector{Tuple{Proposition{A},T}},
        default_truth::T
    ) where {A,T<:TruthValue}
        return DefaultedTruthDict(Dict(v), default_truth)
    end
    function DefaultedTruthDict(
        v::AbstractVector,
        default_truth::T
    ) where {T<:TruthValue}
        return DefaultedTruthDict(
            Vector{Tuple{Proposition{TruthValue},T}}(v),
            default_truth
        )
        return DefaultedTruthDict(
            v::AbstractVector{Pair{Proposition{A},T}},
            default_truth::T
        ) where {A,T<:TruthValue}
        return DefaultedTruthDict(Dict(v), default_truth)
    end
    function DefaultedTruthDict(
        p::Pair{Proposition{A},T},
        default_truth::T
    ) where {A,T<:TruthValue}
        return DefaultedTruthDict([p], default_truth)
    end
    function DefaultedTruthDict(
        t::Tuple{Proposition{A},T},
        default_truth::T
    ) where {A,T<:TruthValue}
        return DefaultedTruthDict(Pair(t...), default_truth)
    end
    function DefaultedTruthDict(default_truth::T) where {T<:TruthValue}
        return DefaultedTruthDict([], default_truth)
    end
end

function Base.getindex(m::DefaultedTruthDict{AA}, p::Proposition{A}) where {AA,A<:AA}
    return (p in keys(m.truth)) ? m.truth[p] : m.default_truth
end
Base.in(p::Proposition{A}, m::DefaultedTruthDict{AA}) where {AA,A<:AA} = true
