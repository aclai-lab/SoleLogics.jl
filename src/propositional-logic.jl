
const BASE_PROPOSITIONAL_OPERATORS = BASE_OPERATORS
const BasePropositionalOperators = Union{typeof.(BASE_PROPOSITIONAL_OPERATORS)...}

"""
    propositionallogic(;
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
julia> (¬) isa operatorstype(propositionallogic())
true

julia> (¬) isa operatorstype(propositionallogic(; operators = [∨]))
false

julia> propositionallogic(; alphabet = ["p", "q"]);

julia> propositionallogic(; alphabet = ExplicitAlphabet([Proposition("p"), Proposition("q")]));

```

See also [`modallogic`](@ref), [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref).
"""
function propositionallogic(;
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
)
    _baselogic(
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
    abstract type AbstractAssignment{A,T<:TruthValue} <: AbstractInterpretation{A,T} end

A propositional assigment (or, simply, an *assigment*) is a propositional interpretation,
encoding a mapping from `Proposition`s of atom type `A`
to truth values of type `T`.

See also [`AbstractInterpretation`](@ref).
"""
abstract type AbstractAssignment{A,T<:TruthValue} <: AbstractInterpretation{A,T} end

"""
    Base.getindex(i::AbstractAssignment{AA,T}, p::Proposition{AA}, args...)::T where {AA,A<:AA,T<:TruthValue}

Returns the truth value of a proposition, given an assignment.

See also [`AbstractInterpretation`](@ref).
"""
function Base.getindex(
    i::AbstractAssignment{AA,T},
    ::Proposition{A},
    args...
)::T where {AA,A<:AA,T<:TruthValue}
    return error("Please, provide method" *
                 " Base.getindex(::$(typeof(i))," *
                 " ::Proposition{$(atomtype(i))}, args...)::$(truthtype(i))" *
                 " with args::$(typeof(args)).")
end

"""
    Base.in(::Proposition{A}, i::AbstractAssignment{A})::Bool where {A}

Returns whether an assigment has a truth value for a given proposition.

See also [`AbstractInterpretation`](@ref).
"""
function Base.in(::Proposition{A}, i::AbstractAssignment{AA})::Bool where {AA,A<:AA}
    return error("Please, provide method" *
                 " Base.in(::Proposition{$(atomtype(i))}," *
                 " ::$(typeof(i)))::Bool.")
end

"""
    check(
        f::AbstractFormula,
        i::AbstractAssignment::{A,T},
        args...
    )::T where {A,T<:TruthValue}

Checks a logical formula on an assigment.
It returns a truth value of the assigment.

See also [`SyntaxTree`](@ref), [`AbstractFormula`](@ref),
[`AbstractAlgebra`](@ref), [`AbstractInterpretation`](@ref).

# Extended help
The fallback method extracts the formula's syntax tree and checks it using the logic's
algebra.

    check(
        a::AbstractAlgebra,
        tree::SyntaxTree,
        i::AbstractAssignment{A,T},
        args...
    )::T where {A,T<:TruthValue}
"""
check(f::AbstractFormula, i::AbstractAssignment, args...) = check(algebra(f), tree(f), i, args...)

function check(
    a::AbstractAlgebra,
    tree::SyntaxTree,
    i::AbstractAssignment{A,T},
    args...
)::T where {A,T<:TruthValue}
    if token(tree) isa Proposition
        return Base.getindex(i, token(tree), args...)
    elseif token(tree) isa AbstractOperator
        ts = Tuple([check(a, childtree, i, args...) for childtree in children(tree)])
        return collatetruth(a, token(tree), ts)
    else
        return error("Unknown token type encountered when checking formula" *
                     " on interpretation of type $(typeof(i)): $(typeof(token(tree))).")
    end
end

# Helper: a proposition can be checked on an interpretation; a simple lookup is performed.
check(p::Proposition{A}, i::AbstractAssignment{AA}, args...) where {AA,A<:AA} = Base.getindex(i, p, args...)

############################################################################################

"""
    struct TruthDict{A,T<:TruthValue} <: AbstractAssignment{A,T}
        truth::Dict{Proposition{A},T}
    end

A truth table instantiated as a dictionary,
explicitly assigning truth values to a set of propositions.
If prompted for the value of an unknown proposition, it throws an error.

See also [`AbstractAssignment`](@ref), [`AbstractInterpretation`](@ref).
"""
struct TruthDict{A,T<:TruthValue} <: AbstractAssignment{A,T}
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

Base.getindex(i::TruthDict{AA}, p::Proposition{A}) where {AA,A<:AA} = i.truth[p]
Base.in(p::Proposition{A}, i::TruthDict{AA}) where {AA,A<:AA} = (p in keys(i.truth))

############################################################################################

"""
    struct DefaultedTruthDict{A,T<:TruthValue} <: AbstractAssignment{A,T}
        truth::Dict{Proposition{A},T}
        default_truth::T
    end

A truth table instantiated as a dictionary, plus a default value.
This structure assigns truth values to a set of propositions and,
when prompted for the value of a proposition that is not in the dictionary,
it returns `default_truth`.

See also [`TruthDict`](@ref), [`AbstractAssignment`](@ref), [`AbstractInterpretation`](@ref).
"""
struct DefaultedTruthDict{A,T<:TruthValue} <: AbstractAssignment{A,T}
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

function Base.getindex(i::DefaultedTruthDict{AA}, p::Proposition{A}) where {AA,A<:AA}
    return (p in keys(i.truth)) ? i.truth[p] : i.default_truth
end
Base.in(p::Proposition{A}, i::DefaultedTruthDict{AA}) where {AA,A<:AA} = true
