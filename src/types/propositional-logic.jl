"""Placeholder to indicate a vector of propositional logical operators, i.e. ¬, ∧, ∨, →."""
const BASE_PROPOSITIONAL_CONNECTIVES = BASE_CONNECTIVES

"""Types associated with propositional logical operators."""
const BasePropositionalConnectives = Union{typeof.(BASE_PROPOSITIONAL_CONNECTIVES)...}

"""A propositional logic based on the base propositional operators."""
const BasePropositionalLogic = AbstractLogic{G,A} where {
        ALP,
        G<:AbstractGrammar{ALP,<:BasePropositionalConnectives},
        A<:AbstractAlgebra
    }

"""
    propositionallogic(;
        alphabet = AlphabetOfAny{String}(),
        operators = $(BASE_PROPOSITIONAL_CONNECTIVES),
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), $(BASE_PROPOSITIONAL_CONNECTIVES)),
        algebra = BooleanAlgebra()
    )

Instantiate a [propositional logic](https://simple.wikipedia.org/wiki/Propositional_logic)
given a grammar and an algebra. Alternatively, an alphabet and a set of operators
can be specified instead of the grammar.

# Examples
```julia-repl
julia> (¬) isa operatorstype(propositionallogic())
true

julia> (¬) isa operatorstype(propositionallogic(; operators = [∨]))
false

julia> propositionallogic(; alphabet = ["p", "q"]);

julia> propositionallogic(; alphabet = ExplicitAlphabet([Atom("p"), Atom("q")]));

```

See also [`modallogic`](@ref), [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref).
"""
function propositionallogic(;
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:Operator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing
)
    _baselogic(
        alphabet = alphabet,
        operators = operators,
        grammar = grammar,
        algebra = algebra;
        default_operators = BASE_PROPOSITIONAL_CONNECTIVES,
        logictypename = "propositional logic",
    )
end

############################################################################################

"""
    abstract type AbstractAssignment <: AbstractInterpretation end

Abstract type for assigments, that is, interpretations of propositional logic,
encoding mappings from `Atom`s to `Truth` values.

# Interface
- `Base.haskey(i::AbstractAssignment, ::AbstractAtom)::Bool`
- `inlinedisplay(i::AbstractAssignment)`
- `interpret(a::AbstractAtom, i::AbstractAssignment, args...; kwargs...)::SyntaxLeaf`

See also [`AbstractInterpretation`](@ref).
"""
abstract type AbstractAssignment <: AbstractInterpretation end

"""
    Base.haskey(i::AbstractAssignment, ::AbstractAtom)::Bool

    Base.haskey(i::AbstractAssignment, v)::Bool

Return whether an assigment has a truth value for a given atom.
If any object is passed, it is wrapped in an atom and then checked.

# Examples

```julia-repl
julia> haskey(TruthDict(["a" => true, "b" => false, "c" => true]), Atom("a"))
true

julia> haskey(TruthDict(1:4, false), 3)
true

julia> haskey(TruthDict(1:4, false), 8)
false
```

See also [`AbstractInterpretation`](@ref), [`AbstractAtom`](@ref), [`TruthDict`](@ref).
"""
function Base.haskey(i::AbstractAssignment, ::AbstractAtom)::Bool
    return error("Please, provide method Base.haskey(::$(typeof(i)), ::AbstractAtom)::Bool.")
end

function Base.haskey(i::AbstractAssignment, v)::Bool
    Base.haskey(i, Atom(v))
end

"""
    inlinedisplay(i::AbstractAssignment)

Provides a string representation of an AbstractAssignment.

# Examples

```julia-repl
julia> SoleLogics.inlinedisplay(TruthDict(["a" => true, "b" => false, "c" => true]))
"TruthDict([c => ⊤, b => ⊥, a => ⊤])"

julia> SoleLogics.inlinedisplay(TruthDict(1:4, false))
"TruthDict([4 => ⊥, 2 => ⊥, 3 => ⊥, 1 => ⊥])"
```

See also [`TruthDict`](@ref).
"""
function inlinedisplay(i::AbstractAssignment)
    return error("Please, provide method inlinedisplay(::$(typeof(i)))::String.")
end

# When interpreting a single atom, if the lookup fails, then return the atom itself
function interpret(a::AbstractAtom, i::AbstractAssignment, args...; kwargs...)::SyntaxLeaf
    if Base.haskey(i, a)
        Base.getindex(i, a, args...; kwargs...)
    else
        a
    end
end

# # TODO remove repetition!!!
# function interpret(
#     φ::SyntaxBranch,
#     i::AbstractAssignment,
#     args...;
#     kwargs...,
# )
#     connective = token(φ)
#     return simplify(connective, Tuple(
#         [interpret(ch, i, args...; kwargs...) for ch in children(φ)]
#     ), args...; kwargs...)
# end

