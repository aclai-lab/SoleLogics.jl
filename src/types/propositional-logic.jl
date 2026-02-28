"""
    const BASE_PROPOSITIONAL_CONNECTIVES = BASE_CONNECTIVES

Vector of propositional logical operators, i.e. ¬, ∧, ∨, →.

See also [`BASE_CONNECTIVES`](@ref).
"""
const BASE_PROPOSITIONAL_CONNECTIVES = BASE_CONNECTIVES

"""
    const BasePropositionalConnectives = Union{typeof.(BASE_PROPOSITIONAL_CONNECTIVES)...}

Types associated with propositional logical operators.

See also [`BASE_PROPOSITIONAL_CONNECTIVES`](@ref), [`BASE_CONNECTIVES`](@ref).
"""
const BasePropositionalConnectives = Union{typeof.(BASE_PROPOSITIONAL_CONNECTIVES)...}


"""
    const BasePropositionalLogic = AbstractLogic{G,A}

A propositional logic based on the base propositional operators.

See also [`AbstractLogic`](@ref).
"""
const BasePropositionalLogic = AbstractLogic{G,A} where {
        ALP,
        G<:AbstractGrammar{ALP,<:BasePropositionalConnectives},
        A<:AbstractAlgebra
    }

############################################################################################

"""
    abstract type AbstractAssignment <: Interpretation end

Abstract type for assigments, that is, interpretations of propositional logic,
encoding mappings from [`AbstractAtom`](@ref)s to `Truth` values.

# Interface
- `Base.haskey(i::AbstractAssignment, ::AbstractAtom)::Bool`
- `inlinedisplay(i::AbstractAssignment)::String`
- `interpret(a::AbstractAtom, i::AbstractAssignment, args...; kwargs...)::SyntaxLeaf`

See also [`AbstractAssignment`](@ref), [`AbstractAtom`](@ref), [`Interpretation`](@ref).
"""
abstract type AbstractAssignment <: Interpretation end

"""
    Base.haskey(i::AbstractAssignment, ::AbstractAtom)::Bool

Return whether an [`AbstractAssignment`](@ref) has a truth value for a given [`Atom`](@ref).
If any object is passed, it is wrapped in an [`Atom`](@ref) and then checked.

# Examples

```julia-repl
julia> haskey(TruthDict(["a" => true, "b" => false, "c" => true]), Atom("a"))
true

julia> haskey(TruthDict(1:4, false), Atom(3))
true
```

See also [`AbstractAssignment`](@ref), [`Interpretation`](@ref), 
[`AbstractAtom`](@ref), [`TruthDict`](@ref), [`Atom`](@ref).
"""
function Base.haskey(i::AbstractAssignment, ::AbstractAtom)::Bool
    return error("Please, provide method Base.haskey(::$(typeof(i)), ::AbstractAtom)::Bool.")
end

"""
    inlinedisplay(i::AbstractAssignment)

Provides a string representation of an assignment.

# Examples

```julia-repl
julia> SoleLogics.inlinedisplay(TruthDict(["a" => true, "b" => false, "c" => true]))
"TruthDict([c => ⊤, b => ⊥, a => ⊤])"

julia> SoleLogics.inlinedisplay(TruthDict(1:4, false))
"TruthDict([4 => ⊥, 2 => ⊥, 3 => ⊥, 1 => ⊥])"
```

See also [`AbstractAssignment`](@ref), [`TruthDict`](@ref).
"""
function inlinedisplay(i::AbstractAssignment)
    return error("Please, provide method inlinedisplay(::$(typeof(i)))::String.")
end

"""
    interpret(a::AbstractAtom, i::AbstractAssignment, args...; kwargs...)::SyntaxLeaf

Return the value corresponding to the [`Atom`](@ref) contained in the 
[`AbstractAssignment`](@ref). When interpreting a single atom, if the lookup fails, 
then return the atom itself.

# Examples

```julia-repl
julia>interpret(Atom("a"), TruthDict(["a" => true, "b" => false, "c" => true]))
⊤

julia>interpret(Atom(3), TruthDict(1:4, false))
⊥
```

See also [`AbstractAssignment`](@ref), [`Atom`](@ref), 
[`DefaultedTruthDict`](@ref), [`TruthDict`](@ref).
"""
function interpret(a::AbstractAtom, i::AbstractAssignment, args...; kwargs...)::SyntaxLeaf
    if Base.haskey(i, a)
        Base.getindex(i, a, args...; kwargs...)
    else
        a
    end
end

# TODO: do test and look if working
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

