#=
    Syntactical Type Hierarchy

    Syntactical
    ├── Formula
    │   ├── SyntaxStructure
    │   │   ├── SyntaxTree
    │   │   │   ├── SyntaxLeaf
    │   │   │   │   ├── AbstractAtom
    │   │   │   │   │   └── Atom (e.g., p)
    │   │   │   │   └── Truth
    │   │   │   │       ├── BooleanTruth (⊤ and ⊥)
    │   │   │   │       └── ...
    │   │   │   └── AbstractSyntaxBranch
    │   │   │   │   └── SyntaxBranch (e.g., p ∧ q)
    │   │   ├── LinearForm
    │   │   │   └── LeftmostLinearForm (e.g., conjunctions, disjunctions, DNFs, CNFs)
    │   │   ├── Literal (e.g., p, ¬p)
    │   │   └── ...
    │   ├── TruthTable
    │   └── ...
    └── Connective
        ├── NamedConnective (e.g., ∧, ∨, →, ¬, □, ◊)
        ├── AbstractRelationalConnective
        │   ├── DiamondRelationalConnective (e.g., ⟨G⟩)
        │   ├── BoxRelationalConnective (e.g., [G])
        │   └── ...
        └── ...
=#

############################################################################################
#### Atom ##################################################################################
############################################################################################

"""
    struct Atom{V} <: AbstractAtom
        value::V
    end

Simplest atom implementation, wrapping a `value`.

See also [`AbstractAtom`](@ref), [`value`](@ref), [`check`](@ref),
[`SyntaxToken`](@ref).
"""
struct Atom{V} <: AbstractAtom
    value::V

    function Atom{V}(value::V) where {V}
        if value isa Union{Formula, Connective}
            throw(ArgumentError("Illegal nesting. " *
                  "Cannot instantiate Atom with value of type $(typeof(value))"
            ))
        end
        new{V}(value)
    end
    function Atom(value::V) where {V}
        Atom{V}(value)
    end
    function Atom{V}(p::Atom) where {V}
        Atom{V}(value(p))
    end
    function Atom(p::Atom)
        p
    end
end

valuetype(::Atom{V}) where {V} = V
valuetype(::Type{Atom{V}}) where {V} = V

value(p::Atom) = p.value

dual(p::Atom) = Atom(dual(value(p)))
hasdual(p::Atom) = hasdual(value(p))
hasdual(value) = false
dual(value) = error("Please, provide method SoleLogics.dual(::$(typeof(value))).") # TODO explain why?

Base.convert(::Type{A}, p::Atom) where {A <: Atom} = A(p)
Base.convert(::Type{A}, a) where {A <: Atom} = A(a)

Base.isequal(a::Atom, b::Atom) = Base.isequal(value(a), value(b)) # Needed to avoid infinite recursion
Base.isequal(a::Atom, b) = Base.isequal(value(a), b)
Base.isequal(a, b::Atom) = Base.isequal(a, value(b))
Base.isequal(a::Atom, b::SyntaxTree) = (a == b) # Needed for resolving ambiguities
Base.isequal(a::SyntaxTree, b::Atom) = (a == b) # Needed for resolving ambiguities
Base.hash(a::Atom) = Base.hash(value(a))

syntaxstring(a::Atom; kwargs...)::String = syntaxstring(value(a); kwargs...)

syntaxstring(value; kwargs...) = string(value)

############################################################################################
#### SyntaxBranch ##########################################################################
############################################################################################

"""
    struct SyntaxBranch <: AbstractSyntaxBranch
        token::Connective
        children::NTuple{N,SyntaxTree} where {N}
    end

Simple implementation of a syntax branch. The
implementation is *arity-compliant*, in that, upon construction,
the arity of the token is checked against the number of children provided.

# Examples
```julia-repl
julia> p,q = Atom.([p, q])
2-element Vector{Atom{String}}:
 Atom{String}: p
 Atom{String}: q

julia> branch = SyntaxBranch(CONJUNCTION, p, q)
SyntaxBranch: p ∧ q

julia> token(branch)
∧

julia> syntaxstring.(children(branch))
(p, q)

julia> ntokens(a) == nconnectives(a) + nleaves(a)
true

julia> arity(a)
2

julia> height(a)
1
```

See also
[`token`](@ref), [`children`](@ref),
[`arity`](@ref),
[`Connective`](@ref),
[`height`](@ref),
[`atoms`](@ref), [`natoms`](@ref),
[`operators`](@ref), [`noperators`](@ref),
[`tokens`](@ref), [`ntokens`](@ref),
"""
struct SyntaxBranch <: AbstractSyntaxBranch

    # The syntax token at the current node
    token::Connective

    # The child nodes of the current node
    children::NTuple{N, SyntaxTree} where {N}

    function _aritycheck(N, token, children)
        if arity(token) != N
            throw(
                "Cannot instantiate SyntaxBranch with token " *
                "$(token) of arity $(arity(token)) and $(N) children."
            )
        end
        return nothing
    end

    function SyntaxBranch(
            token::Connective,
            children::NTuple{N, SyntaxTree} = (),
    ) where {N}
        _aritycheck(N, token, children)
        return new(token, children)
    end

    # Helpers
    function SyntaxBranch(token::Connective, children...)
        return SyntaxBranch(token, children)
    end
end

children(φ::SyntaxBranch) = φ.children
token(φ::SyntaxBranch) = φ.token

################################################################################
################################################################################

"""
    collatetruth(c::Connective, ts::NTuple{N,T where T<:Truth}, args...)::Truth where {N}

Return the truth value for a composed formula `c(t1, ..., tN)`, given the `N`
with t1, ..., tN being `Truth` values.

See also [`simplify`](@ref), [`Connective`](@ref), [`Truth`](@ref).
"""
function collatetruth(
    c::Connective,
    ts::NTuple{N,Truth},
    args...
)::Truth where {N}
    if arity(c) != length(ts)
        return error("Cannot collate $(length(ts)) truth values for " *
                     "connective $(typeof(c)) with arity $(arity(c))).")
    else
        return error("Please, provide method collatetruth(::$(typeof(c)), " *
                     "::NTuple{$(arity(c)),$(eltype(ts))}).")
    end
end

# Helper (so that collatetruth work for all operators)
collatetruth(t::Truth, ::Tuple{}, args...) = t

# With generic formulas, it composes formula
"""
    simplify(c::Connective, ts::NTuple{N,F where F<:Formula} args...)::Truth where {N}

Return a formula with the same semantics of a composed formula `c(φ1, ..., φN)`,
given the `N`
immediate sub-formulas.

See also [`collatetruth`](@ref), [`Connective`](@ref), [`Formula`](@ref).
"""
function simplify(c::Connective, φs::NTuple{N,Formula}, args...; kwargs...) where {N}
    c(φs, args...; kwargs...)
end

function simplify(c::Connective, φs::NTuple{N,T}, args...; kwargs...) where {N,T<:Truth}
    collatetruth(c, φs, args...; kwargs...)
end

############################################################################################
##################################### BASE CONNECTIVES #####################################
############################################################################################

"""
    struct NamedConnective{Symbol} <: Connective end

A singleton type for representing connectives defined by a name or a symbol.

# Examples
The AND connective (i.e., the logical conjunction) is defined as the subtype:

    const CONJUNCTION = NamedConnective{:∧}()
    const ∧ = CONJUNCTION
    arity(::typeof(∧)) = 2

See also [`NEGATION`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref),
[`IMPLICATION`](@ref), [`Connective`](@ref).
"""
struct NamedConnective{Symbol} <: Connective end

name(::NamedConnective{S}) where {S} = S

Base.show(io::IO, c::NamedConnective) = print(io, "$(syntaxstring(c))")

syntaxstring(c::NamedConnective; kwargs...) = string(name(c))

function precedence(c::NamedConnective)
    op = SoleLogics.name(c)
    # Using default Base.operator_precedence is risky. For example,
    # Base.isoperator(:(¬)) is true, but Base.operator_precedence(:(¬)) is 0.
    # See Base.operator_precedence documentation.
    if !Base.isoperator(op) || Base.operator_precedence(op) == 0
        error("Please, provide method SoleLogics.precedence(::$(typeof(c))).")
    else
        Base.operator_precedence(op)
    end
end

function associativity(c::NamedConnective)
    op = SoleLogics.name(c)
    # Base.isoperator(:(++)) is true, but Base.operator_precedence(:(++)) is :none
    if !Base.isoperator(op) || !(Base.operator_associativity(op) in [:left, :right])
        error("Please, provide method SoleLogics.associativity(::$(typeof(c))).")
    else
        Base.operator_associativity(op)
    end
end
