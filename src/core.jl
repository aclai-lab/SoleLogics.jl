import Base: convert, promote_rule, _promote
import Base: eltype, in, getindex, isiterable, iterate, IteratorSize, length, isequal, hash

#=
    New syntactical type hierarchy

    Syntactical
    ├── Formula
    │   ├── AbstractSyntaxStructure
    │   │   ├── SyntaxTree
    │   │   │   ├── SyntaxLeaf
    │   │   │   │   ├── Atom
    │   │   │   │   └── Truth
    │   │   │   │       ├── BooleanTruth
    │   │   │   │       │   ├── Top
    │   │   │   │       │   └── Bot
    │   │   │   │       └── ...
    │   │   │   └── SyntaxBranch
    │   │   ├── LeftmostLinearForm
    │   │   └── ...
    │   └── AbstractMemoFormula
    │       └── TruthTable
    └── Connective
        ├── NamedConnective
        ├── AbstractRelationalOperator
        ├── DiamondRelationalOperator
        ├── BoxRelationalOperator
        └── ...

    Also:
    const Operator = Union{Connective,Truth}
    const SyntaxToken = Union{Connective,SyntaxLeaf}
=#

"""
    abstract type Syntactical end;

Master abstract type of all types related to syntax.
"""
abstract type Syntactical end

"""
    abstract type Formula end;

A generic construct devoted to represent and organize a syntactical structure with specific
properties. Examples of `Formula`s are `SyntaxLeaf`s (for example, `Atom`s and
`Truth` values), `AbstractSyntaxStructure`s (for example, `SyntaxTree`s and
`LeftmostLinearForm`s) and `AbstractMemoFormula`s (for example, `TruthTable`s).

See also [`AbstractSyntaxStructure`](@ref), [`SyntaxLeaf`](@ref),
[`AbstractMemoFormula`](@ref).
"""
abstract type Formula <: Syntactical end

"""
    abstract type AbstractSyntaxStructure <: Formula end

A logical formula, represented by its syntactic component.
The typical representation is the [`SyntaxBranch`](@ref);
however, different implementations can cover specific synctactic forms
(e.g., conjuctive/disjuctive normal forms).

See also [`Formula`](@ref), [`AbstractLogic`](@ref), [`SyntaxBranch`](@ref),
[`tree`](@ref).
"""
abstract type AbstractSyntaxStructure <: Formula end

"""
    abstract type SyntaxTree <: AbstractSyntaxStructure end

Generic syntax tree, which essentially consist in a composition of two syntactical elements:
branches (wrapping `Connective`s) and leafs (`Truth` values and `Atom`s).

See also [`SyntaxBranch`](@ref), [`SyntaxLeaf`](@ref).
"""
abstract type SyntaxTree <: AbstractSyntaxStructure end

"""
    abstract type SyntaxLeaf <: AbstractSyntaxStructure end;

Syntactic component that can be an `AbstractSyntaxStructure`'s leaf. The particularity of
`SyntaxLeaf` subtypes is that their `arity` is always 0, meaning that they are not
allowed to have children in tree-like syntactical structures.

See also [`AbstractSyntaxStructure`](@ref),  [`arity`](@ref), [`SyntaxBranch`](@ref).
"""
abstract type SyntaxLeaf <: SyntaxTree end

"""
    abstract type AbstractMemoFormula <: Formula end

Enriched representation of an `AbstractSyntaxStructure`. This is useful to display more
informations related to a certain syntactical structure, or save computational time by
exploiting [memoization](https://en.wikipedia.org/wiki/Memoization).
"""
abstract type AbstractMemoFormula <: Formula end

"""
    abstract type Truth <: SyntaxLeaf end

Syntactical, manipulable representation of a truth value.

# Implementation

TODO: when implementing a new custom `Truth` subtype..., provide istop, isbot

See also [`Top`](@ref), [`Bot`](@ref), [`BooleanTruth`](@ref), [`arity`](@ref);
"""
abstract type Truth <: SyntaxLeaf end

"""
    abstract type Connective <: Syntactical end

A connective is a [logical constant](https://en.wikipedia.org/wiki/Logical_connective)
which establishes a relation between `SyntaxLeaf`s (i.e., facts).
For example, the boolean connectives AND, OR and IMPLIES (stylized as ∧, ∨ and →)
are used to connect `SyntaxLeaf`s and/or formulas to express derived concepts.

# Implementation

When implementing a new custom connective, one can override the default `precedence` and
`associativity` (see https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity).
If the custom connective is a `NamedConnective` and renders as something considered as a
`math symbol` (for example, `⊙`, see https://stackoverflow.com/a/60321302/5646732),
by the Julia parser, `Base.operator_precedence`
and `Base.operator_associativity` are used to define these behaviors, and
you might want to avoid providing these methods at all.

When implementing a new type `C` for a *commutative* connective with arity higher than 1,
please provide a method `iscommutative(::C)`. This can help model checking operations.

See also [`SyntaxLeaf`](@ref), [`associativity`](@ref), [`check`](@ref),
[`iscommutative`](@ref), [`NamedConnective`](@ref), [`precedence`](@ref),
[`Syntactical`](@ref).
"""
abstract type Connective <: Syntactical end

"""
    const Operator = Union{Connective,Truth}

Logical constants which establishes relations between facts, or are truth values itself.

See also [`Connective`](@ref), [`Truth`](@ref).
"""
const Operator = Union{Connective,Truth}

"""
    const SyntaxToken = Union{Connective,SyntaxLeaf}

Every type from which an `SyntaxTree` can be composed.

!!! Warning
    NOTE: @typeHierarchyUpdate SyntaxToken type might disappear soon (this is a wip).

See also [`SyntaxTree`](@ref), [`SyntaxLeaf`](@ref), [`Connective`](@ref).
"""
const SyntaxToken = Union{Connective,SyntaxLeaf}

#########################

"""
    arity(tok::Connective)::Integer
    arity(t::SyntaxLeaf)::Integer

Return the `arity` of a `Connective` or an `SyntaxLeaf`. The `arity` is an integer
representing the number of allowed children in a `SyntaxBranch`. `Connective`s with `arity`
equal to 0, 1 or 2 are called `nullary`, `unary` and `binary`, respectively.
`SyntaxLeaf`s (`Atom`s and `Truth` values) are always nullary.

See also [`SyntaxLeaf`](@ref), [`Connective`](@ref), [`SyntaxBranch`](@ref).
"""
arity(t::Connective)::Integer = error("Please, provide method arity(::$(typeof(t))).")
arity(t::SyntaxLeaf)::Integer = 0;

isnullary(a) = arity(a) == 0
isunary(a) = arity(a) == 1
isbinary(a) = arity(a) == 2

"""
    dual(op::SyntaxToken)

Return the `dual` of an `Operator`.
Given an operator `op` of arity `n`, the dual `dop` is such that, on a boolean algebra,
`op(ch_1, ..., ch_n)` ≡ `¬dop(¬ch_1, ..., ¬ch_n)`.

Duality can be used to perform synctactic simplifications on formulas.
For example, since `∧` and `∨` are `dual`s, `¬(¬p ∧ ¬q)` can be simplified to `(p ∧ q)`.
Duality also applies to `Truth` values (`⊤`/`⊥`), with existential/universal
semantics (`◊`/`□`), and `Atom`s.

NOTE: @typeHierarchyUpdate SyntaxToken type:
    keep it or substitute it by writing explicitly Union{Connective,Truth}?

# Implementation

When providing a `dual` for an operator of type `O`, please also provide:

    hasdual(::O) = true

The dual of an `Atom` (that is, the atom with inverted semantics)
is defined as:

    dual(p::Atom{A}) where {A} = Atom(dual(value(p)))

As such, `hasdual(::A)` and `dual(::A)` should be defined when wrapping objects of type `A`.

See also [`normalize`](@ref), [`SyntaxToken`](@ref).
"""
dual(t::SyntaxToken) = error("Please, provide method dual(::$(typeof(t))).")
hasdual(t::SyntaxToken) = false

"""
    syntaxstring(φ::Formula; kwargs...)::String
    syntaxstring(tok::SyntaxToken; kwargs...)::String

Produce the string representation of an `Formula` or a `SyntaxToken` by performing
a tree traversal of the syntax tree representation of the formula.
Note that this representation may introduce redundant parentheses.
`kwargs` can be used to specify how to display syntax tokens/trees under
some specific conditions.

The following `kwargs` are currently supported:
- `function_notation = false::Bool`: when set to `true`, it forces the use of
   function notation for binary operators
   (see [here](https://en.wikipedia.org/wiki/Infix_notation)).
- `remove_redundant_parentheses = true::Bool`: when set to `false`, it prints a syntaxstring
   where each syntactical element is wrapped in parentheses.
- `parenthesize_atoms = !remove_redundant_parentheses::Bool`: when set to `true`,
   it forces the atoms (which are the leaves of a formula's tree structure) to be
   wrapped in parentheses.

# Examples
```jldoctest
julia> syntaxstring(parsetree("p∧q∧r∧s∧t"))
"p ∧ q ∧ r ∧ s ∧ t"

julia> syntaxstring(parsetree("p∧q∧r∧s∧t"), function_notation=true)
"∧(∧(∧(∧(p, q), r), s), t)"

julia> syntaxstring(parsetree("p∧q∧r∧s∧t"), remove_redundant_parentheses=false)
"((((p) ∧ (q)) ∧ (r)) ∧ (s)) ∧ (t)""

julia> syntaxstring(parsetree("p∧q∧r∧s∧t"), remove_redundant_parentheses=true, parenthesize_atoms=true)
"(p) ∧ (q) ∧ (r) ∧ (s) ∧ (t)"

julia> syntaxstring(parsetree("◊((p∧s)→q)"))
"◊((p ∧ s) → q)"

julia> syntaxstring(parsetree("◊((p∧s)→q)"); function_notation = true)
"◊(→(∧(p, s), q))"
```

See also [`parsetree`](@ref), [`parsetree`](@ref),
[`SyntaxBranch`](@ref), [`SyntaxToken`](@ref).

# Implementation

In the case of a syntax tree, `syntaxstring` is a recursive function that calls
itself on the syntax children of each node. For a correct functioning, the `syntaxstring`
must be defined (including `kwargs...`) for every newly defined
`SyntaxToken` (e.g., `SyntaxLeaf`s, that is, `Atom`s and `Truth` values, and `Operator`s),
in a way that it produces a
*unique* string representation, since `Base.hash` and `Base.isequal`, at least for
`SyntaxBranch`s, rely on it.

In particular, for the case of `Atom`s, the function calls itself on the wrapped value:

    syntaxstring(p::Atom; kwargs...) = syntaxstring(value(p); kwargs...)

The `syntaxstring` for any value defaults to its `string` representation, but it can be
defined by defining the appropriate `syntaxstring` method.

!!! warning
    The `syntaxstring` for syntax tokens (e.g., atoms, operators) should not be
    prefixed/suffixed by whitespaces, as this may cause ambiguities upon *parsing*.
    For similar reasons, `syntaxstring`s should not contain parentheses (`'('`, `')'`),
    and, when parsing in function notation, commas (`','`).

See also [`SyntaxLeaf`](@ref), [`Operator`](@ref), [`parsetree`](@ref).
"""
function syntaxstring(tok::Syntactical; kwargs...)::String
    return error("Please, provide method syntaxstring(::$(typeof(tok)); kwargs...).")
end

syntaxstring(value; kwargs...) = string(value)
# syntaxstring(value::Union{AbstractString,Number,AbstractChar}; kwargs...) = string(value)

############################################################################################
############################################################################################
############################################################################################

"""
    struct Atom{A} <: SyntaxLeaf
        value::A
    end

An atom, sometimes called an atomic proposition,
propositional letter (or simply *letter*), of type
`Atom{A}` wraps a `value::A` representing a fact which truth can be assessed on
a logical interpretation.

Atoms are nullary tokens (i.e, they are at the leaves of a syntax tree);
note that their atoms cannot be `Atom`s.

See also [`AbstractInterpretation`](@ref), [`check`](@ref), [`SyntaxToken`](@ref).
"""
struct Atom{A} <: SyntaxLeaf
    value::A

    function Atom{A}(value::A) where {A}
        @assert !(value isa Union{Formula,Connective}) "Illegal nesting. " *
            "Cannot instantiate Atom with value of type $(typeof(value))"
        new{A}(value)
    end
    function Atom(value::A) where {A}
        Atom{A}(value)
    end
    function Atom{A}(p::Atom) where {A}
        Atom{A}(value(p))
    end
    function Atom(p::Atom)
        p
    end
end

value(p::Atom) = p.value

arity(::Atom) = 0

valuetype(::Atom{A}) where {A} = A
valuetype(::Type{Atom{A}}) where {A} = A

# Helpers
Base.convert(::Type{P}, p::Atom) where {P<:Atom} = P(p)
Base.convert(::Type{P}, a) where {P<:Atom} = P(a)
# Base.promote_rule(::Type{Union{AbstractString,Number,AbstractChar}}, ::Type{<:Atom}) = Atom
# Base.promote_rule(::Type{<:Atom}, ::Type{Union{AbstractString,Number,AbstractChar}}) = Atom

syntaxstring(p::Atom; kwargs...) = syntaxstring(value(p); kwargs...)

Base.isequal(a::Atom, b::Atom) = Base.isequal(value(a), value(b))
Base.isequal(a::Atom, b) = Base.isequal(value(a), b)
Base.isequal(a, b::Atom) = Base.isequal(a, value(b))
Base.hash(a::Atom) = Base.hash(value(a))

dual(p::Atom) = Atom(dual(value(p)))

function dual(value)
    return error("Please, provide method " *
        "SoleLogics.dual(::$(typeof(value))).")
end

############################################################################################

doc_iscommutative = """
    iscommutative(c::Connective)

Return whether a connective is known to be commutative.

# Examples
```jldoctest
julia> iscommutative(∧)
true

julia> iscommutative(→)
false
```

Note that nullary and unary connectives are considered commutative.

See also [`Connective`](@ref).

# Implementation

When implementing a new type for a *commutative* connective `C` with arity higher than 1,
please provide a method `iscommutative(::C)`. This can help model checking operations.

See also [`Connective`](@ref).
"""

"""$(doc_iscommutative)"""
function iscommutative(c::Connective)
    return arity(c) <= 1
end

"""
    precedence(c::Connective)

Return the precedence of a (binary) connective.

When using infix notation, and in the absence of parentheses,
`precedence` establishes how binary connectives are interpreted.
A precedence value is a standard integer, and
connectives with high precedence take precedence over connectives with lower precedences.
This affects how formulas are shown (via `syntaxstring`) and parsed (via `parsetree`).

By default, the value for a `NamedConnective` is derived from the `Base.operator_precedence`
of its symbol (`name`).
Because of this, when dealing with a custom connective `⊙`,
it will be the case that `parsetree("p ⊙ q ∧ r") == (@synexpr p ⊙ q ∧ r)`.

It is possible to assign a specific precedence to a connective type `C` by providing a method
`Base.operator_precedence(::C)`.

# Examples
```jldoctest
julia> precedence(∧) == Base.operator_precedence(:∧)
true

julia> precedence(¬) == Base.operator_precedence(:¬)
true

julia> precedence(∧), precedence(∨), precedence(→)
∨(12, 11, 4)

julia> syntaxstring(parseformula("¬a ∧ b ∧ c"))
"¬a ∧ b ∧ c"

julia> syntaxstring(parseformula("¬a → b ∧ c"))
"(¬a) → (b ∧ c)"

julia> syntaxstring(parseformula("a ∧ b → c ∧ d"))
"(a ∧ b) → (c ∧ d)"
```

See also [`associativity`](@ref), [`Connective`](@ref).
"""
function precedence(c::Connective)
    return error("Please, provide method precedence(c::$(typeof(c))).")
end

"""
    associativity(::Connective)

Return whether a (binary) connective is right-associative.

When using infix notation, and in the absence of parentheses,
`associativity establishes how binary connectives of the same `precedence`
are interpreted. This affects how formulas are
shown (via `syntaxstring`) and parsed (via `parsetree`).

By default, the value for a `NamedConnective` is derived from the `Base.operator_precedence`
of its symbol (`name`); thus, for example, most connectives are left-associative
(e.g., `∧` and `∨`), while `→` is right-associative.
Because of this, when dealing with a custom connective `⊙`,
it will be the case that `parsetree("p ⊙ q ∧ r") == (@synexpr p ⊙ q ∧ r)`.

# Examples
```jldoctest
julia> associativity(∧)
:left

julia> associativity(→)
:right

julia> syntaxstring(parsetree("p → q → r"); remove_redundant_parentheses = false)
"p → (q → r)"

julia> syntaxstring(parsetree("p ∧ q ∨ r"); remove_redundant_parentheses = false)
"(p ∧ q) ∨ r"
```

See also [`Connective`](@ref), [`parsetree`](@ref), [`precedence`](@ref),
[`syntaxstring`](@ref).
"""
associativity(c::Connective) = :left

############################################################################################

"""
    joinformulas(
        c::Connective,
        ::NTuple{N,F}
    )::F where {N,F<:Formula}

Return a new formula of type `F` by composing `N` formulas of the same type
via a connective `c`. This function allows one to use connectives for flexibly composing
formulas (see *Implementation*).

# Examples
```jldoctest
julia> f = parseformula("◊(p→q)");

julia> p = Atom("p");

julia> ∧(f, p)  # Easy way to compose a formula
SyntaxBranch: ◊(p → q) ∧ p

julia> f ∧ ¬p   # Leverage infix notation ;) See https://stackoverflow.com/a/60321302/5646732
SyntaxBranch: ◊(p → q) ∧ ¬p

julia> ∧(f, p, ¬p) # Shortcut for ∧(f, ∧(p, ¬p))
SyntaxBranch: ◊(p → q) ∧ p ∧ ¬p
```

# Implementation

Upon `joinformulas` lies a flexible way of using connectives for composing
formulas and syntax tokens (e.g., atoms), given by methods like the following:

    function (c::Connective)(
        children::NTuple{N,Formula},
    ) where {N}
        ...
    end

These allow composing formulas as in `∧(f, ¬p)`, and in order to access this composition
with any newly defined subtype of `Formula`,
a new method for `joinformulas` should be defined, together with
promotion from/to other `Formula`s should be taken care of (see
[here](https://docs.julialang.org/en/v1/manual/conversion-and-promotion/)
and [here](https://github.com/JuliaLang/julia/blob/master/base/promotion.jl)).

Similarly,
for allowing a (possibly newly defined) connective to be applied on a number of
syntax tokens/formulas that differs from its arity,
for any newly defined connective `c`, new methods
similar to the two above should be defined.
For example, although ∧ and ∨ are binary, (i.e., have arity equal to 2),
compositions such as `∧(f, f2, f3, ...)` and `∨(f, f2, f3, ...)` can be done
thanks to the following two methods that were defined in SoleLogics:

    function ∧(
        c1::Union{SyntaxToken,Formula},
        c2::Union{SyntaxToken,Formula},
        c3::Union{SyntaxToken,Formula},
        cs::Union{SyntaxToken,Formula}...
    )
        return ∧(c1, ∧(c2, c3, cs...))
    end
    function ∨(
        c1::Union{SyntaxToken,Formula},
        c2::Union{SyntaxToken,Formula},
        c3::Union{SyntaxToken,Formula},
        cs::Union{SyntaxToken,Formula}...
    )
        return ∨(c1, ∨(c2, c3, cs...))
    end

!!! info
    The idea behind `joinformulas` is to concatenate syntax tokens without applying
    simplifications/minimizations of any kind. Because of that, ∧(⊤,⊤) returns a
    `SyntaxBranch` whose root value is ∧, instead of returning just a Truth value ⊤.

See also [`Formula`](@ref), [`Connective`](@ref).
"""
function joinformulas(c::Connective, ::NTuple{N,F})::F where {N,F<:Formula}
    return error("Please, provide method " *
        "joinformulas(c::Connective, children::NTuple{N,$(F)}) where {N}.")
end

function joinformulas(c::Connective, children::Vararg{F,N})::F where {N,F<:Formula}
    joinformulas(c, children)
end

"""
    Base.in(tok::SyntaxToken, φ::Formula)::Bool

Return whether a syntax token appears in a formula.

See also [`Formula`](@ref), [`SyntaxToken`](@ref).
"""
function Base.in(tok::SyntaxToken, φ::Formula)::Bool
    return (φ isa SyntaxLeaf ? tok == φ : Base.in(tok, tree(φ)))
end

function Base.show(io::IO, φ::Formula)
    print(io, "$(typeof(φ))\nsyntaxstring: $(syntaxstring(φ))")
end

function Base.show(io::IO, t::SyntaxToken)
    print(io, syntaxstring(t))
end

function syntaxstring(φ::Formula; kwargs...)
    syntaxstring(tree(φ); kwargs...)
end


doc_tokopprop = """
    tokens(f::AbstractSyntaxStructure)::AbstractVector{<:SyntaxToken}
    operators(f::AbstractSyntaxStructure)::AbstractVector{<:Operator}
    atoms(f::AbstractSyntaxStructure)::AbstractVector{<:Atom}
    ntokens(f::AbstractSyntaxStructure)::Integer
    noperators(f::AbstractSyntaxStructure)::Integer
    natoms(f::AbstractSyntaxStructure)::Integer

Return the list or the number of (unique) `SyntaxToken`s appearing in a formula.

See also [`AbstractSyntaxStructure`](@ref), [`SyntaxToken`](@ref).
"""

"""$(doc_tokopprop)"""
function tokens(f::AbstractSyntaxStructure)::AbstractVector{<:SyntaxToken}
    return tokens(tree(f))
end
"""$(doc_tokopprop)"""
function operators(f::AbstractSyntaxStructure)::AbstractVector{<:Operator}
    return operators(tree(f))
end
"""$(doc_tokopprop)"""
function atoms(f::AbstractSyntaxStructure)::AbstractVector{<:Atom}
    return atoms(tree(f))
end
"""$(doc_tokopprop)"""
function ntokens(f::AbstractSyntaxStructure)::Integer
    return ntokens(tree(f))
end
"""$(doc_tokopprop)"""
function natoms(f::AbstractSyntaxStructure)::Integer
    return natoms(tree(f))
end
"""$(doc_tokopprop)"""
function height(f::AbstractSyntaxStructure)::Integer
    return height(tree(f))
end

# Helpers that make all Formula's map to the same dictionary key.
# Useful when checking formulas on interpretations.
function Base.isequal(a::AbstractSyntaxStructure, b::AbstractSyntaxStructure)
    Base.isequal(tree(a), tree(b))
end

# Base.hash(a::AbstractSyntaxStructure) = Base.hash(tree(a)) # TODO: fix this, since this definition now gives a StackOverflow

#= NOTE: this could be useful
Base.promote_rule(
    ::Type{SS},
    ::Type{<:SyntaxLeaf}
) where {SS<:AbstractSyntaxStructure} = SS

Base.promote_rule(
    ::Type{<:SyntaxLeaf},
    ::Type{SS}
) where {SS<:AbstractSyntaxStructure} = SS
=#

############################################################################################

"""
    struct SyntaxBranch{T<:Connective} <: SyntaxTree
        token::T
        children::NTuple{N,SyntaxTree} where {N}
    end

A syntax tree encoding a logical formula.
Each internal node of the syntax tree holds a `token`, which is of type `Connective`,
and has as many children as the `arity` of the token.

This implementation is *arity-compliant*, in that, upon construction,
the arity is checked against the number of children provided.

If a node `arity` is 0, then it would hold a special token of type `SyntaxLeaf`: in our
implementation, leafs are `SyntaxLeaf` itself and NOT `SyntaxBranch`s wrapping them.

See also [`arity`](@ref), [`Atom`](@ref), [`atoms`](@ref), [`atomstype`](@ref),
[`Connective`](@ref), [`children`](@ref), [`height`](@ref),[`natoms`](@ref),
[`ntokens`](@ref), [`Operator`](@ref),[`operators`](@ref), [`operatorstype`](@ref),
[`token`](@ref), [`tokens`](@ref), [`tokenstype`](@ref), [`tokentype`](@ref).
"""
struct SyntaxBranch{T<:Connective} <: SyntaxTree

    # The syntax token at the current node
    token::T

    # The child nodes of the current node
    children::NTuple{N,SyntaxTree} where {N}

    function _aritycheck(N, T, token, children)
        @assert arity(token) == N "Cannot instantiate SyntaxBranch{$(T)} with token " *
                                  "$(token) of arity $(arity(token)) and $(N) children."
        return nothing
    end

    function SyntaxBranch{T}(
        token::T,
        children::NTuple{N,SyntaxTree} = (),
    ) where {T<:Connective,N}
        children = convert.(SyntaxBranch, children)

        _aritycheck(N, T, token, children)
        return new{T}(token, children)
    end

    function SyntaxBranch{T}(
        t::SyntaxBranch{T},
    ) where {T<:Connective}
        return SyntaxBranch{T}(token(t), children(t))
    end

    function SyntaxBranch(
        token::T,
        children::NTuple{N,SyntaxTree} = (),
    ) where {T<:Connective,N}
        children = convert.(SyntaxBranch, children)

        _aritycheck(N, T, token, children)
        return new{T}(token, children)
    end

    function SyntaxBranch(t::SyntaxLeaf, args...)
        @assert length(args) == 0 "Leaf $(t) (type $(typeof(t))) is nullary, " *
            " and cannot take syntax children ($(length(args)) were given)."
        return t
    end
end

# Helpers
function SyntaxBranch{T}(token::T, children...) where {T<:Connective}
    return SyntaxBranch{T}(token, children)
end
function SyntaxBranch(token::T, children...) where {T<:Connective}
    return SyntaxBranch(token, children)
end

# Getters
token(t::SyntaxBranch) = t.token
token(t::SyntaxLeaf) = t

children(t::SyntaxBranch) = t.children
children(::SyntaxLeaf) = ()

tokentype(::SyntaxBranch{T}) where {T} = T
tokentype(::T) where {T <: SyntaxLeaf} = T

tokenstype(t::SyntaxBranch) = Union{tokentype(t),tokenstype.(children(t))...}
operatorstype(t::SyntaxBranch) = typeintersect(Operator, tokenstype(t))
atomstype(t::SyntaxBranch) = typeintersect(Atom, tokenstype(t))

function joinformulas(
    op::Connective,
    children::NTuple{N,Union{SyntaxToken,SyntaxBranch}}
) where {N}
    return SyntaxBranch(op, children)
end

# Shows the type of the syntax tree and its syntaxstring.
# Base.show(io::IO, t::SyntaxBranch) = print(io, "$(typeof(t))($(syntaxstring(t)))")
function Base.show(io::IO, t::SyntaxBranch)
    print(io, "$(syntaxstring(t))")
    # print(io, "Allowed token types: $(tokenstype(t))")
end


"""
    Base.in(tok::SyntaxToken, tree::SyntaxBranch)::Bool

Return whether a `SyntaxToken` appears in a syntax tree or not.

See also [`SyntaxToken`](@ref), [`SyntaxBranch`](@ref), [`tokens`](@ref).
"""
function Base.in(tok::SyntaxToken, tree::SyntaxBranch)
    return tok == token(tree) || any([Base.in(tok, c) for c in children(tree)])
end

"""
    tokens(t::SyntaxBranch)::AbstractVector{SyntaxToken}

List all tokens appearing in a syntax tree.

See also [`atoms`](@ref), [`ntokens`](@ref), [`operators`](@ref), [`SyntaxToken`](@ref).
"""
function tokens(t::SyntaxBranch)::AbstractVector{SyntaxToken}
    return SyntaxToken[vcat(tokens.(children(t))...)..., token(t)]
end
tokens(t::SyntaxLeaf) = t

"""
    connectives(t::SyntaxTree)::AbstractVector{Connective}

List all connectives appearing in a syntax tree.

See also [`atoms`](@ref), [`Connective`](@ref), [`nconnectives`](@ref).
"""
function connectives(t::SyntaxTree)::AbstractVector{Connective}
    error("Unexpected syntax tree (type = $(typeof(t)))")
end

function connectives(t::SyntaxBranch)::AbstractVector{Connective}
    c = token(t) isa Connective ? [token(t)] : []
    return Connective[vcat(connectives.(children(t))...)..., c...]
end
connectives(t::SyntaxLeaf) = []

"""
    leaves(t::SyntaxTree)::AbstractVector{Operator}

List all leaves appearing in a syntax tree.

See also  [`atoms`](@ref), [`nleaves`](@ref), [`SyntaxLeaf`](@ref),.
"""
function leaves(t::SyntaxTree)::AbstractVector{SyntaxLeaf}
    error("Unexpected syntax tree (type = $(typeof(t)))")
end

function leaves(t::SyntaxBranch)::AbstractVector{SyntaxLeaf}
    return SyntaxLeaf[vcat(leaves.(children(t))...)...]
end
leaves(l::SyntaxLeaf) = l

"""
    operators(t::SyntaxTree)::AbstractVector{Operator}

List all operators appearing in a syntax tree.

See also [`atoms`](@ref), [`noperators`](@ref), [`Operator`](@ref), [`tokens`](@ref).
"""
function operators(t::SyntaxTree)::AbstractVector{Operator}
    error("Unexpected syntax tree (type = $(typeof(t)))")
end

function operators(t::SyntaxBranch)::AbstractVector{Operator}
    c = token(t) isa Operator ? [token(t)] : []
    return Operator[vcat(operators.(children(t))...)..., c...]
end
operators(t::Truth) = Operator[t]
operators(::Atom) = []

"""
    atoms(t::SyntaxTree)::AbstractVector{Atom}

List all `Atom`s appearing in a syntax tree.

See also [`Atom`](@ref), [`natoms`](@ref), [`operators`](@ref), [`tokens`](@ref).
"""
function atoms(t::SyntaxTree)::AbstractVector{Atom}
    error("Unexpected syntax tree (type = $(typeof(t)))")
end

function atoms(t::SyntaxBranch)
    return Atom[vcat(atoms.(children(t))...)...] |> unique
end
atoms(t::Truth) = Atom[]
atoms(t::Atom) = [t]

"""
    ntokens(t::SyntaxBranch)::Integer

Return the count of all tokens appearing in a syntax tree.

See also [`SyntaxToken`](@ref), [`tokens`](@ref).
"""
function ntokens(t::SyntaxBranch)::Integer
    length(children(t)) == 0 ? 1 : 1 + sum(ntokens(c) for c in children(t))
end

# Helpers. TODO Remove?
ntokens(::SyntaxLeaf) = 1

"""
    noperators(t::SyntaxBranch)::Integer
    noperators(t::SyntaxLeaf)::Integer

Return the count of all `Operator`s appearing in a syntax tree.

See also [`operators`](@ref), [`SyntaxToken`](@ref).
"""
function noperators(t::SyntaxBranch)::Integer
    op = token(t) isa Operator ? 1 : 0
    return length(children(t)) == 0 ? op : op + sum(noperators(c) for c in children(t))
end

# Helpers. TODO Remove?
noperators(t::SyntaxLeaf) = error("Unexpected leaf token (type = $(typeof(t)))")
noperators(t::Truth) = 1
noperators(::Atom) = 0

"""
    natoms(t::SyntaxBranch)::Integer
    natoms(t::SyntaxLeaf)::Integer

Return the count of all `Atom`s appearing in a syntax tree.

See also [`atoms`](@ref), [`SyntaxToken`](@ref).
"""
function natoms(t::SyntaxBranch)::Integer
    pr = token(t) isa Atom ? 1 : 0
    return length(children(t)) == 0 ? pr : pr + sum(natoms(c) for c in children(t))
end

# Helpers. TODO Remove?
natoms(t::SyntaxLeaf) = error("Unexpected leaf token (type = $(typeof(t)))")
natoms(t::Truth) = 0
natoms(::Atom) = 1

"""
    height(t::SyntaxBranch)::Integer
    height(t::SyntaxLeaf)::Integer

Return the height of a syntax tree.

See also [`SyntaxToken`](@ref), [`tokens`](@ref).
"""
function height(t::SyntaxBranch)::Integer
    length(children(t)) == 0 ? 0 : 1 + maximum(height(c) for c in children(t))
end

# Helpers. TODO Remove?
height(t::SyntaxLeaf) = 0

# Helpers that make SyntaxBranch's map to the same dictionary key.
# Useful for checking formulas on interpretations.
function Base.isequal(a::SyntaxBranch, b::SyntaxBranch)
    return Base.isequal(token(a), token(b)) &&
        all(((c1,c2),)->Base.isequal(c1, c2), zip(children(a), children(b)))
end
Base.hash(a::SyntaxBranch) = Base.hash(syntaxstring(a))

# Refer to syntaxstring(tok::SyntaxToken; kwargs...) for documentation
function syntaxstring(
    t::SyntaxBranch;
    function_notation = false,
    remove_redundant_parentheses = true,
    parenthesize_atoms = !remove_redundant_parentheses,
    kwargs...
)
    ch_kwargs = merge((; kwargs...), (;
        function_notation = function_notation,
        remove_redundant_parentheses = remove_redundant_parentheses,
        parenthesize_atoms = parenthesize_atoms,
    ))

    # Parenthesization rules for binary operators in infix notation
    function _binary_infix_syntaxstring(
        tok::SyntaxToken,
        ch::Union{SyntaxBranch,SyntaxLeaf}
    )
        chtok = token(ch)
        chtokstring = syntaxstring(ch; ch_kwargs...)

        lpar, rpar = (!remove_redundant_parentheses) ? ["(", ")"] : ["", ""]

        if arity(chtok) == 0
            if chtok isa Atom && parenthesize_atoms # Force parenthesization
                return "($(chtokstring))"
            else
                return "$(lpar)$(chtokstring)$(rpar)"
            end
        end

        tprec = precedence(tok)
        chprec = precedence(chtok)

        # 1st condition, before "||" -> "◊¬p ∧ ¬q" instead of "(◊¬p) ∧ (¬q)"
        # 2nd condition, after  "||" -> "(q → p) → ¬q" instead of "q → p → ¬q"
        if ((!iscommutative(tok) || tok != chtok) && (tprec > chprec)) ||
            (!iscommutative(tok) && tprec <= chprec)
            lpar, rpar = "(", ")"
        end

        return "$(lpar)$(chtokstring)$(rpar)"
    end

    tok = token(t)
    tokstr = syntaxstring(tok; ch_kwargs...)

    if arity(tok) == 0
        # Leaf nodes parenthesization is parent's respsonsability
        return tokstr
    elseif arity(tok) == 2 && !function_notation
        # Infix notation for binary operators
        "$(_binary_infix_syntaxstring(tok, children(t)[1])) "*
        "$tokstr $(_binary_infix_syntaxstring(tok, children(t)[2]))"
    else
        # Infix notation with arity != 2, or function notation
        lpar, rpar = "(", ")"
        charity = arity(token(children(t)[1]))
        if !function_notation && arity(tok) == 1 &&
            (charity == 1 || (charity == 0 && !parenthesize_atoms))
            # When not in function notation, print "¬p" instead of "¬(p)";
            # note that "◊((p ∧ q) → s)" must not be simplified as "◊(p ∧ q) → s".
            lpar, rpar = "", ""
        end

        length(children(t)) == 0 ?
               tokstr :
               tokstr * "$(lpar)" * join(
                    [syntaxstring(c; ch_kwargs...) for c in children(t)], ", ") * "$(rpar)"
    end
end

# Syntax tree, the universal syntax structure representation,
#  wins when promoted with syntax structures/tokens and syntax trees
Base.promote_rule(::Type{<:SyntaxBranch}, ::Type{<:SyntaxBranch}) = SyntaxBranch
Base.promote_rule(::Type{<:AbstractSyntaxStructure}, ::Type{S}) where {S<:SyntaxBranch} = S
Base.promote_rule(::Type{S}, ::Type{<:AbstractSyntaxStructure}) where {S<:SyntaxBranch} = S

# Helper
Base.convert(::Type{S}, tok::SyntaxLeaf) where {S<:SyntaxBranch} = S(tok)
Base.convert(::Type{AbstractSyntaxStructure}, tok::SyntaxLeaf) = SyntaxBranch(tok)

"""
    tree(f::AbstractSyntaxStructure)::SyntaxTree

Extract the `SyntaxBranch` representation of a formula
(equivalent to `Base.convert(SyntaxBranch, f)`).

See also [`AbstractSyntaxStructure`](@ref), [`SyntaxBranch`](@ref).
"""
function tree(f::AbstractSyntaxStructure)::SyntaxTree
    return error("Please, provide method tree(::$(typeof(f)))::SyntaxTree.")
end

Base.convert(::Type{SyntaxTree}, f::AbstractSyntaxStructure) = tree(f)

tree(t::SyntaxTree) = t

############################################################################################

"""
    abstract type AbstractAlphabet{A} end

Abstract type for representing an alphabet of atoms with values of type `A`.
An alphabet (or *propositional alphabet*) is a set of atoms
(assumed to be
[countable](https://en.wikipedia.org/wiki/Countable_set)).

# Examples

```jldoctest
julia> Atom(1) in ExplicitAlphabet(Atom.(1:10))
true

julia> Atom(1) in ExplicitAlphabet(1:10)
true

julia> Atom(1) in AlphabetOfAny{String}()
false

julia> Atom("mystring") in AlphabetOfAny{String}()
true

julia> "mystring" in AlphabetOfAny{String}()
┌ Warning: Please, use Base.in(Atom(mystring), alphabet::AlphabetOfAny{String}) instead of Base.in(mystring, alphabet::AlphabetOfAny{String})
└ @ SoleLogics ...
true
```

# Implementation

When implementing a new alphabet type `MyAlphabet`, you should provide a method for
establishing whether an atom belongs to it or not;
while, in general, this method should be:

    function Base.in(p::Atom, a::MyAlphabet)::Bool

in the case of *finite* alphabets, it suffices to define a method:

    function atoms(a::AbstractAlphabet)::AbstractVector{atomstype(a)}

By default, an alphabet is considered finite:

    Base.isfinite(::Type{<:AbstractAlphabet}) = true
    Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))
    Base.in(p::Atom, a::AbstractAlphabet) = Base.isfinite(a) ? Base.in(p, atoms(a)) : error(...)

See also [`AbstractGrammar`](@ref), [`AlphabetOfAny`](@ref), [`Atom`](@ref),
[`ExplicitAlphabet`](@ref), [`atomstype`](@ref),  [`valuetype`](@ref).
"""
abstract type AbstractAlphabet{A} end

Base.eltype(::Type{<:AbstractAlphabet{A}}) where {A} = Atom{A}
atomstype(A::Type{<:AbstractAlphabet}) = eltype(A)
atomstype(a::AbstractAlphabet) = atomstype(typeof(a))
valuetype(a::Type{<:AbstractAlphabet}) = valuetype(atomstype(a))
valuetype(a::AbstractAlphabet) = valuetype(atomstype(a))

# Default behavior
Base.isfinite(::Type{<:AbstractAlphabet}) = true
Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))

"""
    atoms(a::AbstractAlphabet)::AbstractVector{atomstype(a)}

List the atoms of a *finite* alphabet.

See also [`AbstractAlphabet`](@ref), [`Base.isfinite`](@ref).
"""
function atoms(a::AbstractAlphabet)::AbstractVector{atomstype(a)}
    if Base.isfinite(a)
        return error("Please, provide method atoms(::$(typeof(a))).")
    else
        return error("Cannot list atoms of (infinite) alphabet of type $(typeof(a)).")
    end
end

"""
    Base.in(p::Atom, a::AbstractAlphabet)::Bool

Return whether an atom belongs to an alphabet.

See also [`AbstractAlphabet`](@ref), [`Atom`](@ref).
"""
function Base.in(p::Atom, a::AbstractAlphabet)::Bool
    if Base.isfinite(a)
        Base.in(p, atoms(a))
    else
        return error("Cannot establish whether an atom belongs to " *
            "(infinite) alphabet of type $(typeof(a)).")
    end
end

# Helper
function Base.in(value::Union{AbstractString,Number,AbstractChar}, a::AbstractAlphabet)
    @warn "Please, use Base.in(Atom($(value)), alphabet::$(typeof(a))) instead of " *
        "Base.in($(value), alphabet::$(typeof(a)))"
    Base.in(Atom(value), a)
end

"""
    Base.length(a::AbstractAlphabet)::Bool

Return the alphabet length, if it is finite.

See also [`AbstractAlphabet`](@ref), [`SyntaxBranch`](@ref).
"""
function Base.length(a::AbstractAlphabet)
    if isfinite(a)
        return Base.length(atoms(a))
    else
        return error("Cannot compute length of (infinite) alphabet of type $(typeof(a)).")
    end
end

"""
    Base.iterate(a::AbstractAlphabet)
    Base.iterate(a::AbstractAlphabet, state)

Return an iterator to the next element in an alhabet.

See also [`AbstractAlphabet`](@ref), [`SyntaxBranch`](@ref).
"""
function Base.iterate(a::AbstractAlphabet)
    if isfinite(a)
        return Base.iterate(atoms(a))
    else
        return error("Cannot iterate (infinite) alphabet of type $(typeof(a)).")
    end
end
function Base.iterate(a::AbstractAlphabet, state)
    if isfinite(a)
        return Base.iterate(atoms(a), state)
    else
        return error("Cannot iterate (infinite) alphabet of type $(typeof(a)).")
    end
end

# [Iteration interface](https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration) util.
function Base.IteratorSize(::Type{A}) where {A<:AbstractAlphabet}
    return Base.isfinite(A) ? Base.HasLength() : Base.IsInfinite()
end

"""
    struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
        atoms::Vector{Atom{A}}
    end

An alphabet wrapping atoms in a (finite) `Vector`.

See also [`AbstractAlphabet`](@ref), [`atoms`](@ref).
"""
struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
    atoms::Vector{Atom{A}}

    function ExplicitAlphabet{A}(atoms) where {A}
        return new{A}(collect(atoms))
    end

    function ExplicitAlphabet(atoms::AbstractVector{Atom{A}}) where {A}
        return ExplicitAlphabet{A}(collect(atoms))
    end

    function ExplicitAlphabet(atoms::AbstractVector{A}) where {A}
        return ExplicitAlphabet{A}(Atom.(collect(atoms)))
    end
end
atoms(a::ExplicitAlphabet) = a.atoms

Base.convert(::Type{AbstractAlphabet}, alphabet::Vector{<:Atom}) = ExplicitAlphabet(alphabet)
"""
    struct AlphabetOfAny{A} <: AbstractAlphabet{A} end

An implicit, infinite alphabet that includes all atoms with values of a subtype of A.

See also [`AbstractAlphabet`](@ref).
"""
struct AlphabetOfAny{A} <: AbstractAlphabet{A} end
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.in(::Atom{PA}, ::AlphabetOfAny{AA}) where {PA,AA} = (PA <: AA)

############################################################################################

"""
    abstract type AbstractGrammar{A<:AbstractAlphabet,O<:Operator} end

Abstract type for representing a
[context-free grammar](https://en.wikipedia.org/wiki/Context-free_grammar)
based on a *single* alphabet of type `A`, and a set of operators
that consists of all the (singleton) child types of `O`.
A context-free grammar is a simple structure for defining formulas inductively.

See also [`alphabet`](@ref),
[`atomstype`](@ref), [`tokenstype`](@ref),
[`operatorstype`](@ref), [`alphabettype`](@ref),
[`AbstractAlphabet`](@ref), [`Operator`](@ref).
"""
abstract type AbstractGrammar{A<:AbstractAlphabet,O<:Operator} end

operatorstype(::AbstractGrammar{A,O}) where {A,O} = O
alphabettype(::AbstractGrammar{A,O}) where {A,O} = A

"""
    alphabet(g::AbstractGrammar{A} where {A})::A

Return the propositional alphabet of a grammar.

See also [`AbstractAlphabet`](@ref), [`AbstractGrammar`](@ref).
"""
function alphabet(g::AbstractGrammar{A} where {A})::A
    return error("Please, provide method alphabet(::$(typeof(g))).")
end
atomstype(g::AbstractGrammar) = eltype(alphabet(g))
tokenstype(g::AbstractGrammar) = Union{operatorstype(g),atomstype(g)}


"""
    Base.in(t::SyntaxTree, g::AbstractGrammar)::Bool

Return whether a `SyntaxTree`, belongs to a grammar.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
function Base.in(::SyntaxTree, g::AbstractGrammar)::Bool
    return error("Please, provide method Base.in(::SyntaxTree, ::$(typeof(g))).")
end

# Note: when using this file's syntax tokens, these methods suffice:
Base.in(p::Atom, g::AbstractGrammar) = Base.in(p, alphabet(g))
Base.in(op::Truth, g::AbstractGrammar) = (op <: operatorstype(g))

function Base.in(tok::Connective, g::AbstractGrammar)
    return error("Please, provide method Base.in(::$(typeof(tok)), ::$(typeof(g))).")
end


"""
    formulas(
        g::AbstractGrammar;
        maxdepth::Integer,
        nformulas::Union{Nothing,Integer} = nothing,
        args...
    )::Vector{<:SyntaxBranch}

Enumerate the formulas produced by a given grammar with a finite and iterable alphabet.

# Implementation

Additional `args` can be used to model the function's behavior.
At least these two arguments should be covered:
- a `nformulas` argument can be used to limit the size of the returned `Vector`;
- a `maxdepth` argument can be used to limit the syntactic component, represented as a syntax tree,
to a given maximum depth;

See also [`AbstractGrammar`](@ref), [`SyntaxBranch`](@ref).
"""
function formulas(
    g::AbstractGrammar{A,O} where {A,O};
    maxdepth::Integer,
    nformulas::Union{Nothing,Integer} = nothing,
    args...
)::Vector{<:SyntaxBranch}
    @assert maxdepth >= 0
    @assert nformulas > 0
    if isfinite(alphabet(g))
        return error("Please, provide method formulas(::$(typeof(g)), maxdepth, " *
                     "nformulas, args...).")
    else
        return error("Cannot enumerate formulas of (infinite) " *
            "alphabet of type $(typeof(alphabet(g))).")
    end
end

function Base.isequal(a::AbstractGrammar, b::AbstractGrammar)
    Base.isequal(alphabet(a), alphabet(b)) &&
    Base.isequal(operatorstype(a), operatorstype(b))
end
Base.hash(a::AbstractGrammar) = Base.hash(alphabet(a)) + Base.hash(operatorstype(a))


"""
    struct CompleteFlatGrammar{A<:AbstractAlphabet,O<:Operator} <: AbstractGrammar{A,O}
        alphabet::A
        operators::Vector{<:O}
    end

A grammar of all well-formed formulas obtained by the arity-complying composition
of atoms of an alphabet of type `A`, and all operators in `operators`.
With n operators, this grammar has exactly n+1 production rules.
For example, with `operators = [∧,∨]`, the grammar (in Backus-Naur form) is:

    φ ::= p | φ ∧ φ | φ ∨ φ

with p ∈ alphabet. Note: it is *flat* in the sense that all rules substitute the same
(unique and starting) non-terminal symbol φ.

See also [`AbstractGrammar`](@ref),[`Operator`](@ref), [`alphabet`](@ref),
[`formulas`](@ref),[`nonterminals`](@ref), [`operators`](@ref), [`terminals`](@ref).
"""
struct CompleteFlatGrammar{A<:AbstractAlphabet,O<:Operator} <: AbstractGrammar{A,O}
    alphabet::A
    operators::Vector{<:O}

    function CompleteFlatGrammar{A,O}(
        alphabet::A,
        operators::Vector{<:O},
    ) where {A<:AbstractAlphabet,O<:Operator}
        return new{A,O}(alphabet, operators)
    end

    function CompleteFlatGrammar{A}(
        alphabet::A,
        operators::Vector{<:Operator},
    ) where {A<:AbstractAlphabet}
        return new{A,Union{typeof.(operators)...}}(
            alphabet,
            Vector{Union{typeof.(operators)...}}(operators)
        )
    end

    function CompleteFlatGrammar(
        alphabet::A,
        operators::Vector{<:Operator},
    ) where {A<:AbstractAlphabet}
        return new{A,Union{typeof.(operators)...}}(
            alphabet,
            Vector{Union{typeof.(operators)...}}(operators)
        )
    end
end

alphabet(g::CompleteFlatGrammar) = g.alphabet
operators(g::CompleteFlatGrammar) = g.operators

nonterminals(g::AbstractGrammar) = filter(!isnullary, operators(g))
function terminals(g::AbstractGrammar)
    return [atoms(alphabet(g))..., filter(isnullary, operators(g))...]
end

# A complete grammar includes any *safe* syntax tree that can be built with
#  the grammar token types.
function Base.in(t::SyntaxTree, g::CompleteFlatGrammar)::Bool
    return if token(t) isa Atom
        token(t) in alphabet(g)
    elseif token(t) isa Operator
        if operatorstype(t) <: operatorstype(g)
            true
        else
            all([Base.in(c, g) for c in children(t)])
        end
    else
        false
    end
end

"""
    formulas(
        g::CompleteFlatGrammar{A,O} where {A,O};
        maxdepth::Integer,
        nformulas::Union{Nothing,Integer} = nothing
    )::Vector{SyntaxBranch}

Generate all formulas whose `SyntaxBranch`s that are not taller than a given `maxdepth`.

See also [`AbstractGrammar`](@ref), [`SyntaxBranch`](@ref).
"""
function formulas(
    g::CompleteFlatGrammar{A,O} where {A,O};
    maxdepth::Integer,
    nformulas::Union{Nothing,Integer} = nothing,
)::Vector{SyntaxTree}
    @assert maxdepth >= 0
    @assert isnothing(nformulas) || nformulas > 0
    # With increasing `depth`, accumulate all formulas of length `depth` by combining all
    # formulas of `depth-1` using all non-terminal symbols.
    # Stop as soon as `maxdepth` is reached or `nformulas` have been generated.
    depth = 0
    cur_formulas = Vector{SyntaxTree}(
        convert.(SyntaxBranch, terminals(g)) # @Mauro by Gio: probably `terminals(g)` is fine, without conversion..? (also: now terminals = leaves, and nonterminals = connectives? Maybe we should unify, by replacing the terms `terminals` and `nonterminals`.)
    )
    all_formulas = SyntaxTree[cur_formulas...]
    while depth < maxdepth && (isnothing(nformulas) || length(all_formulas) < nformulas)
        _nformulas = length(all_formulas)
        cur_formulas = []
        for op in nonterminals(g)
            for children in Iterators.product(fill(all_formulas, arity(op))...)
                if !isnothing(nformulas) && nformulas == _nformulas + length(cur_formulas)
                    break
                end
                push!(cur_formulas, SyntaxBranch(op, Tuple(children)))
            end
            if !isnothing(nformulas) && nformulas == _nformulas + length(cur_formulas)
                break
            end
        end
        append!(all_formulas, cur_formulas)
        depth += 1
    end
    return all_formulas
end

############################################################################################
######################################## SEMANTICS #########################################
############################################################################################

"""
    abstract type AbstractAlgebra{T<:Truth} end

Abstract type for representing algebras. Algebras are used for grounding the
truth of atoms and the semantics of operators. They typically encode a
[lattice structure](https://en.wikipedia.org/wiki/Lattice_(order)) where two
elements(or nodes) *⊤* and *⊥* are referred to as *TOP* (or maximum)
and *bot* (or minimum). Each node in the lattice represents a truth value
that an atom or a formula can have on an interpretation, and the
semantics of operators is given in terms of operations between truth values.

# Implementation

When implementing a new algebra type, the methods `domain`,
`TOP`, and `bot` should be implemented.

See also [`bot`](@ref), [`BooleanAlgebra`](@ref), [`Operator`](@ref), [`TOP`](@ref),
[`collatetruth`](@ref), [`domain`](@ref), [`iscrisp`](@ref), [`truthtype`](@ref).
"""
abstract type AbstractAlgebra{T<:Truth} end

"""
    truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:Truth} = T
    truthtype(a::AbstractAlgebra) = truthtype(typeof(a))

The Julia type for representing truth values of the algebra.

See also [`AbstractAlgebra`](@ref).
"""
truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:Truth} = T
truthtype(a::AbstractAlgebra) = truthtype(typeof(a))

"""
    domain(a::AbstractAlgebra)

Return an iterator to the values in the `domain` of a given algebra.

See also [`AbstractAlgebra`](@ref).
"""
function domain(a::AbstractAlgebra{T} where {T<:Truth})::AbstractVector{T}
    return error("Please, provide method domain(::$(typeof(a))).")
end

# Note: maybe one day this will have a use?
# Base.in(t::Truth, a::AbstractAlgebra) = Base.in(t, domain(a))

"""
    top(a::AbstractAlgebra)

Return the top of a given algebra.

See also [`bot`](@ref), [`AbstractAlgebra`](@ref).
"""
function top(a::AbstractAlgebra{T} where {T})::T
    return error("Please, provide method top(::$(typeof(a))).")
end

"""
    bot(a::AbstractAlgebra)

Return the bottom of a given algebra.

See also [`top`](@ref), [`AbstractAlgebra`](@ref).
"""
function bot(a::AbstractAlgebra{T} where {T})::T
    return error("Please, provide method bot(::$(typeof(a))).")
end

"""
    iscrisp(a::AbstractAlgebra) = iscrisp(typeof(a))

An algebra is crisp (or *boolean*) when its domain only has two values, namely,
the top and the bottom. The antonym of crisp is *fuzzy*.

See also [`AbstractAlgebra`](@ref).
"""
iscrisp(a::AbstractAlgebra) = (length(domain(a)) == 2)

joinformulas(c::Truth, ::Tuple{}) = SyntaxTree(c)

############################################################################################

"""
    istop(::Truth)::Bool

Return true if the `Truth` value is the top of its algebra.
For example, in the crisp case, with `Bool` truth values, it is:

    istop(t::Bool)::Bool = (t == true)

See also [`isbot`](@ref), [`Truth`](@ref).
"""
istop(t::Truth)::Bool = false

"""
    isbot(::Truth)::Bool

Return true if the `Truth` value is the bottom of its algebra.
For example, in the crisp case, with `Bool` truth values, it is:

    isbot(t::Bool)::Bool = (t == false)

See also [`istop`](@ref), [`Truth`](@ref).
"""
isbot(t::Truth)::Bool = false

# Helpers
(c::Truth)(::Tuple{}) = c # TODO is this the correct place? What's the role for this? docstring?
function Base.convert(::Type{Truth}, t)::Truth
    return error("Cannot interpret value $t of type ($(typeof(t))) as Truth.")
end

############################################################################################

"""
    abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

Abstract type of a logic, which comprehends a context-free grammar (*syntax*) and
an algebra (*semantics*).

# Implementation

When implementing a new logic type,
the methods `grammar` and `algebra` should be implemented.

See also [`AbstractAlgebra`](@ref), [`AbstractGrammar`](@ref).
"""
abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

"""
    grammar(l::AbstractLogic{G})::G where {G<:AbstractGrammar}

Return the `grammar` of a given logic.

See also [`AbstractGrammar`](@ref), [`AbstractLogic`](@ref), [`algebra`](@ref),
[`alphabet`](@ref), [`formulas`](@ref), [`grammar`](@ref), [`operators`](@ref),
[`truthtype`](@ref).
"""
function grammar(l::AbstractLogic{G})::G where {G}
    return error("Please, provide method grammar(::$(typeof(l))).")
end

operatorstype(l::AbstractLogic) = operatorstype(grammar(l))
alphabettype(l::AbstractLogic) = alphabettype(grammar(l))
operators(l::AbstractLogic) = operators(grammar(l))
alphabet(l::AbstractLogic) = alphabet(grammar(l))
atomstype(l::AbstractLogic) = atomstype(alphabet(l))
tokenstype(l::AbstractLogic) = tokenstype(grammar(l))
formulas(l::AbstractLogic, args...; kwargs...) = formulas(grammar(l), args...; kwargs...)

Base.in(op::Operator, l::AbstractLogic) = Base.in(op, grammar(l))
Base.in(t::SyntaxBranch, l::AbstractLogic) = Base.in(t, grammar(l))
Base.in(p::Atom, l::AbstractLogic) = Base.in(p, alphabet(l))

"""
    algebra(l::AbstractLogic{G,A})::A where {G,A}

Return the `algebra` of a given logic.

See also [`AbstractAlgebra`](@ref), [`AbstractLogic`](@ref).
"""
function algebra(l::AbstractLogic{G,A})::A where {G,A}
    return error("Please, provide method algebra(::$(typeof(l))).")
end

truthtype(l::AbstractLogic) = truthtype(algebra(l))
top(l::AbstractLogic) = top(algebra(l))
bot(l::AbstractLogic) = bot(algebra(l))
iscrisp(l::AbstractLogic) = iscrisp(algebra(l))

function Base.isequal(a::AbstractLogic, b::AbstractLogic)
    Base.isequal(grammar(a), grammar(b)) &&
    Base.isequal(algebra(a), algebra(b))
end
Base.hash(a::AbstractLogic) = Base.hash(grammar(a)) + Base.hash(algebra(a))


############################################################################################

"""
    abstract type AbstractInterpretation{A,T<:Truth} end

Abstract type for representing a propositional
[interpretation](https://en.wikipedia.org/wiki/Interpretation_(logic))
(or propositional model)
that associates truth values of a type `T` to atoms of value type `A`.
In the case of
[propositional logic](https://simple.wikipedia.org/wiki/Propositional_logic),
is essentially a map *atom → truth value*.

Properties expressed via logical formulas can be `check`ed on logical interpretations.

See also [`check`](@ref), [`AbstractAssignment`](@ref), [`AbstractKripkeStructure`](@ref).
"""
abstract type AbstractInterpretation{A,T<:Truth} end

valuetype(::AbstractInterpretation{A,T}) where {A,T} = A
truthtype(::AbstractInterpretation{A,T}) where {A,T} = T

"""
    check(
        φ::Formula,
        i::AbstractInterpretation,
        args...;
        kwargs...
    )::Bool

Check a formula on a logical interpretation (or model), returning `true` if the truth value
for the formula `istop`.
This process is referred to as (finite)
[model checking](https://en.wikipedia.org/wiki/Model_checking), and there are many
algorithms for it, typically depending on the complexity of the logic.

# Examples
```jldoctest
julia> @atoms String p q
2-element Vector{Atom{String}}:
 Atom{String}("p")
 Atom{String}("q")

julia> td = TruthDict([p => TOP, q => BOT])
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│      ⊥ │      ⊤ │
└────────┴────────┘

julia> check(CONJUNCTION(p,q), td)
false
```

See also [`interpret`](@ref), [`Formula`](@ref), [`AbstractInterpretation`](@ref),
[`TruthDict`](@ref).
"""
function check(
    φ::Formula,
    i::AbstractInterpretation,
    args...;
    kwargs...
)::Bool
    istop(interpret(φ, i, args...; kwargs...))
end

"""
    interpret(
        φ::Formula,
        i::AbstractInterpretation,
        args...;
        kwargs...
    )::Formula

Return the truth value for a formula on a logical interpretation (or model).

# Examples
```jldoctest
julia> @atoms String p q
2-element Vector{Atom{String}}:
 Atom{String}("p")
 Atom{String}("q")

julia> td = TruthDict([p => true, q => false])
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│  false │   true │
└────────┴────────┘

julia> interpret(CONJUNCTION(p,q), td)
⊥
```

See also [`check`](@ref), [`Formula`](@ref), [`AbstractInterpretation`](@ref),
[`AbstractAlgebra`](@ref).
"""
function interpret(
    φ::Formula,
    i::AbstractInterpretation,
    args...;
    kwargs...
)::Formula
    interpret(tree(φ), i, args...; kwargs...)
end

function interpret(
    φ::SyntaxBranch,
    i::AbstractInterpretation,
    args...;
    kwargs...
)::Formula
    return error("Please, provide method " *
                 "interpret(φ::SyntaxBranch, i::$(typeof(i)), " *
                 "args...::$(typeof(args)); " *
                 "kwargs...::$(typeof(kwargs))::$(truthtype(i)).")
end

############################################################################################
######################################### UTILS ############################################
############################################################################################

# Formula interpretation via i[φ] -> φ
Base.getindex(i::AbstractInterpretation, φ::Formula, args...; kwargs...) =
    interpret(φ, i, args...; kwargs...)

# Formula interpretation via φ(i) -> φ
(φ::Formula)(i::AbstractInterpretation, args...; kwargs...) =
    interpret(φ, i, args...; kwargs...)


# We provide an extra safety layer by complementing
# Base.in with syntax tokens/trees and alphabets.
function Base.in(t::Union{SyntaxToken,AbstractSyntaxStructure}, a::AbstractAlphabet)
    return error("Attempting Base.in($(typeof(t)), ::$(typeof(a))), " *
                 "but objects of type $(typeof(t)) cannot belong to alphabets.")
end

"""
An alphabet of `valuetype` `A` can be used for instantiating atoms of valuetype `A`.
"""
(::AbstractAlphabet{A})(a) where {A} = Atom{A}(a)

"""
    (op::Operator)(o::Any)

An `Operator` can be used to compose syntax tokens (e.g., atoms),
syntax trees and/or formulas.

# Examples
```jldoctest
    ¬(Atom(1)) ∨ Atom(1) ∧ ⊤
    ∧(⊤,⊤)
    ⊤()
```
"""
function (op::Operator)(o::Any)
    return error("Cannot apply operator $(op)::$(typeof(op)) to object $(o)::$(typeof(o))")
end

function (op::Operator)(children::Union{SyntaxToken,Formula}...)
    return op(children)
end

function (op::Operator)(
    children::NTuple{N,Union{SyntaxToken,Formula}},
) where {N}
    T = Base.promote_type((typeof.(children))...)
    if T <: Union{SyntaxBranch,SyntaxToken}
        return joinformulas(op, tree.(children))
    elseif T <: AbstractSyntaxStructure
        return joinformulas(op, children) # Force SyntaxBranch?
        # return joinformulas(op, Base.promote(children...))
        # println(typeof.(children))
        # println(typeof.(Base.promote(children...)))
        # return joinformulas(op, children)
    else
        # println(typeof.(children))
        return joinformulas(op, Base.promote(children...))
    end
end
