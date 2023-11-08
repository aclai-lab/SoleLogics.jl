#=
    Syntactical Type Hierarchy

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
    │   ├── TruthTable
    │   ├── AnchoredFormula
    │   └── ...
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

include("docstrings.jl")

############################################################################################
#### Syntax Base ##########################################################################
############################################################################################
"""
    abstract type Syntactical end

Master abstract type for all syntactical objects (e.g., formulas, connectives).

See also [`Formula`](@ref), [`Connective`](@ref).
"""
abstract type Syntactical end

"""$(doc_syntaxstring)"""
function syntaxstring(s::Syntactical; kwargs...)::String
    return error("Please, provide method syntaxstring(::$(typeof(s)); kwargs...).")
end

function Base.show(io::IO, φ::Syntactical)
    print(io, "$(typeof(φ))\nsyntaxstring: $(syntaxstring(φ))")
end

############################################################################################
#### Connective ############################################################################
############################################################################################

"""
    abstract type Connective <: Syntactical end

Abstract type for [logical connectives](https://en.wikipedia.org/wiki/Logical_connective),
that are used to express non-atomic statements;
for example, CONJUNCTION, DISJUNCTION, NEGATION and IMPLICATION (stylized as ∧, ∨, ¬ and →).

# Implementation

When implementing a new type `C` for a connective, please define its `arity`.
For example, with a binary operator (e.g., ∨ or ∧):

    arity(::C) = 2

When implementing a new type `C` for a *commutative* connective with arity higher than 1,
please provide a method `iscommutative(::C)`. This can speed up model checking operations.

When implementing a custom binary connective, one can override the default `precedence` and
`associativity` (see https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity).
If the custom connective is a `NamedConnective` and renders as something considered as a
`math symbol` (for example, `⊙`, see https://stackoverflow.com/a/60321302/5646732),
by the Julia parser, `Base.operator_precedence`
and `Base.operator_associativity` are used to define these behaviors, and
you might want to avoid providing these methods at all.

Here is an example of a custom implementation of the xor (⊻) boolean operator.
```jldoctest
julia> const ⊻ = SoleLogics.NamedConnective{:⊻}()
julia> SoleLogics.arity(::typeof(⊻)) = 2
julia> SoleLogics.iscommutative(::typeof(⊻)) = true
```

See also [`arity`](@ref),
[`SyntaxBranch`](@ref), [`associativity`](@ref), [`precedence`](@ref),
[`check`](@ref),
[`iscommutative`](@ref), [`NamedConnective`](@ref),
[`Syntactical`](@ref).
"""
abstract type Connective <: Syntactical end

"""$(doc_arity)"""
arity(c::Connective)::Integer = error("Please, provide method arity(::$(typeof(c))).")

# Helpers
isnullary(c) = arity(c) == 0
isunary(c)   = arity(c) == 1
isbinary(c)  = arity(c) == 2
isternary(c) = arity(c) == 3

"""$(doc_iscommutative)"""
function iscommutative(c::Connective)
    return arity(c) <= 1 # Unless otherwise specified
end

"""$(doc_precedence)"""
function precedence(c::Connective)
    return error("Please, provide method precedence(c::$(typeof(c))).")
end

"""$(doc_associativity)"""
associativity(::Connective) = :left

############################################################################################
#### Formula ###############################################################################
############################################################################################

"""
    abstract type Formula <: Syntactical end

Abstract type for logical formulas.
Examples of `Formula`s are `SyntaxLeaf`s (for example, `Atom`s and
`Truth` values), `AbstractSyntaxStructure`s (for example, `SyntaxTree`s and
`LeftmostLinearForm`s) and `TruthTable`s (
enriched representation, which associates a syntactic structure with
additional [memoization](https://en.wikipedia.org/wiki/Memoization) structures,
which can save computational time upon
[model checking](https://en.wikipedia.org/wiki/Model_checking)).

Any formula can be converted into its [`SyntaxTree`](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
representation via [`tree`](@ref); its [`height`](@ref) can be computed,
and it can be queried for its syntax [`tokens`](@ref), [`atoms`](@ref), etc...
It can be parsed from its [`syntaxstring`](@ref) representation via [`parseformula`](@ref).

See also [`tree`](@ref), [`AbstractSyntaxStructure`](@ref), [`SyntaxLeaf`](@ref).
"""
abstract type Formula <: Syntactical end

"""
    tree(φ::Formula)::SyntaxTree

Return the `SyntaxTree` representation of a formula;
note that this is equivalent to `Base.convert(SyntaxTree, φ)`.

See also [`Formula`](@ref), [`SyntaxTree`](@ref).
"""
function tree(φ::Formula)
    return error("Please, provide method tree(::$(typeof(φ)))::SyntaxTree.")
end

"""
    height(φ::Formula)::Integer

Return the height of a formula in its syntax tree representation.

See also [`SyntaxTree`](@ref).
"""
function height(φ::Formula)::Integer
    return height(tree(φ))
end

"""$(doc_tokopprop)"""
function tokens(φ::Formula) # ::AbstractVector{<:SyntaxToken}
    return tokens(tree(φ))
end
"""$(doc_tokopprop)"""
function atoms(φ::Formula) # ::AbstractVector{<:Atom}
    return atoms(tree(φ))
end
"""$(doc_tokopprop)"""
function truths(φ::Formula) # ::AbstractVector{<:Truth}
    return truths(tree(φ))
end
"""$(doc_tokopprop)"""
function leaves(φ::Formula) # ::AbstractVector{<:SyntaxLeaf}
    return leaves(tree(φ))
end
"""$(doc_tokopprop)"""
function connectives(φ::Formula) # ::AbstractVector{<:Connective}
    return connectives(tree(φ))
end
"""$(doc_tokopprop)"""
function operators(φ::Formula) # ::AbstractVector{<:Operator}
    return operators(tree(φ))
end
"""$(doc_tokopprop)"""
function ntokens(φ::Formula)::Integer
    return ntokens(tree(φ))
end
"""$(doc_tokopprop)"""
function natoms(φ::Formula)::Integer
    return natoms(tree(φ))
end
"""$(doc_tokopprop)"""
function ntruths(φ::Formula)::Integer
    return ntruths(tree(φ));
end
"""$(doc_tokopprop)"""
function nleaves(φ::Formula)::Integer
    return nleaves(tree(φ));
end
"""$(doc_tokopprop)"""
function nconnectives(φ::Formula)::Integer
    return nconnectives(tree(φ));
end
"""$(doc_tokopprop)"""
function noperators(φ::Formula)::Integer
    return noperators(tree(φ));
end

function Base.isequal(φ1::Formula, φ2::Formula)
    Base.isequal(tree(φ1), tree(φ2))
end

Base.hash(φ::Formula) = Base.hash(tree(φ))

function syntaxstring(φ::Formula; kwargs...)
    syntaxstring(tree(φ); kwargs...)
end

"""$(doc_composeformulas)"""
function composeformulas(c::Connective, ::NTuple{N,F})::F where {N,F<:Formula}
    return error("Please, provide method " *
        "composeformulas(c::Connective, φs::NTuple{N,$(F)}) where {N}.")
end

# Note: don't type the output as F
function composeformulas(c::Connective, φs::Vararg{Formula,N}) where {N}
    return composeformulas(c, φs)
end

############################################################################################
#### AbstractSyntaxStructure ###############################################################
############################################################################################

"""
    abstract type AbstractSyntaxStructure <: Formula end

Abstract type for the purely-syntactic component of a logical formula (e.g.,
no fancy memoization structure associated). The typical representation is the
[`SyntaxTree`](@ref), however, different implementations can cover specific syntactic forms
(e.g., [conjunctive](https://en.wikipedia.org/wiki/Conjunctive_normal_form) or
[disjunctive](https://en.wikipedia.org/wiki/Disjunctive_normal_form) normal forms).

See also [`Formula`](@ref), [`AbstractLogic`](@ref), [`SyntaxTree`](@ref),
[`tree`](@ref).
"""
abstract type AbstractSyntaxStructure <: Formula end

function composeformulas(c::Connective, φs::NTuple{N,AbstractSyntaxStructure}) where {N}
    return composeformulas(c, tree.(φs))
end

############################################################################################
#### SyntaxTree ############################################################################
############################################################################################

import AbstractTrees: children

"""
    abstract type SyntaxTree <: AbstractSyntaxStructure end

Abstract type for syntax leaves (see `SyntaxLeaf`, such as `Truth` values and `Atom`s),
and their composition via `Connective`s (i.e., `SyntaxBranch`).

See also [`SyntaxLeaf`](@ref), [`SyntaxBranch`](@ref),
[`AbstractSyntaxStructure`](@ref), [`Formula`](@ref).
"""
abstract type SyntaxTree <: AbstractSyntaxStructure end

tree(φ::SyntaxTree) = φ

"""$(doc_syntaxtree_children)"""
function children(φ::SyntaxTree)
    return error("Please, provide method children(::$(typeof(φ))).")
end

"""$(doc_syntaxtree_token)"""
function token(φ::SyntaxTree)
    return error("Please, provide method token(::$(typeof(φ))).")
end

"""$(doc_arity)"""
arity(φ::SyntaxTree) = length(children(φ))

function height(φ::SyntaxTree)
    return length(children(φ)) == 0 ? 0 : 1 + maximum(height(c) for c in children(φ))
end
function tokens(φ::SyntaxTree) # ::AbstractVector{<:SyntaxToken}
    return SyntaxToken[vcat(tokens.(children(φ))...)..., token(φ)]
end
function atoms(φ::SyntaxTree) # ::AbstractVector{<:Atom}
    a = token(φ) isa Atom ? [token(φ)] : []
    return Atom[vcat(atoms.(children(φ))...)..., a...]
end
function truths(φ::SyntaxTree) # ::AbstractVector{<:Truth}
    t = token(φ) isa Truth ? [token(φ)] : []
    return Truth[vcat(truths.(children(φ))...)..., t...]
end
function leaves(φ::SyntaxTree) # ::AbstractVector{<:SyntaxLeaf}
    l = token(φ) isa SyntaxLeaf ? [token(φ)] : []
    return SyntaxLeaf[vcat(leaves.(children(φ))...)..., l...]
end
function connectives(φ::SyntaxTree) # ::AbstractVector{<:Connective}
    c = token(φ) isa Connective ? [token(φ)] : []
    return Connective[vcat(connectives.(children(φ))...)..., c...]
end
function operators(φ::SyntaxTree) # ::AbstractVector{<:Operator}
    c = token(φ) isa Operator ? [token(φ)] : []
    return Operator[vcat(operators.(children(φ))...)..., c...]
end
function ntokens(φ::SyntaxTree)::Integer
    return length(children(φ)) == 0 ? 1 : 1 + sum(ntokens(c) for c in children(φ))
end
function natoms(φ::SyntaxTree)::Integer
    a = token(φ) isa Atom ? 1 : 0
    return length(children(φ)) == 0 ? a : a + sum(natoms(c) for c in children(φ))
end
function ntruths(φ::SyntaxTree)::Integer
    t = token(φ) isa Truth ? 1 : 0
    return length(children(φ)) == 0 ? t : t + sum(ntruths(c) for c in children(φ))
end
function nleaves(φ::SyntaxTree)::Integer
    op = token(φ) isa SyntaxLeaf ? 1 : 0
    return length(children(φ)) == 0 ? op : op + sum(nleaves(c) for c in children(φ))
end
function nconnectives(φ::SyntaxTree)::Integer
    c = token(φ) isa Connective ? 1 : 0
    return length(children(φ)) == 0 ? c : c + sum(nconnectives(c) for c in children(φ))
end
function noperators(φ::SyntaxTree)::Integer
    op = token(φ) isa Operator ? 1 : 0
    return length(children(φ)) == 0 ? op : op + sum(noperators(c) for c in children(φ))
end

function Base.isequal(a::SyntaxTree, b::SyntaxTree)
    return (
        (arity(a) == 0 && arity(b) == 0 && a == b) ||
        (Base.isequal(token(a), token(b)) &&
                all(((c1,c2),)->Base.isequal(c1, c2), zip(children(a), children(b))))
    )
end

Base.hash(φ::SyntaxTree) = Base.hash(token(φ), Base.hash(children(φ)))

# Helpers
tokentype(φ::SyntaxTree) = typeof(token(φ))
tokenstype(φ::SyntaxTree) = Union{tokentype(φ),tokenstype.(children(φ))...}
atomstype(φ::SyntaxTree) = typeintersect(Atom, tokenstype(φ))
truthstype(φ::SyntaxTree) = typeintersect(Truth, tokenstype(φ))
leavestype(φ::SyntaxTree) = typeintersect(SyntaxLeaf, tokenstype(φ))
connectivestype(φ::SyntaxTree) = typeintersect(Connective, tokenstype(φ))
operatorstype(φ::SyntaxTree) = typeintersect(Operator, tokenstype(φ))

function composeformulas(c::Connective, φs::NTuple{N,SyntaxTree}) where {N}
    return SyntaxBranch(c, φs)
end


function Base.show(io::IO, φ::SyntaxTree)
    # print(io, "$(typeof(φ))($(syntaxstring(φ)))")
    print(io, "$(typeof(φ)): $(syntaxstring(φ))")
    # print(io, "$(syntaxstring(φ))")
end

# Syntax tree, the universal syntax structure representation,
# wins when promoted with syntax structures/tokens and syntax trees.
Base.promote_rule(::Type{<:SyntaxTree}, ::Type{<:SyntaxTree}) = SyntaxTree
Base.promote_rule(::Type{<:AbstractSyntaxStructure}, ::Type{S}) where {S<:SyntaxTree} = S
Base.promote_rule(::Type{S}, ::Type{<:AbstractSyntaxStructure}) where {S<:SyntaxTree} = S

# TODO figure out: are both of these needed? Maybe one of the two is enough
SyntaxTree(φ::Formula) = tree(φ)
Base.convert(::Type{SyntaxTree}, φ::Formula) = tree(φ)

# Syntax tree composition
function SyntaxTree(φ::SyntaxTree, ::Tuple{})
    return φ
end
function SyntaxTree(φ::SyntaxTree)
    return φ
end
function SyntaxTree(c::Connective, φs::NTuple{N,SyntaxTree}) where {N}
    return composeformulas(c, φs)
end
function SyntaxTree(c::Connective, φs::Vararg{SyntaxTree,N}) where {N}
    return composeformulas(c, φs)
end

############################################################################################
#### SyntaxLeaf ############################################################################
############################################################################################

"""
    abstract type SyntaxLeaf <: AbstractSyntaxStructure end

An atomic logical element, like a `Truth` value or an `Atom`.
`SyntaxLeaf`s have `arity` equal to zero, meaning that they are not
allowed to have children in tree-like syntactic structures.

See also [`AbstractSyntaxStructure`](@ref),  [`arity`](@ref), [`SyntaxBranch`](@ref).
"""
abstract type SyntaxLeaf <: SyntaxTree end

children(::SyntaxLeaf) = ()

token(φ::SyntaxLeaf) = φ

############################################################################################
#### SyntaxToken ###########################################################################
############################################################################################

"""
    const SyntaxToken = Union{Connective,SyntaxLeaf}

Union type for values wrapped in `SyntaxTree` nodes.

See also [`SyntaxTree`](@ref), [`SyntaxLeaf`](@ref), [`Connective`](@ref).
"""
const SyntaxToken = Union{Connective,SyntaxLeaf}

"""$(doc_dual)"""
dual(t::SyntaxToken) = error("Please, provide method dual(::$(typeof(t))).")

"""$(doc_dual)"""
hasdual(t::SyntaxToken) = false

"""$(doc_formula_basein)"""
function Base.in(tok::SyntaxToken, φ::SyntaxTree)::Bool # TODO Note that this is interface for SyntaxTree's
    return error("Please, provide method Base.in(tok::$(typeof(tok)), φ::$(typeof(φ))).")
end

function Base.in(tok::SyntaxToken, φ::Formula)::Bool
    return Base.in(tok, tree(φ))
end

function Base.in(tok::SyntaxToken, φ::SyntaxLeaf)::Bool
    return tok == φ
end

############################################################################################
#### Atom ##################################################################################
############################################################################################

"""
    struct Atom{V} <: SyntaxLeaf
        value::V
    end

An atom, sometimes called an atomic proposition,
propositional letter (or simply *letter*), of type
`Atom{V}` wraps a `value::V` representing a fact which truth can be assessed on
a logical interpretation.

Atoms are nullary tokens (i.e, they are at the leaves of a syntax tree);
note that their atoms cannot be `Atom`s.

See also [`AbstractInterpretation`](@ref), [`atoms`](@ref), [`check`](@ref),
[`SyntaxToken`](@ref).
"""
struct Atom{V} <: SyntaxLeaf
    value::V

    function Atom{V}(value::V) where {V}
        @assert !(value isa Union{Formula,Connective}) "Illegal nesting. " *
            "Cannot instantiate Atom with value of type $(typeof(value))"
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

value(p::Atom) = p.value

dual(p::Atom) = Atom(dual(value(p)))
dual(value) = error("Please, provide method SoleLogics.dual(::$(typeof(value))).") # TODO explain why?

valuetype(::Atom{V}) where {V} = V
valuetype(::Type{Atom{V}}) where {V} = V

Base.convert(::Type{A}, p::Atom) where {A<:Atom} = A(p)
Base.convert(::Type{A}, a) where {A<:Atom} = A(a)

Base.isequal(a::Atom, b::Atom) = Base.isequal(value(a), value(b)) # Needed to avoid infinite recursion
Base.isequal(a::Atom, b) = Base.isequal(value(a), b)
Base.isequal(a, b::Atom) = Base.isequal(a, value(b))
Base.isequal(a::Atom, b::SyntaxTree) = (a == b) # Needed for resolving ambiguities
Base.isequal(a::SyntaxTree, b::Atom) = (a == b) # Needed for resolving ambiguities
Base.hash(a::Atom) = Base.hash(value(a))

syntaxstring(a::Atom; kwargs...)::String = syntaxstring(value(a); kwargs...)

syntaxstring(value; kwargs...) = string(value)

############################################################################################
#### Truth #################################################################################
############################################################################################

"""
    abstract type Truth <: SyntaxLeaf end

Abstract type for syntax leaves representing values of a lattice algebra.
In Boolean logic, the two [`BooleanTruth`](@ref) values [`Top`](@ref)
and [`Bot`](@ref) are tused.

Here is an example about inventing a new Truth subtype, MyTruth, where a new value, α,
is defined to be inbetween the canonical boolean top and bottom.
```jldoctest
julia> abstract type MyTruth <: Truth end

julia> struct MyTop <: MyTruth end
julia> const MYTOP = MyTop()
julia> const ⫪ = MYTOP # Note that ⊤ is already use to indicate BooleanTruth's top.
julia> syntaxstring(::MyTop; kwargs...) = "⫪"

julia> struct MyBot <: MyTruth end
julia> const MYBOT = MyBot()
julia> const ⫫ = MYBOT # Note that ⊥ is already use to indicate BooleanTruth's top.
julia> syntaxstring(::MyBot; kwargs...) = "⫫"

julia> struct MyAlpha <: MyTruth end
julia> const MYALPHA = MyAlpha()
julia> const α = MYALPHA
julia> syntaxstring(::MyAlpha; kwargs...) = "α"

julia> istop(t::MyTop) = true
julia> isbot(t::MyBot) = true

See also [`Top`](@ref), [`Bot`](@ref), [`BooleanTruth`](@ref).
"""
abstract type Truth <: SyntaxLeaf end

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

"""
    truthsupertype(T::Type{<:Truth})::Type

Return the supertype of a `Truth` type that includes all values of the same algebra.

# Examples
```jldoctest
julia> truthsupertype(typeof(TOP))
BooleanTruth
```

See also [`Truth`](@ref), [`TruthDict`](@ref).
"""
function truthsupertype(T::Type{<:Truth})
    return T
end

function Base.convert(::Type{Truth}, t)::Truth
    return error("Cannot interpret value $t of type ($(typeof(t))) as Truth.")
end

# Fallback
Base.convert(::Type{Truth}, t::Truth) = t

# Helper: composeformulas actually works for operators as well
composeformulas(c::Truth, ::Tuple{}) = c

############################################################################################
#### Operator ##############################################################################
############################################################################################

"""
    const Operator = Union{Connective,Truth}

Union type for logical constants of any ariety (zero for `Truth` values, non-zero for
`Connective`s).

See also [`Connective`](@ref), [`Truth`](@ref).
"""
const Operator = Union{Connective,Truth}

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

function (op::Operator)(φs::Formula...)
    return op(φs)
end

# function (op::Operator)(φs::NTuple{N, F}) where {N,F<:Formula}
function (op::Operator)(φs::NTuple{N,Formula}) where {N}
    if arity(op) == 2 && length(φs) > arity(op)
        if associativity(op) == :right
            φs = (φs[1], op(φs[2:end]))
        else
            φs = (op(φs[1:end-1]), φs[end])
        end
    end
    AbstractSyntaxStructure
    if AbstractSyntaxStructure <: typejoin(typeof.(φs)...)
        φs = Base.promote(φs...)
    end
    return composeformulas(op, φs)
end

(c::Truth)(::Tuple{}) = c

############################################################################################
#### SyntaxBranch ##########################################################################
############################################################################################

"""
    struct SyntaxBranch{T<:Connective} <: SyntaxTree
        token::T
        children::NTuple{N,SyntaxTree} where {N}
    end

An internal node of a syntax tree encoding a logical formula.
Such a node holds a syntax `token` (a `Connective`,
and has as many children as the `arity` of the token.

This implementation is *arity-compliant*, in that, upon construction,
the arity of the token is checked against the number of children provided.

# Examples
```jldoctest
julia> p,q = Atom.([p, q])
2-element Vector{Atom{String}}:
 Atom{String}: p
 Atom{String}: q
julia> branch = SyntaxBranch(CONJUNCTION, p, q)
SyntaxBranch{NamedConnective{:∧}}: p ∧ q
julia> token(branch)
∧
julia> children(branch)
(Atom{String}: p, Atom{String}: q)
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
[`atoms`](@ref), [`natoms`](@ref), [`atomstype`](@ref),
[`operators`](@ref), [`noperators`](@ref), [`operatorstype`](@ref),
[`tokens`](@ref), [`ntokens`](@ref), [`tokenstype`](@ref),
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
        _aritycheck(N, T, token, children)
        return new{T}(token, children)
    end

    function SyntaxBranch{T}(
        φ::SyntaxBranch{T},
    ) where {T<:Connective}
        return SyntaxBranch{T}(token(φ), children(φ))
    end

    function SyntaxBranch(
        token::T,
        children::NTuple{N,SyntaxTree} = (),
    ) where {T<:Connective,N}
        _aritycheck(N, T, token, children)
        return new{T}(token, children)
    end

    # Helpers
    function SyntaxBranch{T}(token::T, children...) where {T<:Connective}
        return SyntaxBranch{T}(token, children)
    end
    function SyntaxBranch(token::T, children...) where {T<:Connective}
        return SyntaxBranch(token, children)
    end

end

"""$(doc_syntaxtree_children)"""
children(φ::SyntaxBranch) = φ.children

"""$(doc_syntaxtree_token)"""
token(φ::SyntaxBranch) = φ.token

function syntaxstring(
    φ::SyntaxBranch;
    function_notation = false,
    remove_redundant_parentheses = true,
    parenthesize_atoms = !remove_redundant_parentheses,
    kwargs...
)::String
    ch_kwargs = merge((; kwargs...), (;
        function_notation = function_notation,
        remove_redundant_parentheses = remove_redundant_parentheses,
        parenthesize_atoms = parenthesize_atoms,
    ))

    # Parenthesization rules for binary operators in infix notation
    function _binary_infix_syntaxstring(
        tok::SyntaxToken,
        ch::SyntaxTree
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

    tok = token(φ)
    tokstr = syntaxstring(tok; ch_kwargs...)

    if arity(tok) == 0
        # Leaf nodes parenthesization is parent's respsonsability
        return tokstr
    elseif arity(tok) == 2 && !function_notation
        # Infix notation for binary operators
        "$(_binary_infix_syntaxstring(tok, children(φ)[1])) "*
        "$tokstr $(_binary_infix_syntaxstring(tok, children(φ)[2]))"
    else
        # Infix notation with arity != 2, or function notation
        lpar, rpar = "(", ")"
        charity = arity(token(children(φ)[1]))
        if !function_notation && arity(tok) == 1 &&
            (charity == 1 || (charity == 0 && !parenthesize_atoms))
            # When not in function notation, print "¬p" instead of "¬(p)";
            # note that "◊((p ∧ q) → s)" must not be simplified as "◊(p ∧ q) → s".
            lpar, rpar = "", ""
        end

        length(children(φ)) == 0 ?
               tokstr :
               tokstr * "$(lpar)" * join(
                    [syntaxstring(c; ch_kwargs...) for c in children(φ)], ", ") * "$(rpar)"
    end
end

function Base.in(tok::SyntaxToken, tree::SyntaxBranch)::Bool
    return tok == token(tree) || any([Base.in(tok, c) for c in children(tree)])
end

############################################################################################
#### AbstractInterpretation ################################################################
############################################################################################

"""
    abstract type AbstractInterpretation end

Abstract type for representing a logical
[interpretation](https://en.wikipedia.org/wiki/Interpretation_(logic)).
In the case of
[propositional logic](https://simple.wikipedia.org/wiki/Propositional_logic),
is essentially a map *atom → truth value*.

Properties expressed via logical formulas can be `check`ed on logical interpretations.

See also [`check`](@ref), [`AbstractAssignment`](@ref), [`AbstractKripkeStructure`](@ref).
"""
abstract type AbstractInterpretation end

function valuetype(i::AbstractInterpretation)
    return error("Please, provide method valuetype(::$(typeof(i))).")
end
function truthtype(i::AbstractInterpretation)
    return error("Please, provide method truthtype(::$(typeof(i))).")
end

############################################################################################
#### Interpret & Check #####################################################################
############################################################################################

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
julia> @atoms p q
2-element Vector{Atom{String}}:
 p
 q

julia> td = TruthDict([p => true, q => false])
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│      ⊥ │      ⊤ │
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

interpret(t::Truth, i::AbstractInterpretation, args...; kwargs...) = t

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

############################################################################################
#### Utilities #############################################################################
############################################################################################

# Formula interpretation via i[φ] -> φ
Base.getindex(i::AbstractInterpretation, φ::Formula, args...; kwargs...) =
    interpret(φ, i, args...; kwargs...)

# Helper
function Base.getindex(i::AbstractInterpretation, v, args...; kwargs...)
    Base.getindex(i, Atom(v), args...; kwargs...)
end

# Formula interpretation via φ(i) -> φ
(φ::Formula)(i::AbstractInterpretation, args...; kwargs...) =
    interpret(φ, i, args...; kwargs...)
