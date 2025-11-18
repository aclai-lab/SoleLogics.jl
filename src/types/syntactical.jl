#=
    Syntactical Types Hierarchy

    Syntactical
    ├── Connective (e.g, ∧, ¬, □, ◊, ⟨G⟩)
    └── Formula
        └── SyntaxStructure
            └── SyntaxTree
                ├── SyntaxLeaf
                │   ├── AbstractAtom (e.g., p)
                │   └── Truth (e.g, ⊤, ⊥)
                └── AbstractSyntaxBranch (e.g., p ∧ q)

    Also:
    const Operator = Union{Connective,Truth}
    const SyntaxToken = Union{Connective,SyntaxLeaf}

=#

include("docstrings.jl")

############################################################################################
#### Syntax Base ###########################################################################
############################################################################################
"""
    abstract type Syntactical end

Abstract type for all syntactical objects (e.g., formulas, connectives).

# Interface
- `syntaxstring(s::Syntactical; kwargs...)::String`

See also [`Formula`](@ref), [`Connective`](@ref).
"""
abstract type Syntactical end

"""$(doc_syntaxstring)"""
function syntaxstring(s::Syntactical; kwargs...)::String
    return error("Please, provide method syntaxstring(::$(typeof(s)); kwargs...::$(typeof(kwargs))).")
end

function Base.show(io::IO, φ::Syntactical)
    print(io, "$(typeof(φ)): $(syntaxstring(φ))")
end

############################################################################################
#### Connective ############################################################################
############################################################################################

"""
    abstract type Connective <: Syntactical end

Abstract type for [logical connectives](https://en.wikipedia.org/wiki/Logical_connective),
that are used to express non-atomic statements;
for example, CONJUNCTION, DISJUNCTION, NEGATION and IMPLICATION (stylized as ∧, ∨, ¬ and →).

# Interface
- `arity(::Connective)::Int`
- `iscommutative(::Connective)::Bool`
- `precedence(::Connective)::Int`
- `associativity(::Connective)::Symbol`
- `collatetruth(::Connective, ::NTuple{N,Truth})::Truth`
- `simplify(::Connective, ::NTuple{N,SyntaxTree})::SyntaxTree`
- `dual(s::Connective)::Connective`
- `hasdual(s::Connective)::Bool`
- See also [`Syntactical`](@ref)

# Implementation

When implementing a new type `C` for a connective, please define its `arity`.
For example, with a binary operator (e.g., ∨ or ∧):

    arity(::C) = 2

When implementing a new type `C` for a *commutative* connective with arity higher than 1,
please provide a method `iscommutative(::C)`. This can speed up model checking operations.

When implementing a custom binary connective, one can override the default `precedence` and
`associativity` (see [here](https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity).
If the custom connective is a `NamedConnective` and renders as something considered as a
`math symbol` (for example, `⊙`, see https://stackoverflow.com/a/60321302/5646732),
by the Julia parser, `Base.operator_precedence`
and `Base.operator_associativity` are used to define these behaviors, and
you might want to avoid providing these methods at all.

The semantics of a *propositional* connective can be specified via `collatetruth` (see example below);
in principle, the definition can rely on the partial order between truth values
(specified via `precedes`).

Here is an example of a custom implementation of the xor (⊻) Boolean operator.
```julia
import SoleLogics: arity, iscommutative, collatetruth
const ⊻ = SoleLogics.NamedConnective{:⊻}()
SoleLogics.arity(::typeof(⊻)) = 2
SoleLogics.iscommutative(::typeof(⊻)) = true
SoleLogics.collatetruth(::typeof(⊻), (t1, t2)::NTuple{N,T where T<:BooleanTruth}) where {N} = (count(istop, (t1, t2)) == 1)
```
Note that `collatetruth` must be defined at least for some truth value types `T` via methods
accepting an `NTuple{arity,T}` as a second argument.

To make the operator work with incomplete interpretations (e.g., when the `Truth` value
for an atom is not known), simplification rules for `NTuple{arity,T where T<:Formula}`s
should be provided via methods for `simplify`.
For example, these rules suffice for simplifying xors between `TOP/`BOT`s, and other formulas:
```julia
import SoleLogics: simplify
simplify(::typeof(⊻), (t1, t2)::Tuple{BooleanTruth,BooleanTruth}) = istop(t1) == istop(t2) ? BOT : TOP
simplify(::typeof(⊻), (t1, t2)::Tuple{BooleanTruth,Formula}) = istop(t1) ? ¬t2 : t2
simplify(::typeof(⊻), (t1, t2)::Tuple{Formula,BooleanTruth}) = istop(t2) ? ¬t1 : t1
```

Beware of dispatch ambiguities!

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
isnullary(c) = iszero(arity(c))
isunary(c)   = arity(c) == 1
isbinary(c)  = arity(c) == 2

"""$(doc_iscommutative)"""
function iscommutative(c::Connective)
    # Unless otherwise specified
    return arity(c) <= 1 ? true :
        error("Please, provide method iscommutative(::$(typeof(c))).")
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
`Truth` values), `SyntaxStructure`s (for example, `SyntaxTree`s and
`LeftmostLinearForm`s) and `TruthTable`s (
enriched representation, which associates a syntactic structure with
additional [memoization](https://en.wikipedia.org/wiki/Memoization) structures,
which can save computational time upon
[model checking](https://en.wikipedia.org/wiki/Model_checking)).

Any formula can be converted into its [`SyntaxTree`](@ref)
representation via [`tree`](@ref); its [`height`](@ref) can be computed,
and it can be queried for its syntax [`tokens`](@ref), [`atoms`](@ref), etc...
It can be parsed from its [`syntaxstring`](@ref) representation via [`parseformula`](@ref).

# Interface
- `tree(φ::Formula)::SyntaxTree`
- `composeformulas(c::Connective, φs::NTuple{N,F})::F where {N,F<:Formula}`
- See also [`Syntactical`](@ref)

# Utility functions (requiring a walk of the tree)
- `Base.in(tok::SyntaxToken, φ::Formula)::Bool`

- `height(φ::Formula)::Int`
- `tokens(φ::Formula)::Vector{SyntaxToken}`
- `atoms(φ::Formula)::Vector{Atom}`
- `truths(φ::Formula)::Vector{Truth}`
- `leaves(φ::Formula)::Vector{SyntaxLeaf}`
- `connectives(φ::Formula)::Vector{Connective}`
- `operators(φ::Formula)::Vector{Operator}`
- `ntokens(φ::Formula)::Int`
- `natoms(φ::Formula)::Int`
- `ntruths(φ::Formula)::Int`
- `nleaves(φ::Formula)::Int`
- `nconnectives(φ::Formula)::Int`
- `noperators(φ::Formula)::Int`

- `appendtokens!(v::Vector, φ::Formula)`
- `appendatoms!(v::Vector, φ::Formula)`
- `appendtruths!(v::Vector, φ::Formula)`
- `appendleaves!(v::Vector, φ::Formula)`
- `appendconnectives!(v::Vector, φ::Formula)`
- `appendoperators!(v::Vector, φ::Formula)`

See also [`tree`](@ref), [`SyntaxStructure`](@ref), [`SyntaxLeaf`](@ref).
"""
abstract type Formula <: Syntactical end

"""$(doc_tree)"""
function tree(φ::Formula)
    return error("Please, provide method tree(::$(typeof(φ)))::SyntaxTree.")
end

"""$(doc_composeformulas)"""
function composeformulas(c::Connective, φs::NTuple{N,F})::F where {N,F<:Formula}
    return error("Please, provide method " *
        "composeformulas(c::Connective, φs::NTuple{N,$(F)}) where {N}.")
end

# Helper (?)
function composeformulas(c::Connective, φs::Vararg{Formula,N}) where {N}
    return composeformulas(c, φs)
end

"""
    Base.in(tok::SyntaxToken, φ::Formula)::Bool

Return whether a syntax token appears in a formula.

See also [`Formula`](@ref), [`SyntaxToken`](@ref).
"""
function Base.in(tok, φ::Formula)::Bool
    return Base.in(tok, tree(φ))
end

"""
    height(φ::Formula)::Int

Return the height of a formula, in its syntax tree representation.

See also [`SyntaxTree`](@ref).
"""
function height(φ::Formula)::Int
    return height(tree(φ))
end

"""
    tokens(φ::Formula)::Vector{SyntaxToken}
    atoms(φ::Formula)::Vector{Atom}
    truths(φ::Formula)::Vector{Truth}
    leaves(φ::Formula)::Vector{SyntaxLeaf}
    connectives(φ::Formula)::Vector{Connective}
    operators(φ::Formula)::Vector{Operator}
    
    ntokens(φ::Formula)::Integer
    natoms(φ::Formula)::Integer
    ...

    appendtokens!(v::Vector, φ::Formula)::Vector
    appendatoms!(v::Vector, φ::Formula)::Vector
    ...

Return the list/number of (non-unique) `SyntaxToken`s, `Atom`s, etc...
appearing in a formula. Inplace versions of these methods
append the tokens to an existing vector, and are faster, e.g., for collecting
all tokens from many formulas at once.

See also [`Formula`](@ref), [`SyntaxToken`](@ref).
"""
tokens(φ::Formula)::Vector = tokens(tree(φ))
"See docstring for [`tokens`](@ref)."
atoms(φ::Formula)::Vector = atoms(tree(φ))
"See docstring for [`tokens`](@ref)."
truths(φ::Formula)::Vector = truths(tree(φ))
"See docstring for [`tokens`](@ref)."
leaves(φ::Formula)::Vector = leaves(tree(φ))
"See docstring for [`tokens`](@ref)."
connectives(φ::Formula)::Vector = connectives(tree(φ))
"See docstring for [`tokens`](@ref)."
operators(φ::Formula)::Vector = operators(tree(φ))
"See docstring for [`tokens`](@ref)."
ntokens(φ::Formula)::Int = ntokens(tree(φ))
"See docstring for [`tokens`](@ref)."
natoms(φ::Formula)::Int = natoms(tree(φ))
"See docstring for [`tokens`](@ref)."
ntruths(φ::Formula)::Int = ntruths(tree(φ));
"See docstring for [`tokens`](@ref)."
nleaves(φ::Formula)::Int = nleaves(tree(φ));
"See docstring for [`tokens`](@ref)."
nconnectives(φ::Formula)::Int = nconnectives(tree(φ));
"See docstring for [`tokens`](@ref)."
noperators(φ::Formula)::Int = noperators(tree(φ));
"See docstring for [`tokens`](@ref)."
appendtokens!(v::Vector, φ::Formula)::Vector = appendtokens!(v, tree(φ))
"See docstring for [`tokens`](@ref)."
appendatoms!(v::Vector, φ::Formula)::Vector = appendatoms!(v, tree(φ))
"See docstring for [`tokens`](@ref)."
appendtruths!(v::Vector, φ::Formula)::Vector = appendtruths!(v, tree(φ))
"See docstring for [`tokens`](@ref)."
appendleaves!(v::Vector, φ::Formula)::Vector = appendleaves!(v, tree(φ))
"See docstring for [`tokens`](@ref)."
appendconnectives!(v::Vector, φ::Formula)::Vector = appendconnectives!(v, tree(φ))
"See docstring for [`tokens`](@ref)."
appendoperators!(v::Vector, φ::Formula)::Vector = appendoperators!(v, tree(φ))


@inline function Base.isequal(φ1::Formula, φ2::Formula)
    Base.isequal(tree(φ1), tree(φ2))
end

@inline Base.hash(φ::Formula) = Base.hash(tree(φ))

function syntaxstring(φ::Formula; kwargs...)
    syntaxstring(tree(φ); kwargs...)
end



############################################################################################
#### SyntaxStructure ###############################################################
############################################################################################

"""
    abstract type SyntaxStructure <: Formula end

Abstract type for the purely-syntactic component of a logical formula (e.g.,
no fancy memoization structure associated). The typical representation is the
[`SyntaxTree`](@ref), however, different implementations can cover specific syntactic forms
(e.g., [conjunctive](https://en.wikipedia.org/wiki/Conjunctive_normal_form) or
[disjunctive](https://en.wikipedia.org/wiki/Disjunctive_normal_form) normal forms).

# Interface
- See also [`Formula`](@ref)

See also [`Formula`](@ref), [`AbstractLogic`](@ref), [`SyntaxTree`](@ref),
[`tree`](@ref).
"""
abstract type SyntaxStructure <: Formula end

function composeformulas(c::Connective, φs::NTuple{N,SyntaxStructure}) where {N}
    return composeformulas(c, tree.(φs))
end

############################################################################################
#### SyntaxTree ############################################################################
############################################################################################

using AbstractTrees
import AbstractTrees: children, printnode

"""
    abstract type SyntaxTree <: SyntaxStructure end

Abstract type for
[syntax trees](https://en.wikipedia.org/wiki/Abstract_syntax_tree); that is,
syntax leaves (see `SyntaxLeaf`, such as `Truth` values and `Atom`s),
and their composition via `Connective`s (i.e., `SyntaxBranch`).

Note that `SyntaxTree` are *ranked trees*,
and (should) implement `AbstractTrees` interface.

# Interface
- `children(φ::SyntaxTree)::NTuple{N,SyntaxTree} where N`
- `token(φ::SyntaxTree)::Connective`
- See also [`SyntaxStructure`](@ref)

# Utility functions
- `arity(φ::SyntaxTree)::Int`
- `composeformulas(c::Connective, φs::NTuple{N,SyntaxTree})`

# Other utility functions requiring a tree walk
- `Base.in(tok::SyntaxToken, φ::SyntaxTree)::Bool`

- `height(φ::SyntaxTree)::Int`
- `tokens(φ::SyntaxTree)::Vector{SyntaxToken}`
- `atoms(φ::SyntaxTree)::Vector{Atom}`
- `truths(φ::SyntaxTree)::Vector{Truth}`
- `leaves(φ::SyntaxTree)::Vector{SyntaxLeaf}`
- `connectives(φ::SyntaxTree)::Vector{Connective}`
- `operators(φ::SyntaxTree)::Vector{Operator}`
- `ntokens(φ::SyntaxTree)::Int`
- `natoms(φ::SyntaxTree)::Int`
- `ntruths(φ::SyntaxTree)::Int`
- `nleaves(φ::SyntaxTree)::Int`
- `nconnectives(φ::SyntaxTree)::Int`
- `noperators(φ::SyntaxTree)::Int`

- `appendtokens!(v::Vector, φ::SyntaxTree)`
- `appendatoms!(v::Vector, φ::SyntaxTree)`
- `appendtruths!(v::Vector, φ::SyntaxTree)`
- `appendleaves!(v::Vector, φ::SyntaxTree)`
- `appendconnectives!(v::Vector, φ::SyntaxTree)`
- `appendoperators!(v::Vector, φ::SyntaxTree)`

See also [`SyntaxLeaf`](@ref), [`SyntaxBranch`](@ref),
[`SyntaxStructure`](@ref), [`Formula`](@ref).
"""
abstract type SyntaxTree <: SyntaxStructure end

tree(φ::SyntaxTree) = φ

"""$(doc_syntaxtree_children)"""
function children(φ::SyntaxTree)
    return error("Please, provide method children(::$(typeof(φ))).")
end

AbstractTrees.printnode(io, φ::SyntaxTree) = print(io, syntaxstring(φ))


"""$(doc_syntaxtree_token)"""
function token(φ::SyntaxTree)
    return error("Please, provide method token(::$(typeof(φ))).")
end

arity(φ::SyntaxTree) = length(children(φ))

function height(φ::SyntaxTree)::Int
    ch = children(φ)
    return isempty(ch) ? 0 : 1 + maximum(height, ch)
end

function gather_tokens!(φ::SyntaxTree, out::Vector, pred::Function)::Vector
    foreach(x -> gather_tokens!(x, out, pred), children(φ))
    pred(token(φ)) && push!(out, token(φ))
    return out
end

function gather_tokens!(φ::SyntaxTree, out::Vector)::Vector
    foreach(x -> gather_tokens!(x, out), children(φ))
    push!(out, token(φ))
    return out
end
appendtokens!(v::Vector, φ::SyntaxTree)::Vector = gather_tokens!(φ, v)
appendatoms!(v::Vector, φ::SyntaxTree)::Vector = gather_tokens!(φ, v, x -> x isa AbstractAtom)
appendtruths!(v::Vector, φ::SyntaxTree)::Vector = gather_tokens!(φ, v, x -> x isa Truth)
appendleaves!(v::Vector, φ::SyntaxTree)::Vector = gather_tokens!(φ, v, x -> x isa SyntaxLeaf)
appendconnectives!(v::Vector, φ::SyntaxTree)::Vector = gather_tokens!(φ, v, x -> x isa Connective)
appendoperators!(v::Vector, φ::SyntaxTree)::Vector = gather_tokens!(φ, v, x -> x isa Operator)

tokens(φ::SyntaxTree)::Vector = appendtokens!(SyntaxToken[], φ)
atoms(φ::SyntaxTree)::Vector = appendatoms!(Atom[], φ)
truths(φ::SyntaxTree)::Vector = appendtruths!(Truth[], φ)
leaves(φ::SyntaxTree)::Vector = appendleaves!(SyntaxLeaf[], φ)
connectives(φ::SyntaxTree)::Vector = appendconnectives!(Connective[], φ)
operators(φ::SyntaxTree)::Vector = appendoperators!(Operator[], φ)

function ntokens(φ::SyntaxTree)::Int
    ch = children(φ)
    return isempty(ch) ? 1 : 1 + sum(ntokens, ch)
end
function natoms(φ::SyntaxTree)::Int
    ch = children(φ)
    return isempty(ch) ? Int(token(φ) isa AbstractAtom) : sum(natoms, ch)
end
function ntruths(φ::SyntaxTree)::Int
    ch = children(φ)
    return isempty(ch) ? Int(token(φ) isa Truth) : sum(ntruths, ch)
end
function nleaves(φ::SyntaxTree)::Int
    ch = children(φ)
    return isempty(ch) ? Int(token(φ) isa SyntaxLeaf ? 1 : 0) : sum(nleaves, ch)
end
function nconnectives(φ::SyntaxTree)::Int
    c = token(φ) isa Connective ? 1 : 0
    ch = children(φ)
    return isempty(ch) ? c : c + sum(nconnectives, ch)
end
function noperators(φ::SyntaxTree)::Int
    op = token(φ) isa Operator ? 1 : 0
    ch = children(φ)
    return isempty(ch) ? op : op + sum(noperators, ch)
end

function Base.isequal(a::SyntaxTree, b::SyntaxTree)
    if iszero(arity(a)) && iszero(arity(b))
        return a == b
    else
        return (Base.isequal(token(a), token(b)) &&
                all(((c1,c2),)->Base.isequal(c1, c2), zip(children(a), children(b))))
    end
end

@inline Base.hash(φ::SyntaxTree) = Base.hash(token(φ), Base.hash(children(φ)))

function composeformulas(c::Connective, φs::NTuple{N,SyntaxTree}) where {N}
    return SyntaxBranch(c, φs)
end

function syntaxstring(φ::SyntaxTree; kwargs...)
    return error("Please, provide method syntaxstring(::$(typeof(φ)); kwargs...::$(typeof(kwargs))).")
end

# Syntax tree, the universal syntax structure representation,
# wins when promoted with syntax structures/tokens and syntax trees.
Base.promote_rule(::Type{<:SyntaxTree}, ::Type{<:SyntaxTree}) = SyntaxTree
Base.promote_rule(::Type{<:SyntaxStructure}, ::Type{S}) where {S<:SyntaxTree} = S
Base.promote_rule(::Type{S}, ::Type{<:SyntaxStructure}) where {S<:SyntaxTree} = S

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
    abstract type SyntaxLeaf <: SyntaxStructure end

An atomic logical element, like a `Truth` value or an `Atom`.
`SyntaxLeaf`s have `arity` equal to zero, meaning that they are not
allowed to have children in tree-like syntactic structures.

# Interface
- `syntaxstring(s::SyntaxLeaf; kwargs...)::String`
- `dual(s::SyntaxLeaf)::SyntaxLeaf`
- `hasdual(s::SyntaxLeaf)::Bool`

See also [`SyntaxStructure`](@ref),  [`arity`](@ref), [`SyntaxBranch`](@ref).
"""
abstract type SyntaxLeaf <: SyntaxTree end

@inline children(::SyntaxLeaf) = ()
@inline token(φ::SyntaxLeaf) = φ

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

"""See [`dual`](@ref)."""
hasdual(t::SyntaxToken) = false

############################################################################################
#### AbstractAtom ##########################################################################
############################################################################################

"""
    abstract type AbstractAtom <: SyntaxLeaf end

An atom, sometimes called an atomic proposition,
propositional letter (or simply *letter*),
representing a fact which truth can be assessed on
a logical interpretation.

Atoms are nullary tokens (i.e, they are at the leaves of a syntax tree);
note that their atoms cannot be `Atom`s.

# Interface
- `syntaxstring(s::AbstractAtom; kwargs...)::String`
- `dual(s::AbstractAtom)::AbstractAtom`
- `hasdual(s::AbstractAtom)::Bool`

See also [`AbstractInterpretation`](@ref), [`atoms`](@ref), [`check`](@ref),
[`SyntaxToken`](@ref).
"""
abstract type AbstractAtom <: SyntaxLeaf end

############################################################################################
#### AbstractSyntaxBranch ##################################################################
############################################################################################

"""
    abstract type AbstractSyntaxBranch <: SyntaxTree end

An internal node of a syntax tree encoding a logical formula.
Such a node holds a syntax `token` (a `Connective`),
and has as many children as the `arity` of the token.

# Interface
- `children(φ::SyntaxTree)::NTuple{N,SyntaxTree} where N`
- `token(φ::SyntaxTree)::Connective`
- See also [`Syntactical`](@ref)

See also
[`token`](@ref), [`children`](@ref),
[`arity`](@ref),
[`Connective`](@ref),
[`height`](@ref),
[`atoms`](@ref), [`natoms`](@ref),
[`operators`](@ref), [`noperators`](@ref),
[`tokens`](@ref), [`ntokens`](@ref),
"""
abstract type AbstractSyntaxBranch <: SyntaxTree end

function syntaxstring(
    φ::AbstractSyntaxBranch;
    function_notation = false,
    remove_redundant_parentheses = true,
    parenthesize_atoms = !remove_redundant_parentheses,
    parenthesization_level = 1,
    parenthesize_commutatives = false,
    kwargs...,
)::String
    ch_kwargs = merge((; kwargs...),
        (;
            function_notation = function_notation,
            remove_redundant_parentheses = remove_redundant_parentheses,
            parenthesize_atoms = parenthesize_atoms,
            parenthesization_level = parenthesization_level,
            parenthesize_commutatives = parenthesize_commutatives,
        ),)

    # Parenthesization rules for binary operators in infix notation
    function _infix_mode(
        ptok::SyntaxToken,
        ch::SyntaxTree,
        childtype::Symbol,
    )
        chtok = token(ch)
        chtokstring = syntaxstring(ch; ch_kwargs...)

        parenthesize = begin
            if !remove_redundant_parentheses
                true
            elseif iszero(arity(chtok))
                false
            elseif arity(chtok) == 2 # My child is infix
                tprec = precedence(ptok)
                chprec = precedence(chtok)
                if ptok == chtok
                    if !parenthesize_commutatives && iscommutative(ptok)
                        false
                    elseif associativity(ptok) == childtype
                        # `a → b → c = a → (b → c)`
                        # or
                        # `a ∧ b ∧ c = (a ∧ b) ∧ c`
                        false
                    else
                        true
                    end
                elseif tprec == chprec # Read left to right
                    if childtype == :left
                        false
                    elseif childtype == :right
                        true
                    else
                        error("Unexpected precedence: $(childtype).")
                    end
                elseif tprec < chprec
                    (chprec - tprec <= parenthesization_level)
                elseif tprec > chprec
                    # # 1st condition, before "||" -> "◊¬p ∧ ¬q" instead of "(◊¬p) ∧ (¬q)"
                    # # 2nd condition, after  "||" -> "(q → p) → ¬q" instead of "q → p → ¬q" <- Not sure: wrong?
                    # # 3nd condition
                    true
                end
            else
                false
            end
        end
        lpar, rpar = parenthesize ? ["(", ")"] : ["", ""]
        lpar * chtokstring * rpar
    end

    tok = token(φ)
    tokstr = syntaxstring(tok; ch_kwargs...)

    if arity(tok) == 2 && !function_notation
        # Infix notation for binary operators
        _infix_mode(tok, children(φ)[1], :left) *
        " " * tokstr * " " *
        _infix_mode(tok, children(φ)[2], :right)
    else
        # Function notation
        lpar, rpar = "(", ")"
        # TODO this is very dirty...
        ch = token(only(children(φ)))
        parenthesize = true
        if !function_notation && arity(tok) == 1 && (arity(ch) == 1 || (ch isa AbstractAtom))
            # When not in function notation, print "¬p" instead of "¬(p)";
            # note that "◊((p ∧ q) → s)" must not be simplified as "◊(p ∧ q) → s".
            parenthesize = false
        end
        
        if parenthesize
            lpar, rpar = "", ""
        end

        tokstr * lpar *
        join(
            [begin
                _ch_kwargs =
                    if (c isa AbstractAtom && parenthesize_atoms)
                        merge(ch_kwargs, (; parenthesize_atoms = false))
                    else
                        ch_kwargs
                    end
                syntaxstring(c; ch_kwargs...)
            end for c in children(φ)], ", ") * rpar
    end
end

function Base.in(tok::SyntaxToken, tree::AbstractSyntaxBranch)::Bool
    return tok == token(tree) || any([Base.in(tok, c) for c in children(tree)])
end

function Base.in(tok::SyntaxToken, φ::SyntaxLeaf)::Bool
    return tok == φ
end

############################################################################################
#### Truth #################################################################################
############################################################################################

"""
    abstract type Truth <: SyntaxLeaf end

Abstract type for syntax leaves representing values of a
[lattice algebra](https://en.wikipedia.org/wiki/Lattice_(order)).
In Boolean logic, the two [`BooleanTruth`](@ref) values TOP (⊤) and BOT (⊥) are used.

See also [`BooleanTruth`](@ref).

# Interface
- `syntaxstring(s::Truth; kwargs...)::String`
- `dual(s::Truth)::Truth`
- `hasdual(s::Truth)::Bool`
- `istop(t::Truth)::Bool`
- `isbot(t::Truth)::Bool`
- `precedes(t1::Truth, t2::Truth)::Bool`
- `truthmeet(t1::Truth, t2::Truth)::Truth`
- `truthjoin(t1::Truth, t2::Truth)::Truth`

# Implementation
A [three-valued algebra](https://en.wikipedia.org/wiki/Three-valued_logic),
that is, an algebra with three truth values
(top, bottom and *unknown*),
can be based on the following `Truth` value definitions:

```julia
import SoleLogics: precedes

abstract type ThreeVTruth <: Truth end

struct ThreeTop <: ThreeVTruth end
const ⫪ = ThreeTop() # Note that ⊤ is already use to indicate BooleanTruth's top.
syntaxstring(::ThreeTop; kwargs...) = "⫪"

struct ThreeBot <: ThreeVTruth end
const ⫫ = ThreeBot() # Note that ⊥ is already use to indicate BooleanTruth's top.
syntaxstring(::ThreeBot; kwargs...) = "⫫"

struct ThreeUnknown <: ThreeVTruth end
const υ = ThreeUnknown()
syntaxstring(::ThreeUnknown; kwargs...) = "υ"

istop(t::ThreeTop) = true
isbot(t::ThreeBot) = true

precedes(::ThreeBot, ::ThreeTop) = true
precedes(::ThreeBot, ::ThreeUnknown) = true
precedes(::ThreeUnknown, ::ThreeTop) = true
precedes(::ThreeTop, ::ThreeBot) = false
precedes(::ThreeUnknown, ::ThreeBot) = false
precedes(::ThreeTop, ::ThreeUnknown) = false
```

Note that `precedes` is used to define the (partial) order between `Truth` values.

See also [`Connective`](@ref), [`BooleanTruth`](@ref).
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
    precedes(t1::Truth, t2::Truth)::Bool

Encodes the order relation (also denoted as `≺`) between truth values.

# Examples
```
julia> using SoleLogics


julia> SoleLogics.precedes(⊥, ⊤)
true
```
"""
function precedes(t1::Truth, t2::Truth)::Bool
    if Base.isequal(t1, t2)
        return false
    else
        return error("Please, provide method precedes(::$(typeof(t1)), ::$(typeof(t2))).")
    end
end

function truthmeet(t1::Truth, t2::Truth)::Truth
    error("Please, provide method truthmeet(::$(typeof(t1)), ::$(typeof(t2))).")
end
function truthjoin(t1::Truth, t2::Truth)::Truth
    error("Please, provide method truthjoin(::$(typeof(t1)), ::$(typeof(t2))).")
end

# Alias
"""Alias for [`precedes`](@ref)."""
const ≺ = precedes

# Fallback
function Base.:<(t1::Truth, t2::Truth)
    return precedes(t1, t2)
end

# Helper: some types can be specified to be converted to Truth types
function Base.convert(::Type{Truth}, t)::Truth
    return error("Cannot interpret value $t of type ($(typeof(t))) as Truth.")
end

# Helpers
Base.min(t1::Truth, t2::Truth) = truthmeet(t1, t2)
Base.max(t1::Truth, t2::Truth) = truthjoin(t1, t2)
Base.isless(t1::Truth, t2::Truth) = precedes(t1, t2)

# Fallback
Base.convert(::Type{Truth}, t::Truth) = t

# Helper: composeformulas actually works for operators as well
composeformulas(c::Truth, ::Tuple{}) = c

# Note: Extend istop to formulas. TODO find correct place for this.
function istop(φ::Formula)
    false
end

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
```julia-repl
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

function (op::Operator)(φs::NTuple{N,Formula}) where {N}
    if arity(op) == 2 && length(φs) > arity(op)
        if associativity(op) == :right
            φs = (φs[1], op(φs[2:end]))
        else
            φs = (op(φs[1:end-1]), φs[end])
        end
    end

    if SyntaxStructure <: typejoin(typeof.(φs)...)
        φs = Base.promote(φs...)
    end
    return composeformulas(op, φs)
end

(c::Truth)(::Tuple{}) = c
