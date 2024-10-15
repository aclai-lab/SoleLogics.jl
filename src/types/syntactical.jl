#=
    Syntactical Types Hierarchy

    Syntactical
    ├── AbstractConnective (e.g, ∧, ¬, □, ◊, ⟨G⟩)
    └── Formula
        └── SyntaxStructure
            └── SyntaxTree
                ├── SyntaxLeaf
                │   ├── AbstractAtom (e.g., p)
                │   └── Truth (e.g, ⊤, ⊥)
                └── AbstractSyntaxBranch (e.g., p ∧ q)

    Also:
    const Operator = Union{AbstractConnective,Truth}
    const SyntaxToken = Union{AbstractConnective,SyntaxLeaf}
=#

include("docstrings.jl")

############################################################################################
#### Syntax Base ###########################################################################
############################################################################################
"""
    abstract type Syntactical end

Master abstract type for all syntactical objects (e.g., formulas, connectives).

# Interface
- `syntaxstring(s::Syntactical; kwargs...)::String`

See also [`Formula`](@ref), [`AbstractConnective`](@ref).
"""
abstract type Syntactical end

"""$(doc_syntaxstring)"""
function syntaxstring(s::Syntactical; kwargs...)::String
    return error("Please, provide method syntaxstring(::$(typeof(s)); kwargs...).")
end

function Base.show(io::IO, φ::Syntactical)
    # print(io, "$(typeof(φ))\nsyntaxstring: $(syntaxstring(φ))")
    print(io, "$(typeof(φ)) with syntaxstring: $(syntaxstring(φ))")
end

############################################################################################
#### AbstractConnective ############################################################################
############################################################################################

"""
    abstract type AbstractConnective <: Syntactical end

Abstract type for [logical connectives](https://en.wikipedia.org/wiki/Logical_connective),
that are used to express non-atomic statements;
for example, CONJUNCTION, DISJUNCTION, NEGATION and IMPLICATION (stylized as ∧, ∨, ¬ and →).

# Interface
- `arity(::AbstractConnective)::Int`
- `iscommutative(::AbstractConnective)::Bool`
- `precedence(::AbstractConnective)::Int`
- `associativity(::AbstractConnective)::Symbol`
- `collatetruth(::AbstractConnective, ::NTuple{N,Truth})::Truth`
- `simplify(::AbstractConnective, ::NTuple{N,Truth})::SyntaxTree`
- `dual(s::AbstractConnective)::AbstractConnective`
- `hasdual(s::AbstractConnective)::Bool`
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
abstract type AbstractConnective <: Syntactical end

"""$(doc_arity)"""
arity(c::AbstractConnective)::Integer = error("Please, provide method arity(::$(typeof(c))).")

# Helpers
isnullary(c) = arity(c) == 0
isunary(c)   = arity(c) == 1
isbinary(c)  = arity(c) == 2
isternary(c) = arity(c) == 3

"""$(doc_iscommutative)"""
function iscommutative(c::AbstractConnective)
    # Unless otherwise specified
    return arity(c) <= 1 ? true :
        error("Please, provide method iscommutative(::$(typeof(c))).")
end

"""$(doc_precedence)"""
function precedence(c::AbstractConnective)
    return error("Please, provide method precedence(c::$(typeof(c))).")
end

"""$(doc_associativity)"""
associativity(::AbstractConnective) = :left

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
- `composeformulas(c::AbstractConnective, φs::NTuple{N,F})::F where {N,F<:Formula}`
- See also [`Syntactical`](@ref)

# Utility functions (requiring a walk of the tree)
- `Base.in(tok::SyntaxToken, φ::Formula)::Bool`
- `height(φ::Formula)::Int`
- `tokens(φ::Formula)::AbstractVector{<:SyntaxToken}`
- `atoms(φ::Formula)::AbstractVector{<:AbstractAtom}`
- `truths(φ::Formula)::AbstractVector{<:Truth}`
- `leaves(φ::Formula)::AbstractVector{<:SyntaxLeaf}`
- `connectives(φ::Formula)::AbstractVector{<:AbstractConnective}`
- `operators(φ::Formula)::AbstractVector{<:Operator}`
- `ntokens(φ::Formula)::Int`
- `natoms(φ::Formula)::Int`
- `ntruths(φ::Formula)::Int`
- `nleaves(φ::Formula)::Int`
- `nconnectives(φ::Formula)::Int`
- `noperators(φ::Formula)::Int`

See also [`tree`](@ref), [`SyntaxStructure`](@ref), [`SyntaxLeaf`](@ref).
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
    height(φ::Formula)::Int

Return the height of a formula, in its syntax tree representation.

See also [`SyntaxTree`](@ref).
"""
function height(φ::Formula)::Int
    return height(tree(φ))
end

"""$(doc_tokopprop)"""
function tokens(φ::Formula) # ::AbstractVector{<:SyntaxToken}
    return tokens(tree(φ))
end
"""$(doc_tokopprop)"""
function atoms(φ::Formula) # ::AbstractVector{<:AbstractAtom}
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
function connectives(φ::Formula) # ::AbstractVector{<:AbstractConnective}
    return connectives(tree(φ))
end
"""$(doc_tokopprop)"""
function operators(φ::Formula) # ::AbstractVector{<:Operator}
    return operators(tree(φ))
end
"""$(doc_tokopprop)"""
function ntokens(φ::Formula)::Int
    return ntokens(tree(φ))
end
"""$(doc_tokopprop)"""
function natoms(φ::Formula)::Int
    return natoms(tree(φ))
end
"""$(doc_tokopprop)"""
function ntruths(φ::Formula)::Int
    return ntruths(tree(φ));
end
"""$(doc_tokopprop)"""
function nleaves(φ::Formula)::Int
    return nleaves(tree(φ));
end
"""$(doc_tokopprop)"""
function nconnectives(φ::Formula)::Int
    return nconnectives(tree(φ));
end
"""$(doc_tokopprop)"""
function noperators(φ::Formula)::Int
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
function composeformulas(c::AbstractConnective, φs::NTuple{N,F})::F where {N,F<:Formula}
    return error("Please, provide method " *
        "composeformulas(c::AbstractConnective, φs::NTuple{N,$(F)}) where {N}.")
end

# Helper (?)
# Note: don't type the output as F
function composeformulas(c::AbstractConnective, φs::Vararg{Formula,N}) where {N}
    return composeformulas(c, φs)
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

function composeformulas(c::AbstractConnective, φs::NTuple{N,SyntaxStructure}) where {N}
    return composeformulas(c, tree.(φs))
end

############################################################################################
#### SyntaxTree ############################################################################
############################################################################################

import AbstractTrees: children

"""
    abstract type SyntaxTree <: SyntaxStructure end

Abstract type for
[syntax trees](https://en.wikipedia.org/wiki/Abstract_syntax_tree); that is,
syntax leaves (see `SyntaxLeaf`, such as `Truth` values and `Atom`s),
and their composition via `AbstractConnective`s (i.e., `SyntaxBranch`).

Note that `SyntaxTree` are *ranked trees*,
and (should) implement `AbstractTrees` interface.

# Interface
- `children(φ::SyntaxTree)::NTuple{N,SyntaxTree} where N`
- `token(φ::SyntaxTree)::AbstractConnective`
- See also [`SyntaxStructure`](@ref)

# Utility functions
- `tokentype(φ::SyntaxTree)`
- `arity(φ::SyntaxTree)::Int`

# Other utility functions (requiring a walk of the tree)
- `Base.in(tok::SyntaxToken, φ::SyntaxTree)::Bool`
- `height(φ::SyntaxTree)::Int`
- `tokens(φ::SyntaxTree)::AbstractVector{<:SyntaxToken}`
- `atoms(φ::SyntaxTree)::AbstractVector{<:AbstractAtom}`
- `truths(φ::SyntaxTree)::AbstractVector{<:Truth}`
- `leaves(φ::SyntaxTree)::AbstractVector{<:SyntaxLeaf}`
- `connectives(φ::SyntaxTree)::AbstractVector{<:AbstractConnective}`
- `operators(φ::SyntaxTree)::AbstractVector{<:Operator}`
- `ntokens(φ::SyntaxTree)::Int`
- `natoms(φ::SyntaxTree)::Int`
- `ntruths(φ::SyntaxTree)::Int`
- `nleaves(φ::SyntaxTree)::Int`
- `nconnectives(φ::SyntaxTree)::Int`
- `noperators(φ::SyntaxTree)::Int`
- `tokenstype(φ::SyntaxTree)`
- `atomstype(φ::SyntaxTree)`
- `truthstype(φ::SyntaxTree)`
- `leavestype(φ::SyntaxTree)`
- `connectivestype(φ::SyntaxTree)`
- `operatorstype(φ::SyntaxTree)`
- `composeformulas(c::AbstractConnective, φs::NTuple{N,SyntaxTree})`

See also [`SyntaxLeaf`](@ref), [`SyntaxBranch`](@ref),
[`SyntaxStructure`](@ref), [`Formula`](@ref).
"""
abstract type SyntaxTree <: SyntaxStructure end

tree(φ::SyntaxTree) = φ

"""$(doc_syntaxtree_children)"""
function children(φ::SyntaxTree)
    return error("Please, provide method children(::$(typeof(φ))).")
end

"""$(doc_syntaxtree_token)"""
function token(φ::SyntaxTree)
    return error("Please, provide method token(::$(typeof(φ))).")
end

arity(φ::SyntaxTree) = length(children(φ))

function height(φ::SyntaxTree)::Int
    return length(children(φ)) == 0 ? 0 : 1 + maximum(height(c) for c in children(φ))
end
function tokens(φ::SyntaxTree) # ::AbstractVector{<:SyntaxToken}
    return SyntaxToken[vcat(tokens.(children(φ))...)..., token(φ)]
end
function atoms(φ::SyntaxTree) # ::AbstractVector{<:AbstractAtom}
    a = token(φ) isa AbstractAtom ? [token(φ)] : []
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
function connectives(φ::SyntaxTree) # ::AbstractVector{<:AbstractConnective}
    c = token(φ) isa AbstractConnective ? [token(φ)] : []
    return AbstractConnective[vcat(connectives.(children(φ))...)..., c...]
end
function operators(φ::SyntaxTree) # ::AbstractVector{<:Operator}
    c = token(φ) isa Operator ? [token(φ)] : []
    return Operator[vcat(operators.(children(φ))...)..., c...]
end
function ntokens(φ::SyntaxTree)::Int
    return length(children(φ)) == 0 ? 1 : 1 + sum(ntokens(c) for c in children(φ))
end
function natoms(φ::SyntaxTree)::Int
    a = token(φ) isa AbstractAtom ? 1 : 0
    return length(children(φ)) == 0 ? a : a + sum(natoms(c) for c in children(φ))
end
function ntruths(φ::SyntaxTree)::Int
    t = token(φ) isa Truth ? 1 : 0
    return length(children(φ)) == 0 ? t : t + sum(ntruths(c) for c in children(φ))
end
function nleaves(φ::SyntaxTree)::Int
    op = token(φ) isa SyntaxLeaf ? 1 : 0
    return length(children(φ)) == 0 ? op : op + sum(nleaves(c) for c in children(φ))
end
function nconnectives(φ::SyntaxTree)::Int
    c = token(φ) isa AbstractConnective ? 1 : 0
    return length(children(φ)) == 0 ? c : c + sum(nconnectives(c) for c in children(φ))
end
function noperators(φ::SyntaxTree)::Int
    op = token(φ) isa Operator ? 1 : 0
    return length(children(φ)) == 0 ? op : op + sum(noperators(c) for c in children(φ))
end

function Base.isequal(a::SyntaxTree, b::SyntaxTree)
    if arity(a) == 0 && arity(b) == 0
        return a == b
    else
        return (Base.isequal(token(a), token(b)) &&
                all(((c1,c2),)->Base.isequal(c1, c2), zip(children(a), children(b))))
    end
end

Base.hash(φ::SyntaxTree) = Base.hash(token(φ), Base.hash(children(φ)))

# Helpers
tokentype(φ::SyntaxTree) = typeof(token(φ))
tokenstype(φ::SyntaxTree) = Union{tokentype(φ),tokenstype.(children(φ))...}
atomstype(φ::SyntaxTree) = typeintersect(AbstractAtom, tokenstype(φ))
truthstype(φ::SyntaxTree) = typeintersect(Truth, tokenstype(φ))
leavestype(φ::SyntaxTree) = typeintersect(SyntaxLeaf, tokenstype(φ))
connectivestype(φ::SyntaxTree) = typeintersect(AbstractConnective, tokenstype(φ))
operatorstype(φ::SyntaxTree) = typeintersect(Operator, tokenstype(φ))

function composeformulas(c::AbstractConnective, φs::NTuple{N,SyntaxTree}) where {N}
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
function SyntaxTree(c::AbstractConnective, φs::NTuple{N,SyntaxTree}) where {N}
    return composeformulas(c, φs)
end
function SyntaxTree(c::AbstractConnective, φs::Vararg{SyntaxTree,N}) where {N}
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

children(::SyntaxLeaf) = ()
token(φ::SyntaxLeaf) = φ

############################################################################################
#### SyntaxToken ###########################################################################
############################################################################################

"""
    const SyntaxToken = Union{AbstractConnective,SyntaxLeaf}

Union type for values wrapped in `SyntaxTree` nodes.

See also [`SyntaxTree`](@ref), [`SyntaxLeaf`](@ref), [`AbstractConnective`](@ref).
"""
const SyntaxToken = Union{AbstractConnective,SyntaxLeaf}

"""$(doc_dual)"""
dual(t::SyntaxToken) = error("Please, provide method dual(::$(typeof(t))).")

"""See [`dual`](@ref)"""
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
Such a node holds a syntax `token` (a `AbstractConnective`),
and has as many children as the `arity` of the token.

# Interface
- `children(φ::SyntaxTree)::NTuple{N,SyntaxTree} where N`
- `token(φ::SyntaxTree)::AbstractConnective`
- See also [`Syntactical`](@ref)

See also
[`token`](@ref), [`children`](@ref),
[`arity`](@ref),
[`AbstractConnective`](@ref),
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
    function _binary_infix_syntaxstring(
            ptok::SyntaxToken,
            ch::SyntaxTree,
            childtype::Symbol,
    )
        chtok = token(ch)
        chtokstring = syntaxstring(ch; ch_kwargs...)

        parenthesize = begin
            if !remove_redundant_parentheses
                true
            elseif arity(chtok) == 0
                if chtok isa Atom && parenthesize_atoms
                    true
                else
                    false
                end
            elseif arity(chtok) == 2 # My child is infix
                tprec = precedence(ptok)
                chprec = precedence(chtok)
                if ptok == chtok
                    if !parenthesize_commutatives && iscommutative(ptok)
                        false
                    elseif associativity(ptok) == :left && childtype == :left
                        false # a ∧ b ∧ c = (a ∧ b) ∧ c
                    elseif associativity(ptok) == :right && childtype == :right
                        false # a → b → c = a → (b → c)
                    else
                        true
                    end
                elseif tprec == chprec # Read left to right
                    if childtype == :left
                        false
                    elseif childtype == :right
                        true
                    end
                elseif tprec < chprec
                    if chprec - tprec <= parenthesization_level
                        true
                    else
                        false
                    end
                elseif tprec > chprec
                    true
                    # # 1st condition, before "||" -> "◊¬p ∧ ¬q" instead of "(◊¬p) ∧ (¬q)"
                    # # 2nd condition, after  "||" -> "(q → p) → ¬q" instead of "q → p → ¬q" <- Not sure: wrong?
                    # # 3nd condition
                    # @show !(tprec <= chprec)
                    # @show ((chprec-tprec) <= parenthesization_level)
                    # @show tprec <= chprec
                    # @show chprec-tprec
                    # @show chprec-tprec <= parenthesization_level
                    # @show iscommutative(ptok)
                    # @show ptok, chtok, iscommutative(ptok), tprec, chprec
                    # @show ((!iscommutative(ptok) || ptok != chtok) && (tprec > chprec))
                    # @show (!iscommutative(ptok) && tprec <= chprec)

                    # if (
                    #     (tprec > chprec  && (!iscommutative(ptok) || ptok != chtok)) || # 1
                    #     (tprec <= chprec && (!iscommutative(ptok))) # 2
                    # )
                    #     true
                    # else
                    #     false
                    # end
                end
            else
                false
            end
        end
        lpar, rpar = parenthesize ? ["(", ")"] : ["", ""]
        return "$(lpar)$(chtokstring)$(rpar)"
    end

    tok = token(φ)
    tokstr = syntaxstring(tok; ch_kwargs...)

    if arity(tok) == 0
        # Leaf nodes parenthesization is parent's respsonsability
        return tokstr
    elseif arity(tok) == 2 && !function_notation
        # Infix notation for binary operators

        "$(_binary_infix_syntaxstring(tok, children(φ)[1], :left)) " *
        "$tokstr $(_binary_infix_syntaxstring(tok, children(φ)[2], :right))"
    else
        # Infix notation with arity != 2, or function notation
        lpar, rpar = "(", ")"
        ch = token(children(φ)[1])
        charity = arity(ch)
        if !function_notation && arity(tok) == 1 &&
           (charity == 1 || (ch isa Atom && !parenthesize_atoms))
            # When not in function notation, print "¬p" instead of "¬(p)";
            # note that "◊((p ∧ q) → s)" must not be simplified as "◊(p ∧ q) → s".
            lpar, rpar = "", ""
        end

        if length(children(φ)) == 0
            tokstr
        else
            tokstr * "$(lpar)" *
            join(
                [syntaxstring(c; ch_kwargs...) for c in children(φ)], ", ",) * "$(rpar)"
        end
    end
end

"""$(doc_formula_basein)"""
function Base.in(tok::SyntaxToken, φ::Formula)::Bool
    return Base.in(tok, tree(φ))
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

See also [`AbstractConnective`](@ref), [`BooleanTruth`](@ref).
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
    const Operator = Union{AbstractConnective,Truth}

Union type for logical constants of any ariety (zero for `Truth` values, non-zero for
`AbstractConnective`s).

See also [`AbstractConnective`](@ref), [`Truth`](@ref).
"""
const Operator = Union{AbstractConnective,Truth}

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

# function (op::Operator)(φs::NTuple{N, F}) where {N,F<:Formula}
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
