import Base: convert, promote_rule, _promote
import Base: eltype, in, getindex, isiterable, iterate, IteratorSize, length, isequal, hash

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
    │   └── AbstractMemoFormula
    │       ├── TruthTable
    │       └── ...
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
function syntaxstring(φ::Syntactical; kwargs...)::String
    return error("Please, provide method syntaxstring(::$(typeof(φ)); kwargs...).")
end

"""$(doc_syntaxstring)"""
syntaxstring(value; kwargs...) = string(value)

############################################################################################
#### Connective ############################################################################
############################################################################################

"""
    abstract type Connective <: Syntactical end

Abstract type for [logical connectives](https://en.wikipedia.org/wiki/Logical_connective),
that are used to express non-atomic statements;
for example, CONJUNCTION, DISJUNCTION and IMPLICATION (stylized as ∧, ∨ and →).

# Implementation

When implementing a custom connective, one can override the default `precedence` and
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

"""$(doc_iscommutative)"""
function iscommutative(c::Connective)
    return arity(c) <= 1
end

"""$(doc_precedence)"""
function precedence(c::Connective)
    return error("Please, provide method precedence(c::$(typeof(c))).")
end

"""$(doc_arity)"""
arity(c::Connective)::Integer = error("Please, provide method arity(::$(typeof(c))).")

isnullary(c) = arity(c) == 0
isunary(c)   = arity(c) == 1
isbinary(c)  = arity(c) == 2
isternary(c) = arity(c) == 3

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
`LeftmostLinearForm`s) and `AbstractMemoFormula`s (for example, `TruthTable`s).

See also [`AbstractSyntaxStructure`](@ref), [`SyntaxLeaf`](@ref),
[`AbstractMemoFormula`](@ref).
"""
abstract type Formula <: Syntactical end

"""
    tree(f::Formula)::SyntaxTree

Return the `SyntaxTree` representation of a formula;
note that this is equivalent to `Base.convert(SyntaxTree, f)`.

See also [`Formula`](@ref), [`SyntaxTree`](@ref).
"""
function tree(f::Formula)
    return error("Please, provide method tree(::$(typeof(f)))::SyntaxTree.")
end

"""$(doc_tokopprop)"""
function tokens(f::Formula)#::AbstractVector{<:SyntaxToken} Commented out because, at this point in the code, return type is still an unknown symbol
    return tokens(tree(f))
end
"""$(doc_tokopprop)"""
function operators(f::Formula)#::AbstractVector{<:Operator}
    return operators(tree(f))
end
"""$(doc_tokopprop)"""
function connectives(f::Formula)#::AbstractVector{<:Connective}
    return connectives(tree(f))
end
"""$(doc_tokopprop)"""
function leaves(f::Formula)#::AbstractVector{<:AbstractLeaf}
    return leaves(tree(f))
end
"""$(doc_tokopprop)"""
function atoms(f::Formula)#::AbstractVector{<:Atom}
    return atoms(tree(f))
end
"""$(doc_tokopprop)"""
function truths(f::Formula)#::AbstractVector{<:Truth}
    return truths(tree(f))
end
"""$(doc_tokopprop)"""
function ntokens(f::Formula)::Integer
    return ntokens(tree(f))
end
"""$(doc_tokopprop)"""
function natoms(f::Formula)::Integer
    return natoms(tree(f))
end
function nleaves(f::Formula)::Integer
    return nleaves(tree(f));
end
"""$(doc_tokopprop)"""
function height(f::Formula)::Integer
    return height(tree(f))
end

function Base.isequal(a::Formula, b::Formula)
    Base.isequal(tree(a), tree(b))
end

Base.hash(a::Formula) = Base.hash(tree(a))

"""$(doc_syntaxstring)"""
function syntaxstring(φ::Formula; kwargs...)
    syntaxstring(tree(φ); kwargs...)
end

function Base.show(io::IO, φ::Formula)
    print(io, "$(typeof(φ))\nsyntaxstring: $(syntaxstring(φ))")
end

"""$(doc_joinformulas)"""
function joinformulas(c::Connective, ::NTuple{N,F})::F where {N,F<:Formula}
    return error("Please, provide method " *
        "joinformulas(c::Connective, children::NTuple{N,$(F)}) where {N}.")
end

function joinformulas(c::Connective, children::Vararg{F,N})::F where {N,F<:Formula}
    joinformulas(c, children)
end

############################################################################################
#### AbstractSyntaxStructure ###############################################################
############################################################################################

"""
    abstract type AbstractSyntaxStructure <: Formula end

Abstract type for the purely-syntactic component of a logical formula (e.g.,
no fancy memoization structure associated).
The typical representation is the [`SyntaxTree`](@ref),
however, different implementations can cover specific syntactic forms
(e.g., conjuctive/disjuctive normal forms).

See also [`Formula`](@ref), [`AbstractLogic`](@ref), [`SyntaxTree`](@ref),
[`tree`](@ref).
"""
abstract type AbstractSyntaxStructure <: Formula end

############################################################################################
#### SyntaxTree ############################################################################
############################################################################################

"""
    abstract type SyntaxTree <: AbstractSyntaxStructure end

Abstract type for syntax leaves (see `SyntaxLeaf`, such as `Truth` values and `Atom`s),
and their composition via `Connective`s (i.e., `SyntaxBranch`).

See also [`SyntaxLeaf`](@ref), [`SyntaxBranch`](@ref).
"""
abstract type SyntaxTree <: AbstractSyntaxStructure end

tree(t::SyntaxTree) = t
SyntaxTree(t::SyntaxTree) = tree(t)


"""$(doc_tokopprop)"""
function tokens(t::SyntaxTree)
    return error("Unexpected syntax tree (type = $(typeof(t)))")
end

"""$(doc_syntaxtree_operators)"""
function operators(t::SyntaxTree)
    error("Unexpected syntax tree (type = $(typeof(t)))")
end

"""$(doc_syntaxtree_connectives)"""
function connectives(t::SyntaxTree)::AbstractVector{Connective}
    error("Unexpected syntax tree (type = $(typeof(t)))")
end

"""$(doc_syntaxtree_leaves)"""
function leaves(t::SyntaxTree)#::AbstractVector{<:AbstractLeaf}
    error("Unexpected syntax tree (type = $(typeof(t)))")
end
"""$(doc_syntaxtree_atoms)"""
function atoms(t::SyntaxTree)#::AbstractVector{<:Atom}
    error("Unexpected syntax tree (type = $(typeof(t)))")
end
"""$(doc_syntaxtree_truths)"""
function truths(t::SyntaxTree)#::AbstractVector{<:Truth}
    error("Unexpected syntax tree (type = $(typeof(t)))")
end

function Base.isequal(a::SyntaxTree, b::SyntaxTree)
    error("Please, provide method Base.hash(::$(typeof(a)), ::$(typeof(b))).")
end

function Base.hash(a::SyntaxTree)
    error("Please, provide method Base.hash(::$(typeof(a))).")
end

# Syntax tree, the universal syntax structure representation,
# wins when promoted with syntax structures/tokens and syntax trees.
Base.promote_rule(::Type{<:SyntaxTree}, ::Type{<:SyntaxTree}) = SyntaxTree
Base.promote_rule(::Type{<:AbstractSyntaxStructure}, ::Type{S}) where {S<:SyntaxTree} = S
Base.promote_rule(::Type{S}, ::Type{<:AbstractSyntaxStructure}) where {S<:SyntaxTree} = S

############################################################################################
#### SyntaxBranch, part 1 (temporarely ignoring SyntaxLeaf, Atom and Truth definition) #####
############################################################################################

"""
    struct SyntaxBranch{T<:Connective} <: SyntaxTree
        token::T
        children::NTuple{N,SyntaxTree} where {N}
    end

An internal node of a syntax tree encoding a logical formula.
Such a node holds a syntax `token`, which is a `Connective`,
and has as many children as the `arity` of the token.

This implementation is *arity-compliant*, in that, upon construction,
the arity of the token is checked against the number of children provided.

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
end

# Helpers
function SyntaxBranch{T}(token::T, children...) where {T<:Connective}
    return SyntaxBranch{T}(token, children)
end
function SyntaxBranch(token::T, children...) where {T<:Connective}
    return SyntaxBranch(token, children)
end

"""$(doc_syntaxbranch_children)"""
children(t::SyntaxBranch) = t.children

"""$(doc_syntaxbranch_token)"""
token(t::SyntaxBranch) = t.token

"""$(doc_syntaxbranch_tokens)"""
function tokens(t::SyntaxBranch)
    return SyntaxToken[vcat(tokens.(children(t))...)..., token(t)]
end

tokentype(::SyntaxBranch{T}) where {T} = T

"""$(doc_syntaxbranch_tokenstype)"""
tokenstype(t::SyntaxBranch) = Union{tokentype(t),tokenstype.(children(t))...}

"""$(doc_syntaxbranch_operators)"""
function operators(t::SyntaxBranch)::AbstractVector{Operator}
    c = token(t) isa Operator ? [token(t)] : []
    return Operator[vcat(operators.(children(t))...)..., c...]
end

"""$(doc_syntaxbranch_operatorstype)"""
operatorstype(t::SyntaxBranch) = typeintersect(Operator, tokenstype(t))

"""$(doc_syntaxbranch_connectives)"""
function connectives(t::SyntaxBranch)::AbstractVector{Connective}
    c = token(t) isa Connective ? [token(t)] : []
    return Connective[vcat(connectives.(children(t))...)..., c...]
end

# Dispatches defined later because of type dependency reasons
# function leaves    (t::SyntaxBranch)::AbstractVector{SyntaxLeaf}
# function atoms     (t::SyntaxBranch)
# function atomstype (t::SyntaxBranch)
# function truths    (t::SyntaxBranch)

"""$(doc_syntaxbranch_ntokens)"""
function ntokens(t::SyntaxBranch)::Integer
    length(children(t)) == 0 ? 1 : 1 + sum(ntokens(c) for c in children(t))
end

"""$(doc_syntaxbranch_noperators)"""
function noperators(t::SyntaxBranch)::Integer
    op = token(t) isa Operator ? 1 : 0
    return length(children(t)) == 0 ? op : op + sum(noperators(c) for c in children(t))
end

"""$(doc_syntaxbranch_nconnectives)"""
function nconnectives(t::SyntaxBranch)::Integer
    c = token(t) isa Connective ? 1 : 0
    return length(children(t)) == 0 ? c : c + sum(nconnectives(c) for c in children(t))
end

# Dispatches defined later because of type dependency reasons
# function nleaves  (t::SyntaxBranch)::Integer
# function natoms   (t::SyntaxBranch)::Integer
# function ntruths  (t::SyntaxBranch)::Integer

"""$(doc_syntaxbranch_height)"""
function height(t::SyntaxBranch)::Integer
    length(children(t)) == 0 ? 0 : 1 + maximum(height(c) for c in children(t))
end

function Base.isequal(a::SyntaxBranch, b::SyntaxBranch)
    return Base.isequal(token(a), token(b)) &&
        all(((c1,c2),)->Base.isequal(c1, c2), zip(children(a), children(b)))
end

Base.hash(a::SyntaxBranch) = Base.hash(syntaxstring(a))

function Base.show(io::IO, t::SyntaxBranch)
    print(io, "$(typeof(t))($(syntaxstring(t)))")
    print(io, "$(syntaxstring(t))")
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

"""$(doc_arity)"""
arity(l::SyntaxLeaf)::Integer = 0;

token(l::SyntaxLeaf) = l
tokens(l::SyntaxLeaf) = [l]
ntokens(::SyntaxLeaf) = 1
tokentype(::T) where {T <: SyntaxLeaf} = T

connectives(::SyntaxLeaf) = []
nconnectives(l::SyntaxLeaf) = error("Unexpected leaf token (type = $(typeof(l)))")

children(::SyntaxLeaf) = ()

leaves(l::SyntaxLeaf) = l
nleaves(l::SyntaxLeaf) = 1

noperators(l::SyntaxLeaf) = error("Unexpected leaf token (type = $(typeof(l)))")

natoms(l::SyntaxLeaf) = error("Unexpected leaf token (type = $(typeof(l)))")

ntruths(l::SyntaxLeaf) = error("Unexpected leaf token (type = $(typeof(l))))")

height(l::SyntaxLeaf) = 0

Base.convert(::Type{S}, tok::SyntaxLeaf) where {S<:SyntaxTree} = S(tok)
Base.convert(::Type{AbstractSyntaxStructure}, tok::SyntaxLeaf) = SyntaxTree(tok)

############################################################################################
#### SyntaxToken ###########################################################################
############################################################################################

"""
    const SyntaxToken = Union{Connective,SyntaxLeaf}

Union type for values wrapped in `SyntaxTree` nodes.

See also [`SyntaxTree`](@ref), [`SyntaxLeaf`](@ref), [`Connective`](@ref).
"""
const SyntaxToken = Union{Connective,SyntaxLeaf}

dual(value) = error("Please, provide method SoleLogics.dual(::$(typeof(value))).")
"""$(doc_dual)"""
dual(t::SyntaxToken) = error("Please, provide method dual(::$(typeof(t))).")

hasdual(t::SyntaxToken) = false

"""$(doc_formula_basein)"""
function Base.in(tok::SyntaxToken, φ::Formula)::Bool
    return (φ isa SyntaxLeaf ? tok == φ : Base.in(tok, tree(φ)))
end

function Base.show(io::IO, t::SyntaxToken)
    print(io, syntaxstring(t))
end

############################################################################################
#### Atom ##################################################################################
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

arity(::Atom) = 0

value(p::Atom) = p.value

atoms(t::Atom) = [t]
natoms(::Atom) = 1

truths(t::Atom) = []

operators(::Atom) = []
noperators(::Atom) = 0

nconnectives(::Atom) = 0

ntruths(::Atom) = 0

valuetype(::Atom{A}) where {A} = A
valuetype(::Type{Atom{A}}) where {A} = A

Base.convert(::Type{P}, p::Atom) where {P<:Atom} = P(p)
Base.convert(::Type{P}, a) where {P<:Atom} = P(a)

dual(p::Atom) = Atom(dual(value(p)))

Base.isequal(a::Atom, b::Atom) = Base.isequal(value(a), value(b))
Base.isequal(a::Atom, b) = Base.isequal(value(a), b)
Base.isequal(a, b::Atom) = Base.isequal(a, value(b))
Base.hash(a::Atom) = Base.hash(value(a))

"""$(doc_syntaxstring)"""
syntaxstring(a::Atom; kwargs...)::String = syntaxstring(value(a); kwargs...)

############################################################################################
#### Truth #################################################################################
############################################################################################

"""
    abstract type Truth <: SyntaxLeaf end

Abstract type for syntax leaves representing values of a lattice algebra.
In Boolean logic, the two [`BooleanTruth`](@ref) values [`Top`](@ref)
and [`Bot`](@ref) are tused.

# Implementation

TODO: when implementing a custom `Truth` subtype..., provide istop, isbot

See also [`Top`](@ref), [`Bot`](@ref), [`BooleanTruth`](@ref), [`arity`](@ref);
"""
abstract type Truth <: SyntaxLeaf end

"""
    const Operator = Union{Connective,Truth}

Union type for logical constants of any ariety (zero for `Truth` values, non-zero for
`Connective`s).

See also [`Connective`](@ref), [`Truth`](@ref).
"""
const Operator = Union{Connective,Truth}

operators(t::Truth) = Operator[t]
noperators(t::Truth) = 1

atoms(t::Truth) = Atom[]

truths(t::Truth) = [t]
ntruths(t::Truth) = 1

nconnectives(t::Truth) = 1

natoms(t::Truth) = 0

############################################################################################
#### SyntaxBranch, part 2 (new methods considering SyntaxLeaf, Atom and Truth definition) ##
############################################################################################

function SyntaxBranch(l::SyntaxLeaf, args...)
    @assert length(args) == 0 "Leaf $(l) (type $(typeof(l))) is nullary, " *
        " and cannot take syntax children ($(length(args)) were given)."
    return l
end

function leaves(t::SyntaxBranch)::AbstractVector{SyntaxLeaf}
    return SyntaxLeaf[vcat(leaves.(children(t))...)...]
end

function atoms(t::SyntaxBranch)
    return Atom[vcat(atoms.(children(t))...)...] |> unique
end

function truths(t::SyntaxBranch)
    return Atom[vcat(truths.(children(t))...)...] |> unique
end

"""$(doc_syntaxbranch_ntruths)"""
function ntruths(t::SyntaxBranch)::Integer
    t = token(t) isa Truth ? 1 : 0
    return length(children(t)) == 0 ? t : t + sum(ntruths(c) for c in children(t))
end

"""$(doc_syntaxbranch_atomstype)"""
atomstype(t::SyntaxBranch) = typeintersect(Atom, tokenstype(t))

"""$(doc_syntaxbranch_natoms)"""
function natoms(t::SyntaxBranch)::Integer
    a = token(t) isa Atom ? 1 : 0
    return length(children(t)) == 0 ? a : a + sum(natoms(c) for c in children(t))
end

"""$(doc_syntaxbranch_nleaves)"""
function nleaves(t::SyntaxBranch)::Integer
    op = token(t) isa SyntaxLeaf ? 1 : 0
    return length(children(t)) == 0 ? op : op + sum(nleaves(c) for c in children(t))
end

"""$(doc_formula_basein)"""
function Base.in(tok::SyntaxToken, tree::SyntaxBranch)::Bool
    return tok == token(tree) || any([Base.in(tok, c) for c in children(tree)])
end

"""$(doc_syntaxstring)"""
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

############################################################################################
#### AbstractMemoFormula ###################################################################
############################################################################################

"""
    abstract type AbstractMemoFormula <: Formula end

Enriched formula representation, which associates a syntactic structure with
additional [memoization](https://en.wikipedia.org/wiki/Memoization) structures,
which can save computational time upon
[model checking](https://en.wikipedia.org/wiki/Model_checking).

See also [`AbstractSyntaxStructure`](@ref).
"""
abstract type AbstractMemoFormula <: Formula end


############################################################################################
#### AbstractAlphabet ######################################################################
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

############################################################################################
#### ExplicitAlphabet ######################################################################
############################################################################################

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

Base.convert(::Type{AbstractAlphabet}, alphabet::Vector{<:Atom}) =
    ExplicitAlphabet(alphabet)

############################################################################################
#### AlphabetOfAny #########################################################################
############################################################################################

"""
    struct AlphabetOfAny{A} <: AbstractAlphabet{A} end

An implicit, infinite alphabet that includes all atoms with values of a subtype of A.

See also [`AbstractAlphabet`](@ref).
"""
struct AlphabetOfAny{A} <: AbstractAlphabet{A} end
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.in(::Atom{PA}, ::AlphabetOfAny{AA}) where {PA,AA} = (PA <: AA)

############################################################################################
#### AbstractGrammar #######################################################################
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

############################################################################################
#### CompleteFlatGrammar ###################################################################
############################################################################################

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

See also [`AbstractGrammar`](@ref), [`Operator`](@ref), [`alphabet`](@ref),
[`formulas`](@ref), [`connectives`](@ref), [`operators`](@ref), [`leaves`](@ref).
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

"""
    connectives(g::AbstractGrammar)

List all connectives appearing in a grammar.

See also [`Connective`](@ref), [`nconnectives`](@ref).
"""
function connectives(g::AbstractGrammar)::AbstractVector{Connective}
    return filter(!isnullary, operators(g))
end

"""
    leaves(g::AbstractGrammar)

List all leaves appearing in a grammar.

See also [`SyntaxLeaf`](@ref), [`nleaves`](@ref).
"""
function leaves(g::AbstractGrammar)
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
    cur_formulas = Vector{SyntaxTree}(leaves(g))
    all_formulas = SyntaxTree[cur_formulas...]
    while depth < maxdepth && (isnothing(nformulas) || length(all_formulas) < nformulas)
        _nformulas = length(all_formulas)
        cur_formulas = []
        for op in connectives(g)
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
#### AbstractAlgebra, semantics ############################################################
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

function joinformulas(op::Connective, children::NTuple{N,AbstractSyntaxStructure}) where {N}
    return SyntaxBranch(op, tree.(children))
end

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
#### AbstractLogic #########################################################################
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
#### AbstractInterpretation ################################################################
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

############################################################################################
#### Check & Interpret #####################################################################
############################################################################################

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
│  false │   true │ TODO @Mauro adjust all of these. (TruthDict functioning)
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
#### Utilities #############################################################################
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
