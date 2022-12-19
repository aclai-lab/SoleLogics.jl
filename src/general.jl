import Base: convert, promote_rule, _promote
import Base: eltype, in, getindex, isiterable, iterate, IteratorSize, length

export iscrisp, isfuzzy, isfinite,
    isnullary, isunary, isbinary

export Proposition,
    #
    AlphabetOfAny,
    ExplicitAlphabet,
    LazyAlphabet,
    #
    AbstractOperator,
    #
    SyntaxTree,
    #
    CompleteFlatGrammar,
    #
    TruthOperator,
    #
    Formula

export TOP, BOTTOM, ⊤, ⊥

export arity, atomtype, propositiontype, tokentype, tokentypes, propositiontypes, operatortypes, truthtype, collate_truth
export check
export atom, propositions, token, children, alphabet, formulas, domain, top, bottom, grammar, algebra, logic, check, tree
export tokens, operators, propositions

############################################################################################
########################################## SYNTAX ##########################################
############################################################################################

"""
    abstract type SyntaxToken end

A token in a syntax tree.
A syntax tree is a tree-like structure representing a formula, where each
node holds a *token*, and has as many children as the `arity` of the token.
"""
abstract type SyntaxToken end

"""
Each syntax token must provide a method yielding its `arity`:

    arity(::Type{<:SyntaxToken})::Integer
    arity(t::SyntaxToken)::Integer = arity(typeof(t))

The arity of a token is the expected number of children of a node that
wraps it in a syntax tree.
See also [`SyntaxToken`](@ref).

TODO: I have tried to use ?arity and it says.
help?> arity
search: ariety startswith unsafe_write atreplinit partialsort! partialsort partialsortperm partialsortperm! AbstractDict AbstractUnitRange AbstractIrrational

Couldn't find arity
Perhaps you meant ariety, write, Array, acot, acotd, acoth, any, asin, asind, asinh, wait, edit, empty, exit, print, retry, try or varinfo
  No documentation found.

  Binding arity does not exist.

Anyway, this is a recurring behavior; e.g., the same error holds for ?Proposition, and
perhaps others.
TODO-reply: Works for me when I julia -it8 src/test.jl. To make it clear:
 all of these definitions are not imported when you do `using SoleLogics`, because these
 files are still not in the package. Of course, this is a TODO.
"""
arity(T::Type{<:SyntaxToken})::Integer = error("Please, provide method arity(::$(T)).")
arity(t::SyntaxToken)::Integer = arity(typeof(t))

"""
    struct Proposition{A} <: SyntaxToken
        atom::A
    end

A `Proposition{A}` (also called a propositional letter, or simply *letter*) wraps a value
`atom::A` representing a fact which truth can be assessed on a logical model.

See also [`SyntaxToken`](@ref), [`AbstractLogicalModel`](@ref), [`check`](@ref).
"""
struct Proposition{A} <: SyntaxToken
    atom::A
end

arity(::Type{<:Proposition}) = 0
atomtype(::Proposition{A}) where {A} = A
atomtype(::Type{Proposition{A}}) where {A} = A
atom(p::Proposition) = p.atom

show(io::IO, t::Proposition) = print(io, atom(t))

Base.convert(::Type{P1}, t::P2) where {P1<:Proposition,P2<:Proposition} = P1(atom(t))

"""
    abstract type AbstractOperator <: SyntaxToken end

An operator is a [logical constant](https://en.m.wikipedia.org/wiki/Logical_connective)
which establishes a relation between propositions (i.e., facts).
For example, the boolean operators AND, OR and IMPLIES (stylized as ∧, ∨ and →)
are used to connect propositions and express derived concepts.
TODO: Correct, but perhaps not enough.
TODO-reply: Ok boomer, scrivi what it's missing.

Since operators display very different algorithmic behaviors,
all `struct`'s that are subtypes of `AbstractOperator` must
be parametric singleton types, which can be dispatched upon.

See also [`SyntaxToken`](@ref), [`NamedOperator`](@ref), [`check`](@ref).
"""
abstract type AbstractOperator <: SyntaxToken end

isnullary(O::Type{<:AbstractOperator}) = arity(O) == 0
isnullary(o::AbstractOperator) = isnullary(typeof(o))
isunary(O::Type{<:AbstractOperator}) = arity(O) == 1
isunary(o::AbstractOperator) = isunary(typeof(o))
isbinary(O::Type{<:AbstractOperator}) = arity(O) == 2
isbinary(o::AbstractOperator) = isbinary(typeof(o))
"""
TODO: Observe that there can be, e.g., modal operators which are ternary as the ones in the CDT logic.
Therefore, maybe we should also have a check_arity(::T, a::Integer) function, if there isnt one already.
TODO-reply: These are just helpers, for now there's no need.
"""

"""
    abstract type AbstractAlphabet{A} end

Abstract type for representing an alphabet of propositions with atoms of type `A`.
An alphabet (or propositional alphabet) is assumed to be a
[https://en.m.wikipedia.org/wiki/Countable_set](countable) set of propositions.

See also [`Proposition`](@ref), [`AbstractGrammar`](@ref).
"""
abstract type AbstractAlphabet{A} end

Base.eltype(::Type{<:AbstractAlphabet{A}}) where {A} = Proposition{A}
propositiontype(A::Type{<:AbstractAlphabet}) = eltype(A)
propositiontype(a::AbstractAlphabet) = propositiontype(typeof(a))
atomtype(a::Type{<:AbstractAlphabet}) = atomtype(propositiontype(a))
atomtype(a::AbstractAlphabet) = atomtype(propositiontype(a))

"""
Each alphabet must provide a method for establishing whether
a proposition belongs or not to it:

    Base.in(p::Proposition, a::AbstractAlphabet)::Bool

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
function Base.in(p::Proposition, a::AbstractAlphabet)::Bool
    if atomtype(p) <: eltype(a)
        return error("Please, provide method Base.in(::Proposition, ::$(typeof(a))).")
    else
        return error("Cannot establish whether proposition $(p) of type $(typeof(p)) is in" *
            " alphabet $(a) of type $(typeof(a)) and eltype $(eltype(a)).")
    end
end

# # Dangerous Helper
# Base.in(o::Any, a::AbstractAlphabet) = Base.in(Proposition(o), a) # error("Attempting Base.in($(typeof(o)), ::$(typeof(a))), but only Proposition's can belong to alphabets.")

"""
Each alphabet must specify whether it is *iterable* or not.
An alphabet is iterable if it provides the (two) `iterate` methods required by the
[https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration](iteration interface).

By default, an alphabet is considered iterable:

    Base.isiterable(::Type{<:AbstractAlphabet}) = true
    Base.isiterable(a::AbstractAlphabet) = Base.isiterable(typeof(a))
    Base.iterate(a::AbstractAlphabet) = error(...)
    Base.iterate(a::AbstractAlphabet, state) = error(...)

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
Base.isiterable(::Type{<:AbstractAlphabet}) = true
Base.isiterable(a::AbstractAlphabet) = Base.isiterable(typeof(a))
function Base.iterate(a::AbstractAlphabet)
    if isiterable(a)
        return error("Please, provide method Base.iterate(::$(typeof(a)))," *
            " or define Base.isiterable(::$(typeof(a))) = false.")
    else
        return error("Cannot iterate infinite alphabet of type $(typeof(a)).")
    end
end
function Base.iterate(a::AbstractAlphabet, state)
    if isiterable(a)
        return error("Please, provide method Base.iterate(::$(typeof(a)), state)," *
            " or define Base.isiterable(::$(typeof(a))) = false.")
    else
        return error("Cannot iterate infinite alphabet of type $(typeof(a)).")
    end
end

"""
Each alphabet must specify whether it is finite.
An alphabet is finite if it provides the `length` method.

By default, an alphabet is considered finite:

    Base.isfinite(::Type{<:AbstractAlphabet}) = true
    Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))
    Base.length(a::AbstractAlphabet) = error(...)

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
Base.isfinite(::Type{<:AbstractAlphabet}) = true
Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))
function Base.length(a::AbstractAlphabet)
    if isfinite(a)
        return error("Please, provide method Base.length(::$(typeof(a)))," *
            " or define Base.isfinite(::$(typeof(a))) = false.")
    else
        return error("Cannot compute length of alphabet of type $(typeof(a)).")
    end
end

# [https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration](Iteration interface) util.
function Base.IteratorSize(::Type{M}) where {M<:AbstractAlphabet}
    return Base.isfinite(M) ? Base.HasLength() : Base.IsInfinite()
end

"""
TODO: At this point I would like to stress that the BlueStyle requires the following:
    When using long-form functions always use the return keyword (check https://github.com/invenia/BlueStyle)

    I will add return keywords where I think it is safe, please check.
TODO Checked ;)
"""

"""
    propositions(a::AbstractAlphabet)::AbstractVector{propositiontype(a)}

Provides access to the propositions of an iterable alphabet.
If the alphabet is finite, the default behavior is `collect`ing all the propositions.
If it is not finite, a method for enumerating the propositions should be provided.

An alphabet can also implement an extended version of this function:

    propositions(a::AbstractAlphabet, args...)::AbstractVector{propositiontype(a)}

that only returns propositions satisfying a given constraint.
This is especially useful when dealing with infinite alphabets.

See also [`AbstractAlphabet`](@ref), [`isiterable`](@ref), [`Base.isfinite`](@ref).
"""
function propositions(a::AbstractAlphabet)::AbstractVector{propositiontype(a)}
    if isiterable(a)
        if Base.isfinite(a)
            return collect(a)
        else
            return error("Please, provide method propositions(::$(typeof(a)))." *
                " Note: attempting at iterating through an infinite alphabet.")
        end
    else
        return error("Cannot list propositions of an alphabet of type $(typeof(a)).")
    end
end

function propositions(a::AbstractAlphabet, args...)::AbstractVector{propositiontype(a)}
    return error("Please, provide method propositions(::$(typeof(a)), args...) for a bounded" *
        " iteration through an infinite alphabet.")
end


"""
    struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
        propositions::Vector{Proposition{A}}
    end

An alphabet wrapping propositions in a Vector{A}

See also [`AbstractAlphabet`](@ref).
"""
struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
    propositions::Vector{Proposition{A}}

    function ExplicitAlphabet{A}(propositions) where {A}
        return new{A}(collect(propositions))
    end

    """
    TODO: Why ExplicitAlphabet{A} and not new{A}? The same question holds for the other constructor
    """
    function ExplicitAlphabet(propositions::AbstractVector{Proposition{A}}) where {A}
        return ExplicitAlphabet{A}(collect(propositions))
    end

    function ExplicitAlphabet(propositions::AbstractVector{A}) where {A}
        return ExplicitAlphabet{A}(Proposition.(collect(propositions)))
    end
end
Base.in(p::Proposition, a::ExplicitAlphabet) = Base.in(p, a.propositions)
Base.iterate(a::ExplicitAlphabet) = Base.iterate(a.propositions)
Base.iterate(a::ExplicitAlphabet, state) = Base.iterate(a.propositions, state)
Base.length(a::ExplicitAlphabet) = length(a.propositions)

"""
    struct AlphabetOfAny{A} <: AbstractAlphabet{A} end

An implicit infinite alphabet that includes all propositions with atoms of a subtype of A.
"""
struct AlphabetOfAny{A} <: AbstractAlphabet{A} end
Base.in(::Proposition{AA}, ::AlphabetOfAny{A}) where {A,AA} = (AA <: A)
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.isiterable(::Type{<:AlphabetOfAny}) = false

############################################################################################
############################################################################################
############################################################################################

"""
    struct SyntaxTree{FT<:SyntaxToken,T<:FT}
        token::T
        children::NTuple{N, SyntaxTree} where {N}
    end

A syntax tree encoding a logical formula.
Each node of the syntax tree holds a `token::T`, and
has as many children as the `arity` of the token.

This implementation is *safe*, in that, this arity check is performed upon construction.
An additional type parameter `FT` ensures that the token types of the sub-tree are
constrained to a predefined set of types.
When it is not specified, this parameter defaults to the `Union` between `T`, and the `FT`'s
of the child nodes.

See also [`SyntaxToken`](@ref), [`arity`](@ref), [`Proposition`](@ref), [`Operator`](@ref).
"""

# TODO: Perhaps we should have some macros, or something, for the errors/asserts/etc for homogeneity in the returned messages
# TODO-reply: yes, definitely.
struct SyntaxTree{FT<:SyntaxToken,T<:SyntaxToken} # T<:FT
    token::T
    children::NTuple{N,SyntaxTree} where {N}

    function _boundchecks(FT, N, T, token, children)
        @assert arity(token) == N "Cannot instantiate SyntaxTree{$(FT), $(T)} with token" *
            " $(token) of arity $(arity(token)) and $(N) children."
        @assert all([T, tokentypes.(children)...] .<: FT) "Cannot instantiate" *
            " SyntaxTree{$(FT), $(T)} with token::$(T) and children of" *
            " tokens::$(tokentypes.(children))."
        return nothing
    end

    """
    TODO: I'm not sure about Union{SyntaxToken,SyntaxTree} because Operator <: SyntaxToken,
    and an Operator can be ∧ (logical and) which must have a sub-tree with 2 childs.
    Is this intended to address such issue?
    TODO-reply: I think it's okay: SyntaxTree(¬, (⊤,)) works,
        while SyntaxTree(¬, (∨,)) says something like "cannot instantiate SyntaxTree{∨},
            because of arity..."
    """
    function SyntaxTree{FT,T}(
        token::T,
        children::NTuple{N,Union{SyntaxToken,SyntaxTree}}=(),
    ) where {FT<:SyntaxToken,T<:FT,N}
        children = convert.(SyntaxTree, children)
        _boundchecks(FT, N, T, token, children)
        return new{FT,T}(token, children)
    end

    function SyntaxTree{FT}(
        token::T,
        children::NTuple{N,Union{SyntaxToken,SyntaxTree}}=(),
    ) where {FT<:SyntaxToken,T<:FT,N}
        children = convert.(SyntaxTree, children)
        _boundchecks(FT, N, T, token, children)
        return new{FT,T}(token, children)
    end

    function SyntaxTree(
        token::T,
        children::NTuple{N,Union{SyntaxToken,SyntaxTree}}=(),
    ) where {T<:SyntaxToken,N}
        children = convert.(SyntaxTree, children)
        FT = Union{T,tokentypes.(children)...}
        _boundchecks(FT, N, T, token, children)
        return new{FT,T}(token, children)
    end
end

# Helpers
function SyntaxTree{FT,T}(token::T, children...) where {FT,T<:SyntaxToken}
    return SyntaxTree{FT,T}(token, children)
end
function SyntaxTree{FT}(token::T, children...) where {FT,T<:SyntaxToken}
    return SyntaxTree{FT}(token, children)
end
function SyntaxTree(token::T, children...) where {T<:SyntaxToken}
    return SyntaxTree(token, children)
end

"""
    inorder(t::SyntaxTree)::String

Performs an in-order visit of `t`, returning it as a string.
"""
function inorder(t::SyntaxTree)
    return length(children(t)) == 0 ? string(token(t)) : string(token(t)) * "(" * join([inorder(c) for c in children(t)], ", ") * ")"
end

show(io::IO, t::SyntaxTree) = print(io, inorder(t))

# Getters
token(t::SyntaxTree) = t.token
children(t::SyntaxTree) = t.children

tokentype(::SyntaxTree{FT,T}) where {FT,T} = T
tokentypes(::SyntaxTree{FT}) where {FT} = FT
operatortypes(t::SyntaxTree) = typeintersect(AbstractOperator, tokentypes(t))
propositiontypes(t::SyntaxTree) = typeintersect(Proposition, tokentypes(t))

function Base.in(t::SyntaxToken, tree::SyntaxTree)
    return t == token(tree) || any([Base.in(t, c) for c in children(tree)])
end

"""
    tokens(t::SyntaxTree)::AbstractVector{tokentypes(t)}

Enumerates all tokens appearing in a tree

See also [`SyntaxToken`](@ref).

"""
function tokens(t::SyntaxTree)::AbstractVector{tokentypes(t)}
    return SyntaxToken[vcat(tokens.(children(t))...)..., token(t)]
end

"""
    operators(t::SyntaxTree)::AbstractVector{operatortypes(t)}

Enumerates all operators appearing in a tree

See also [`propositions`](@ref), [`tokens`](@ref), [`SyntaxToken`](@ref).

"""
function operators(t::SyntaxTree)::AbstractVector{operatortypes(t)}
    ops = token(t) isa AbstractOperator ? [token(t)] : []
    return AbstractOperator[vcat(operators.(children(t))...)..., ops...]
end

"""
    propositions(t::SyntaxTree)::AbstractVector{propositiontypes(t)}

Enumerates all propositions appearing in a tree

See also [`operators`](@ref), [`tokens`](@ref), [`SyntaxToken`](@ref).

"""
function propositions(t::SyntaxTree)::AbstractVector{propositiontypes(t)}
    ps = token(t) isa Proposition ? [token(t)] : []
    return Proposition[vcat(propositions.(children(t))...)..., ps...]
end

"""
    ntokens(t::SyntaxTree)::Integer

Counts all tokens appearing in a tree

See also ...

"""
function ntokens(t::SyntaxTree)::Integer
    length(children(t)) == 0 ? 1 : 1 + sum(ntoken(c) for c in children(t))
end

"""
    npropositions(t::SyntaxTree)::Integer

Counts all propositions appearing in a tree

See also ...
"""
function npropositions(t::SyntaxTree)::Integer
    pr = token(t) isa Proposition ? 1 : 0
    return length(children(t)) == 0 ? pr : pr + sum(npropositions(c) for c in children(t))
end

# We use standard promotion between syntax tokens and trees
Base.promote_rule(::Type{<:SyntaxToken}, ::Type{S}) where {S<:SyntaxTree} = S
Base.promote_rule(::Type{S}, ::Type{<:SyntaxToken}) where {S<:SyntaxTree} = S

Base.convert(::Type{<:SyntaxTree}, t::SyntaxToken) = SyntaxTree(t)
# Base.convert(::Type{SyntaxTree}, t::SyntaxToken) = SyntaxTree(t)
# Base.convert(::Type{S}, t::T) where {FT<:SyntaxToken, T<:FT, S<:SyntaxTree{FT, T}} = SyntaxTree(t)

"""
    abstract type AbstractGrammar{A<:AbstractAlphabet, O<:AbstractOperator} end

Abstract type for representing a
[context-free grammar](https://en.m.wikipedia.org/wiki/Context-free_grammar)
based on a *single* alphabet of type A, and a set of operators
that consists of all the (singleton) child types of `O`.
A context-free grammar is a simple structure for defining formulas inductively.

See also [`AbstractAlphabet`](@ref), [`AbstractOperator`](@ref).
"""
abstract type AbstractGrammar{A<:AbstractAlphabet,O<:AbstractOperator} end

operatortypes(::AbstractGrammar{A,O}) where {A,O} = O

"""
    alphabet(g::AbstractGrammar{A} where {A})::A

Each grammar must provide a method for accessing its propositional `alphabet`.

See also [`AbstractGrammar`](@ref).
"""
function alphabet(g::AbstractGrammar{A} where {A})::A
    return error("Please, provide method alphabet(::$(typeof(g))).")
end
propositiontype(g::AbstractGrammar) = eltype(alphabet(g))
tokentypes(g::AbstractGrammar) = Union{operatortypes(g),propositiontype(g)}

"""
    Base.in(t::SyntaxToken, g::AbstractGrammar)::Bool

Each grammar must provide some methods for establishing whether a syntax token belongs to it,
that is, whether it is a legal token in the grammar's formulas.

These two fallbacks are defined:

    Base.in(p::Proposition, g::AbstractGrammar) = Base.in(p, alphabet(g))
    Base.in(op::AbstractOperator, g::AbstractGrammar) = op <: operatortypes(O)

See also [`AbstractGrammar`](@ref).
"""
function Base.in(t::SyntaxToken, g::AbstractGrammar)
    return error("Please, provide method Base.in(::$(typeof(t)), ::$(typeof(g))).")
end
# Note: when using this file's syntax tokens, these methods suffice:
Base.in(p::Proposition, g::AbstractGrammar) = Base.in(p, alphabet(g))
Base.in(op::AbstractOperator, g::AbstractGrammar) = (op <: operatortypes(g))


"""
    Base.in(t::SyntaxTree, g::AbstractGrammar)::Bool

Each grammar must provide a method for establishing whether a formula,
encoded as a syntax tree, belongs to it.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
function Base.in(::SyntaxTree, g::AbstractGrammar)::Bool
    return error("Please, provide method Base.in(::SyntaxTree, ::$(typeof(g))).")
end

"""
    formulas(g::AbstractGrammar;
        maxdepth::Integer,
        nformulas::Union{Integer, Nothing} = nothing,
        args...
    )::Vector{<:SyntaxTree{<:tokentypes(g)}}

Each grammar with a finite and iterable alphabet must provide a method for
enumerating its formulas, encoded as syntax trees.
Additional `args` can be used to model the function's behavior.
At least these two arguments should be covered:
- a `nformulas` argument can be used to limit the size of the returned `Vector`;
- a `maxdepth` argument can be used to limit the result to syntax trees of a given
maximum depth;

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
# TODO: do we use space around "=" ? that is "something = else" or "something=else"?
# make a choice and keep it, please (also in the documentation)
# TODO-reply: contrary to BlueStyle, I would like to keep " = ".
function formulas(
    g::AbstractGrammar{A,O} where {A,O};
    maxdepth::Integer,
    nformulas::Union{Integer,Nothing} = nothing,
    args...
)::Vector{<:SyntaxTree{<:tokentypes(g)}}
    @assert maxdepth > 0
    @assert nformulas > 0
    fin = isfinite(alphabet(g))
    ite = isiterable(alphabet(g))
    if fin && ite
        return error("Please, provide method formulas(::$(typeof(g)), maxdepth," *
            " nformulas, args...).")
    else
        return error("Cannot enumerate formulas of $(!fin ?
            "infinite" * (!ite ? " and uniterable" : "") :
            (!ite ? "uniterable" : "")) alphabet ($(typeof(alphabet(g)))).")
    end
end

"""
    struct CompleteFlatGrammar{A<:AbstractAlphabet, O<:AbstractOperator} <: AbstractGrammar{A, O}
        alphabet::A
        operators::Vector{<:O}
    end

Grammar that generates all well-formed formulas obtained by the arity-complying composition
of propositions of an alphabet of type `A`, and all operators with type in `O`.
With n operators, this grammar has exactly n+1 production rules, and
m+1 terminal symbols, where m is the number of nullary operators.
For example, with `O = Union{⊥,∧,∨}`, the grammar is:

    T ::= p | ⊥ | T ∧ T | T ∨ T

with p ∈ alphabet. Note: it is *flat* in the sense that all rules substitute the same
(unique and starting) non-terminal symbol T.

See also [`AbstractOperator`](@ref), [`AbstractGrammar`](@ref).
"""
struct CompleteFlatGrammar{A<:AbstractAlphabet,O<:AbstractOperator} <: AbstractGrammar{A,O}
    alphabet::A
    operators::Vector{<:O}

    function CompleteFlatGrammar{A,O}(
        alphabet::A,
        operators::Vector{<:O},
    ) where {A<:AbstractAlphabet,O<:AbstractOperator}
        return new{A,O}(alphabet, operators)
    end

    function CompleteFlatGrammar{A}(
        alphabet::A,
        operators::Vector{<:AbstractOperator},
    ) where {A<:AbstractAlphabet}
        return new{A,Union{typeof.(operators)...}}(
            alphabet,
            Vector{Union{typeof.(operators)...}}(operators)
        )
    end

    function CompleteFlatGrammar(
        alphabet::A,
        operators::Vector{<:AbstractOperator},
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
    return [propositions(alphabet(g))..., filter(isnullary, operators(g))...]
end

# A complete grammar includes any *safe* syntax tree that can be built with
#  the grammar token types.
function Base.in(t::SyntaxTree, g::CompleteFlatGrammar)::Bool
    return if token(t) isa Proposition
        token(t) in alphabet(g)
    elseif token(t) isa AbstractOperator
        if operatortypes(t) <: operatortypes(g)
            true
        else
            all([Base.in(c, g) for c in children(t)])
        end
    end
end

# TODO: Please comment some parts of this function to understand its logic -- I don't understand its logic
"""
Generates all formulas with syntax trees shorter than a given `maxdepth` .
"""
function formulas(
    g::CompleteFlatGrammar{A,O} where {A,O};
    maxdepth::Integer,
    nformulas::Union{Integer,Nothing} = nothing
)::Vector{SyntaxTree{<:tokentypes(g)}}
    @assert maxdepth > 0
    @assert isnothing(nformulas) || nformulas > 0
    # For increasing `depth`, accumulate all formulas of length `depth` by combining all
    # formulas of `depth-1` using all non-terminal symbols.
    # Stop as soon as `maxdepth` is reached or `nformulas` have been generated.
    depth = 1
    cur_formulas = convert.(SyntaxTree, terminals(g))
    all_formulas = cur_formulas
    while depth < maxdepth && (isnothing(nformulas) || length(all_formulas) < nformulas)
        _nformulas = length(all_formulas)
        cur_formulas = []
        for op in nonterminals(g)
            for children in Iterators.product(fill(all_formulas, arity(op))...)
                if !isnothing(nformulas) && nformulas == _nformulas + length(cur_formulas)
                    break
                end
                push!(cur_formulas, SyntaxTree(op, Tuple(children)))
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
Type alias for any Julia type that may instantiate truth values.

See also [`Algebra`](@ref).
"""
# TODO: Why not all uppercase?
# What do you mean?
const TruthValue = Any

"""
    abstract type AbstractTruthOperator <: AbstractOperator end

A nullary operator wrapping a truth value; in fact, truth values can be used in formulas.
Two canonical truth values that are used as nullary operators are
`⊤` (*top*) and `⊥` (*bottom*), representing truth (`true`) and falsity (`false`),
respectively.

See also [`TOP`](@ref), [`BOTTOM`](@ref), [`TruthValue`](@ref).
"""
abstract type AbstractTruthOperator <: AbstractOperator end
arity(::Type{<:AbstractTruthOperator}) = 0


doc_TOP = """
    struct TopOperator <: AbstractTruthOperator end
    const TOP = TopOperator()
    const ⊤ = TOP

Canonical truth operator representing the value `true`.

See also [`BOTTOM`](@ref), [`AbstractTruthOperator`](@ref), [`TruthValue`](@ref).
"""
"""$(doc_TOP)"""
struct TopOperator <: AbstractTruthOperator end
"""$(doc_TOP)""" # TODO: perhaps useless (or maybe not?). I get it but I don't know, it seems useful to me.
const TOP = TopOperator()
"""$(doc_TOP)""" # TODO: perhaps useless
const ⊤ = TOP

doc_BOTTOM = """
    struct BottomOperator <: AbstractTruthOperator end
    const BOTTOM = BottomOperator()
    const ⊥ = BOTTOM

Canonical truth operator representing the value `false`.

See also [`TOP`](@ref), [`AbstractTruthOperator`](@ref), [`TruthValue`](@ref).
"""
"""$(doc_BOTTOM)"""
struct BottomOperator <: AbstractTruthOperator end
"""$(doc_BOTTOM)""" # TODO: as above
const BOTTOM = BottomOperator()
"""$(doc_BOTTOM)""" # TODO: as above
const ⊥ = BOTTOM

"""
    struct TruthOperator{T<:TruthValue} <: AbstractTruthOperator
        value::T
    end

A truth operator wrapping a truth value of a given type.

See also [`AbstractTruthOperator`](@ref), [`TruthValue`](@ref).
"""
struct TruthOperator{T<:TruthValue} <: AbstractTruthOperator
    value::T
end

"""
    abstract type AbstractAlgebra{T<:TruthValue} end

Abstract type for representing algebras. Algebras are used for grounding the truth of
propositions and the semantics of operators. They typically encode a
[https://en.m.wikipedia.org/wiki/Lattice_(order)](lattice structure) where two elements
(or nodes) *⊤* and *⊥* are referred to as *top* (or maximum) and *bottom* (or minimum).
Each node in the lattice represents a truth value that a proposition or a formula can have
on a model, and the semantics of operators is given in terms of operations between truth
values.

See also [`BooleanAlgebra`](@ref), [`AbstractOperator`](@ref), [`collate_truth`](@ref).
"""
abstract type AbstractAlgebra{T<:TruthValue} end

"""
    truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:TruthValue} = T
    truthtype(a::AbstractAlgebra) = truthtype(typeof(a))

The Julia type for representing truth values of the algebra.

See also [`AbstractAlgebra`](@ref).
"""
# TODO: I <3 this pattern. Me too man
truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:TruthValue} = T
truthtype(a::AbstractAlgebra) = truthtype(typeof(a))

"""
    domain(a::AbstractAlgebra)

Each algebra must provide a method for accessing its `domain`.

See also [`AbstractAlgebra`](@ref).
"""
function domain(a::AbstractAlgebra{T} where {T<:TruthValue})::AbstractVector{T}
    error("Please, provide method domain(::$(typeof(a))).")
end

# Base.in(t::TruthValue, a::AbstractAlgebra) = Base.in(t, domain(a)) maybe one day this will have a use?

"""
    top(a::AbstractAlgebra)

Each algebra must provide a method for accessing its `top`.

See also [`AbstractAlgebra`](@ref).
"""
function top(a::AbstractAlgebra{T} where {T})::T
    return error("Please, provide method top(::$(typeof(a))).")
end

"""
    bottom(a::AbstractAlgebra)

Each algebra must provide a method for accessing its `bottom`.

See also [`AbstractAlgebra`](@ref).
"""
function bottom(a::AbstractAlgebra{T} where {T})::T
    return error("Please, provide method bottom(::$(typeof(a))).")
end

"""
    iscrisp(::Type{<:AbstractAlgebra}) = (truthtype(a) == Bool)
    iscrisp(a::AbstractAlgebra) = iscrisp(typeof(a))

An algebra is crisp (or *boolean*) when its domain type is... `Bool`, quite literally!
The antonym of crisp is *fuzzy*.

See also [`AbstractAlgebra`](@ref).
"""
iscrisp(::Type{<:AbstractAlgebra}) = (truthtype(a) == Bool)
iscrisp(a::AbstractAlgebra) = iscrisp(typeof(a))
isfuzzy = !iscrisp # TODO: Is this even legal? :)) If so, BAAAAM! Yep it is! Nice right.

# TODO: Observe that the following docstring is out of bound (92 columns). The BlueStyle sheds light on this.
# TODO-reply: What should we do?
"""
    collate_truth(a::AbstractAlgebra,op::AbstractOperator,t::NTuple{N, T})::T where {N, T<:TruthValue}

This function computes a truth value of a composed formula op(φ1, ..., φN), given the `N`
truth values of its immediate sub-formulas.
An algebra must provide a `collate_truth` method for each operator that can be
interpreted on it.

See also [`AbstractAlgebra`](@ref) [`AbstractOperator`](@ref), [`TruthValue`](@ref).
"""
function collate_truth(
    a::AbstractAlgebra{T},
    op::AbstractOperator,
    t::NTuple{N,T},
)::T where {N,T<:TruthValue}
    if truthtype(a) != T
        return error("Cannot collate $(length(t)) truth values of type $(T)" *
            " with algebra $(typeof(a)) with truth type $(truthtype(a))).")
    elseif arity(op) != length(t)
        return error("Cannot collate $(length(t)) truth values for" *
            " operator $(typeof(op)) with arity $(arity(op))).")
    else
        return error("Please, provide method collate_truth(::$(typeof(a)), ::$(typeof(op))," *
            " ::NTuple{$(arity(op)), $(truthtype(a))}.")
    end
end

# Note: `collate_truth` for TOP and BOTTOM relies on the `top` and `bottom` methods.
collate_truth(a::AbstractAlgebra, ::typeof(⊤), t::NTuple{0}) = top(a)
collate_truth(a::AbstractAlgebra, ::typeof(⊥), t::NTuple{0}) = bottom(a)

"""
    abstract type AbstractLogic{G<:AbstractGrammar, A<:AbstractAlgebra} end

Abstract type of a logic, which comprehends a context-free grammar (syntax) and
an algebra (semantics).

See also [`AbstractGrammar`](@ref), [`AbstractAlgebra`](@ref).
"""
abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

"""
    grammar(l::AbstractLogic{G})::G where {G<:AbstractGrammar}

A logic must provide a method for accessing its grammar.

See also [`AbstractGrammar`](@ref), [`AbstractLogic`](@ref).
"""
function grammar(l::AbstractLogic{G})::G where {G}
    return error("Please, provide method grammar(::$(typeof(l))).")
end

operatortypes(l::AbstractLogic) = operatortypes(grammar(l))
alphabet(l::AbstractLogic) = alphabet(grammar(l))
propositiontype(l::AbstractLogic) = propositiontype(alphabet(l))
tokentypes(l::AbstractLogic) = tokentypes(grammar(l))
formulas(l::AbstractLogic; args...) = formulas(grammar(l); args...)

Base.in(op::AbstractOperator, l::AbstractLogic) = Base.in(op, grammar(l))
Base.in(t::SyntaxTree, l::AbstractLogic) = Base.in(t, alphabet(l))
Base.in(p::Proposition, l::AbstractLogic) = Base.in(p, alphabet(l))

"""
    algebra(l::AbstractLogic{G,A})::A where {G,A}

A logic must provide a method for accessing its algebra.

See also [`AbstractAlgebra`](@ref), [`AbstractLogic`](@ref).
"""
function algebra(l::AbstractLogic{G,A})::A where {G,A}
    return error("Please, provide method algebra(::$(typeof(l))).")
end

truthtype(l::AbstractLogic) = truthtype(algebra(l))
top(l::AbstractLogic) = top(algebra(l))
bottom(l::AbstractLogic) = bottom(algebra(l))
iscrisp(l::AbstractLogic) = iscrisp(algebra(l))

"""
    abstract type AbstractFormula{L<:AbstractLogic} end

A formula encodes a statement, anchored to a certain logic,
which truth can be evaluated on models of the logic.

It is canonically encoded via a syntax tree (see [`Formula`](@ref))

See also [`AbstractLogic`](@ref), [`SyntaxTree`](@ref).
"""
abstract type AbstractFormula{L<:AbstractLogic} end

"""
    logic(f::AbstractFormula{L})::L where {L<:AbstractLogic}

Each formula must provide a method for accessing its `logic`.

See also [`AbstractLogic`](@ref).
"""
logic(f::AbstractFormula) = error("Please, provide method logic(::$(typeof(f))).")
iscrisp(f::AbstractFormula) = iscrisp(logic(f))
grammar(f::AbstractFormula) = grammar(logic(f))
algebra(f::AbstractFormula) = algebra(logic(f))

"""
    Base.in(t::SyntaxToken, f::AbstractFormula)::Bool

Each formula must provide a method for establishing whether a syntax token appears in it.

See also [`SyntaxToken`](@ref).
"""
function Base.in(::SyntaxToken, f::AbstractFormula)
    return error("Please, provide method Base.in(::SyntaxToken, ::$(typeof(f))).")
end

"""
    tree(f::AbstractFormula)::SyntaxTree{<:tokentypes(logic(f))}

A formula must provide a method for extracting its syntax tree representation.

See also [`SyntaxTree`](@ref).
"""
function tree(f::AbstractFormula)::SyntaxTree{<:tokentypes(logic(f))}
    return error("Please, provide method" *
        " tree(::$(typeof(f)))::SyntaxTree{<:$(tokentypes(logic(f)))}.")
end

Base.convert(::Type{<:SyntaxTree}, f::AbstractFormula) = tree(f)

doc_tokopprop = """
    tokens(f::AbstractFormula)::AbstractVector{<:tokentypes(logic(f))}
    operators(f::AbstractFormula)::AbstractVector{<:operatortypes(logic(f))}
    propositions(f::AbstractFormula)::AbstractVector{<:propositiontype(logic(f))}
    ntokens(f::AbstractFormula)::Integer
    npropositions(f::AbstractFormula)::Integer

A formula can provide a method for extracting its tokens/operators/propositions.
The fallbacks extract the tokens/operators/propositions
appearing in its syntax tree representation.

See also [`SyntaxTree`](@ref).
"""

"""$(doc_tokopprop)"""
function tokens(f::AbstractFormula)::AbstractVector{tokentypes(logic(f))}
    return tokens(tree(f))
end
"""$(doc_tokopprop)"""
function operators(f::AbstractFormula)::AbstractVector{operatortypes(logic(f))}
    return operators(tree(f))
end
"""$(doc_tokopprop)"""
function propositions(f::AbstractFormula)::AbstractVector{propositiontype(logic(f))}
    return propositions(tree(f))
end
"""$(doc_tokopprop)"""
function ntokens(f::AbstractFormula)::Integer
    return ntokens(tree(f))
end
"""$(doc_tokopprop)"""
function npropositions(f::AbstractFormula)::Integer
    return npropositions(tree(f))
end

# error("Please, provide method propositions(::$(typeof(f)))::AbstractVector{<:$(propositiontype(logic(f)))}.") # TODO: remove it?


"""
A formula can be used for instating other formulas of the same logic.

In order to use operators for composing formulas, along with syntax tokens (e.g.,
propositions) and syntax trees, each formula should specify a method for constructing
formulas of the same logic out of syntax trees. Let F<:AbstractFormula, this method should
have the following signature:

    (f::F)(t::SyntaxTree)::F # TODO: there is no return type in the definition -- please check. I know, I tried it but I cannot make it work. I think it's useful in the docstring though.

See also [`SyntaxToken`](@ref), [`SyntaxTree`](@ref), [`AbstractOperator`](@ref).

# TODO: I do not understand the meaning of this -- we should provide examples in the final docstrings
# TODO-reply: I don't know if this docstring will appear in the Julia help REPL or in the online doc.
# It's just for commenting this thing. https://docs.julialang.org/en/v1/manual/methods/#Function-like-objects
"""
function (f::F where {F<:AbstractFormula})(::SyntaxTree)
    return error("Please, provide method (::$(typeof(f)))(t::SyntaxTree)::$(typeof(f))" *
        " for instantiating a formula of the same logic.")
end

# Adapted from https://github.com/JuliaLang/julia/blob/master/base/promotion.jl
function Base._promote(x::AbstractFormula, y::SyntaxTree)
    @inline
    return (x, x(y))
end

function Base._promote(x::F, y::SyntaxToken) where {F<:AbstractFormula}
    Base._promote(x, Base.convert(SyntaxTree, y))
end
Base._promote(x::Union{SyntaxTree,SyntaxToken}, y::AbstractFormula) = reverse(Base._promote(y, x))

"""
In order to use operators for composing formulas, along with syntax tokens (e.g.,
propositions) and syntax trees, each formula should specify a composition method:

    (op::AbstractOperator)(children::NTuple{N, F}, args...) where {N, F<:AbstractFormula}

Note that, since `op` might not be in the logic of the child formulas,
the resulting formula may be of a different logic.

See also [`AbstractFormula`](@ref), [`SyntaxTree`](@ref), [`AbstractOperator`](@ref).
"""
function (op::AbstractOperator)(::NTuple{N,F}, args...)::F where {N,F<:AbstractFormula}
    return error("Please, provide method
        (op::AbstractOperator)(children::NTuple{N, $(F)}, args...) where {N}.")
end

"""
    struct Formula{L<:AbstractLogic} <: AbstractFormula{L}
        _logic::Base.RefValue{L}
        tree::SyntaxTree
    end

In the most general case, a formula encodes a syntax tree that is anchored to
a certain logic; that is: a) the tree encodes a formula belonging to the grammar
of the logic; b) the truth of the formula can be evaluated
on models of the logic. Note that, here, the logic is represented by a reference.

Upon construction, the logic can be passed either directly, or via a RefValue.
Additionally, the following keyword arguments may be specified:
- `check_propositions`: whether to perform or not a check that the propositions
    belong to the alphabet of the logic;
- `check_tree`: whether to perform or not a check that the formula's syntax tree
    honors the grammar (includes the check performed with `check_propositions = true`) (TODO);
# TODO: what "honors" should mean? Honor means "rispettare".

See also [`AbstractLogic`](@ref).
"""
struct Formula{L<:AbstractLogic} <: AbstractFormula{L}
    _logic::Base.RefValue{L}
    tree::SyntaxTree # SyntaxTree{FT} where {FT<:tokentypes(_logic[])}

    _l(l::AbstractLogic) = Base.RefValue(l)
    _l(l::Base.RefValue) = l

    function Formula{L}(
        l::Union{L,Base.RefValue{L}},
        ttf::Union{SyntaxToken,SyntaxTree,AbstractFormula};
        check_propositions=false,
        check_tree=false
    ) where {L<:AbstractLogic}
        _logic = _l(l)
        tree = convert(SyntaxTree, ttf)

        if check_tree
            return error("TODO implement check_tree parameter when constructing Formula's!")
        end
        # Check that the propositions belong to the alphabet of the logic
        if !check_tree && check_propositions
            # TODO-reply keep an eye out for macros: they care about line breaking.
            @assert all([p in alphabet(_logic[])
                for p in propositions(tree)]) "Cannot" *
                " instantiate Formula{$(L)} with illegal propositions:" *
                " $(filter((p)->!(p in alphabet(_logic[])), propositions(tree)))"
        end

        # Check that the token types of the tree are a subset of the tokens
        #  allowed by the logic
        @assert tokentypes(tree) <: tokentypes(_logic[]) "Cannot" *
            " instantiate Formula{$(L)} with illegal token types $(tokentypes(tree))." *
            " Token types should be <: $(tokentypes(_logic[]))."

        return new{L}(_logic, tree)
    end

    function Formula(
        l::Union{L,Base.RefValue{L}},
        ttf::Union{SyntaxToken,SyntaxTree,AbstractFormula};
        args...
    ) where {L<:AbstractLogic}
        return Formula{L}(l, ttf; args...)
    end
end

_logic(f::Formula) = f._logic # TODO: so _logic is a "private" function? Yes.
logic(f::Formula) = f._logic[] # TODO: why []? Dereferencing.
tree(f::Formula) = f.tree

Base.in(t::SyntaxToken, f::Formula) = Base.in(t, tree(f))

# When constructing a new formula from a syntax tree, the logic is passed by reference.
(f::Formula)(t::SyntaxTree) = Formula(_logic(f), t)

function (op::AbstractOperator)(children::NTuple{N,Formula}, args...) where {N}
    ls = unique(logic.(children))
    @assert length(ls) == 1 "Cannot" *
        " build formula by combination of formulas with different logics: $(ls)."
    l = first(ls)
    @assert typeof(op) <: operatortypes(l) "TODO expand" *
        " logic's set of operators (op is not in it: $(typeof(op)) ∉ $(operatortypes(l)))."
    return Formula(l, op(map(tree, children)))
end

"""
    abstract type AbstractLogicalModel{A, T<:TruthValue} end

Abstract type for representing a logical model that associates truth values of a type `T`
to propositional letters of atom type `A`.
Classically, a model is referred to as
[interpretation](https://en.m.wikipedia.org/wiki/Interpretation_(logic))
in the case of
[propositional logic](https://simple.m.wikipedia.org/wiki/Propositional_logic),
and can be thought as a map *proposition → truth value*;
more complex structures are used as logical models for the case
of modal and first-order logics (e.g.,
[Kripke models](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s)

Properties, expressed via logical formulas, can be `check`ed on logical models.

See also [`check`](@ref), [`Interpretation`](@ref), [`KripkeStructure`](@ref).
"""
abstract type AbstractLogicalModel{A,T<:TruthValue} end

atomtype(::AbstractLogicalModel{A,T}) where {A,T} = A
truthtype(::AbstractLogicalModel{A,T}) where {A,T} = T

# TODO: docstring out of bounds. What do we do?
"""
    check(f::AbstractFormula, m::AbstractLogicalModel{A, T}, args...)::T where {A, T<:TruthValue}

Each logical model must provide a method `check` for checking formulas on it.

A formula can be checked on a logical model; this process is referred to as
[model checking](https://en.m.wikipedia.org/wiki/Model_checking), and there are many
algorithms for it, typically depending on the complexity of the logic.

See also [`AbstractFormula`](@ref), [`AbstractLogicalModel`](@ref).

"""
function check(
    f::AbstractFormula,
    m::AbstractLogicalModel{A,T},
    args...,
)::T where {A,T<:TruthValue}
    return error("Please, provide method" *
        " check(f::$(typeof(f)), m::$(typeof(m)), args...)::$(truthtype(m))" *
        " with args::$(typeof(args)).")
end

############################################################################################
######################################### UTILS ############################################
############################################################################################

# We provide an extra safety layer by complementing Base.in with syntax tokens/trees and alphabets.
function Base.in(t::Union{SyntaxToken,SyntaxTree}, a::AbstractAlphabet)
    return error("Attempting Base.in($(typeof(t)), ::$(typeof(a)))," *
        " but $(typeof(t))'s cannot belong to alphabets.")
end

"""
An alphabet of `atomtype` `A` can be used for instantiating propositions of atomtype `A`.
"""
(::AbstractAlphabet{A})(a) where {A} = Proposition{A}(a)

"""
An operator can be used to compose syntax tokens (e.g., propositions),
syntax trees and/or formulas. This is quite handy, try it:

    ¬(Proposition(1)) ∨ Proposition(1) ∧ ⊤
"""
function (op::AbstractOperator)(o::Any)
    return error("Cannot apply operator $(op)::$(typeof(op)) to object $(o)::$(typeof(o))")
end
(op::AbstractOperator)(children::Union{SyntaxToken,SyntaxTree}...) = op(children)
function (op::AbstractOperator)(
    children::NTuple{N,Union{SyntaxToken,SyntaxTree}}
) where {N}
    return SyntaxTree(op, children...)
end
function (op::AbstractOperator)(children::Union{SyntaxToken,SyntaxTree,AbstractFormula}...)
    return op(children)
end
function (op::AbstractOperator)(
    children::NTuple{N,Union{SyntaxToken,SyntaxTree,AbstractFormula}}
) where {N}
    _children = map((c) -> begin
            isa(c, SyntaxToken) ? convert(SyntaxTree, c) : c
        end, children)
    return op(Base.promote(_children...))
end
# Resolve ambiguity with nullary operators
function (op::AbstractOperator)()
    return SyntaxTree(op)
end

"""
   default_algebra(::Type{T})::AbstractAlgebra{<:T} where {T<:TruthValue}

In order to check syntax trees without algebras, each truth value should provide
a default algebra it works with.
"""
function default_algebra(::Type{T})::AbstractAlgebra{<:T} where {T<:TruthValue}
    return error("Please, provide method" *
        " default_algebra(::Type{$(T)})::AbstractAlgebra{<:$(T)}.")
end

# Helper: use default algebra when model checking a syntax tree.
function check(tree::SyntaxTree, m::AbstractLogicalModel, args...)
    return check(default_algebra(truthtype(m)), tree, m, args...)
end
