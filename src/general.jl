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

export TOP, ⊤
export BOTTOM, ⊥

export arity, atomtype, propositionstype, tokentype, tokenstype,
        propositionstype, operatorstype, truthtype, collate_truth
export check
export atom, propositions, token, children, alphabet, formulas, domain, top,
        bottom, grammar, algebra, logic, check, tree
export tokens, operators, propositions

############################################################################################
########################################## SYNTAX ##########################################
############################################################################################

"""
    abstract type AbstractSyntaxToken end

A token in a syntax tree.
A syntax tree is a tree-like structure representing a formula, where each
node holds a *token*, and has as many children as the `arity` of the token.
"""
abstract type AbstractSyntaxToken end

"""
Each syntax token must provide a method yielding its `arity`:

    arity(::Type{<:AbstractSyntaxToken})::Integer
    arity(t::AbstractSyntaxToken)::Integer = arity(typeof(t))

The arity of a token is the expected number of children of a node that
wraps it in a syntax tree.

See also [`AbstractSyntaxToken`](@ref).
"""
arity(T::Type{<:AbstractSyntaxToken})::Integer = error("Please, provide method arity(::$(T)).")
arity(t::AbstractSyntaxToken)::Integer = arity(typeof(t))

"""
    struct Proposition{A} <: AbstractSyntaxToken
        atom::A
    end

A `Proposition{A}` (also called a propositional letter, or simply *letter*) wraps a value
`atom::A` representing a fact which truth can be assessed on a logical model.

See also [`AbstractSyntaxToken`](@ref), [`AbstractLogicalModel`](@ref), [`check`](@ref).
"""
struct Proposition{A} <: AbstractSyntaxToken
    atom::A
end

arity(::Type{<:Proposition}) = 0
atomtype(::Proposition{A}) where {A} = A
atomtype(::Type{Proposition{A}}) where {A} = A
atom(p::Proposition) = p.atom

show(io::IO, t::Proposition) = print(io, atom(t))

Base.convert(::Type{P1}, t::P2) where {P1<:Proposition,P2<:Proposition} = P1(atom(t))

"""
    abstract type AbstractOperator <: AbstractSyntaxToken end

An operator is a [logical constant](https://en.m.wikipedia.org/wiki/Logical_connective)
which establishes a relation between propositions (i.e., facts).
For example, the boolean operators AND, OR and IMPLIES (stylized as ∧, ∨ and →)
are used to connect propositions and express derived concepts.
TODO: Correct, but perhaps not enough.
TODO-reply: Ok boomer, scrivi what it's missing.

Since operators display very different algorithmic behaviors,
all `struct`ss that are subtypes of `AbstractOperator` must
be parametric singleton types, which can be dispatched upon.

See also [`AbstractSyntaxToken`](@ref), [`NamedOperator`](@ref), [`check`](@ref).
"""
abstract type AbstractOperator <: AbstractSyntaxToken end

isnullary(O::Type{<:AbstractOperator}) = arity(O) == 0
isnullary(o::AbstractOperator) = isnullary(typeof(o))
isunary(O::Type{<:AbstractOperator}) = arity(O) == 1
isunary(o::AbstractOperator) = isunary(typeof(o))
isbinary(O::Type{<:AbstractOperator}) = arity(O) == 2
isbinary(o::AbstractOperator) = isbinary(typeof(o))

"""
    abstract type AbstractAlphabet{A} end

Abstract type for representing an alphabet of propositions with atoms of type `A`.
An alphabet (or propositional alphabet) is assumed to be a
[https://en.m.wikipedia.org/wiki/Countable_set](countable) set of propositions.

See also [`Proposition`](@ref), [`AbstractGrammar`](@ref).
"""
abstract type AbstractAlphabet{A} end

Base.eltype(::Type{<:AbstractAlphabet{A}}) where {A} = Proposition{A}
propositionstype(A::Type{<:AbstractAlphabet}) = eltype(A)
propositionstype(a::AbstractAlphabet) = propositionstype(typeof(a))
atomtype(a::Type{<:AbstractAlphabet}) = atomtype(propositionstype(a))
atomtype(a::AbstractAlphabet) = atomtype(propositionstype(a))

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
        return error("Cannot establish whether proposition $(p) of type $(typeof(p)) is" *
                     " in alphabet $(a) of type $(typeof(a)) and eltype $(eltype(a)).")
    end
end

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
function Base.IteratorSize(::Type{A}) where {A<:AbstractAlphabet}
    return Base.isfinite(A) ? Base.HasLength() : Base.IsInfinite()
end

"""
    propositions(a::AbstractAlphabet)::AbstractVector{propositionstype(a)}

Provides access to the propositions of an iterable alphabet.
If the alphabet is finite, the default behavior is `collect`ing all the propositions.
If it is not finite, a method for enumerating the propositions should be provided.

An alphabet can also implement an extended version of this function:

    propositions(a::AbstractAlphabet, args...)::AbstractVector{propositionstype(a)}

that only returns propositions satisfying a given constraint.
This is especially useful when dealing with infinite alphabets.

See also [`AbstractAlphabet`](@ref), [`isiterable`](@ref), [`Base.isfinite`](@ref).
"""
function propositions(a::AbstractAlphabet)::AbstractVector{propositionstype(a)}
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

function propositions(a::AbstractAlphabet, args...)::AbstractVector{propositionstype(a)}
    return error("Please, provide method propositions(::$(typeof(a)), args...) for" *
                 " a bounded iteration through an infinite alphabet.")
end


"""
    struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
        propositions::Vector{Proposition{A}}
    end

An alphabet wrapping propositions in a `Vector`.

See also [`AbstractAlphabet`](@ref).
"""
struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
    propositions::Vector{Proposition{A}}

    function ExplicitAlphabet{A}(propositions) where {A}
        return new{A}(collect(propositions))
    end

    """
    TODO: Why ExplicitAlphabet{A} and not new{A}? The same question holds for the other constructors
    TODO2: Please reply or solve it

    TODO: sorry, I missed this one. In this case I have three different
      constructors. If at
      some point I have to add, say, a safety check, I can add it three times
      (one per each constructor); however, I prefer to "cascade" the three
      constructors, so that I can cover the three constructors by writing a unique safety check in the first one.
      This avoids code duplication.
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
Base.in(::Proposition{PA}, ::AlphabetOfAny{AA}) where {PA,AA} = (PA <: AA)
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.isiterable(::Type{<:AlphabetOfAny}) = false

############################################################################################
############################################################################################
############################################################################################

"""
    struct SyntaxTree{FT<:AbstractSyntaxToken,T<:FT}
        token::T
        children::NTuple{N, SyntaxTree} where {N}
    end

A syntax tree encoding a logical formula.
Each node of the syntax tree holds a `token::T`, and
has as many children as the `arity` of the token.

This implementation is *safe*, in that, this arity check is performed upon construction.
An additional type parameter `FT` ensures that the token types of the sub-tree are
constrained to a predefined set of types.
When it is not specified, this parameter defaults to the `Union` between `T`, and the `FT`ss
of the child nodes.

See also [`AbstractSyntaxToken`](@ref), [`arity`](@ref), [`Proposition`](@ref), [`Operator`](@ref).
"""

# TODO: Perhaps we should have some macros, or something, for the errors/asserts/etc for homogeneity in the returned messages
# TODO-reply: yes, definitely.
struct SyntaxTree{FT<:AbstractSyntaxToken,T<:AbstractSyntaxToken} # T<:FT
    token::T
    children::NTuple{N,SyntaxTree} where {N}

    function _boundchecks(FT, N, T, token, children)
        @assert arity(token) == N "Cannot instantiate SyntaxTree{$(FT), $(T)} with token" *
                                  " $(token) of arity $(arity(token)) and $(N) children."
        @assert all([T, tokenstype.(children)...] .<: FT) "Cannot instantiate" *
                                                          " SyntaxTree{$(FT), $(T)} with token::$(T) and children of" *
                                                          " tokens::$(tokenstype.(children))."
        return nothing
    end

    function SyntaxTree{FT,T}(
        token::T,
        children::NTuple{N,Union{AbstractSyntaxToken,SyntaxTree}} = (),
    ) where {FT<:AbstractSyntaxToken,T<:FT,N}
        children = convert.(SyntaxTree, children)
        _boundchecks(FT, N, T, token, children)
        return new{FT,T}(token, children)
    end

    function SyntaxTree{FT}(
        token::T,
        children::NTuple{N,Union{AbstractSyntaxToken,SyntaxTree}} = (),
    ) where {FT<:AbstractSyntaxToken,T<:FT,N}
        children = convert.(SyntaxTree, children)
        _boundchecks(FT, N, T, token, children)
        return new{FT,T}(token, children)
    end

    function SyntaxTree(
        token::T,
        children::NTuple{N,Union{AbstractSyntaxToken,SyntaxTree}} = (),
    ) where {T<:AbstractSyntaxToken,N}
        children = convert.(SyntaxTree, children)
        FT = Union{T,tokenstype.(children)...}
        _boundchecks(FT, N, T, token, children)
        return new{FT,T}(token, children)
    end
end

# Helpers
function SyntaxTree{FT,T}(token::T, children...) where {FT,T<:AbstractSyntaxToken}
    return SyntaxTree{FT,T}(token, children)
end
function SyntaxTree{FT}(token::T, children...) where {FT,T<:AbstractSyntaxToken}
    return SyntaxTree{FT}(token, children)
end
function SyntaxTree(token::T, children...) where {T<:AbstractSyntaxToken}
    return SyntaxTree(token, children)
end

"""
    inorder(t::SyntaxTree)::String

Performs an in-order visit of `t`, returning it as a string.
"""
function inorder(t::SyntaxTree)
    return length(children(t)) == 0 ?
           string(token(t)) :
           string(token(t)) * "(" * join([inorder(c) for c in children(t)], ", ") * ")"
end

show(io::IO, t::SyntaxTree) = print(io, inorder(t))

# Getters
token(t::SyntaxTree) = t.token
children(t::SyntaxTree) = t.children

tokentype(::SyntaxTree{FT,T}) where {FT,T} = T
tokenstype(::SyntaxTree{FT}) where {FT} = FT
# TODO2: are these intersections "fast"? because we use it in the return types of some functions -- I'm not sure how julia handles it
# TODO2: I think so, because typeintersect can be computed at compile-time.
operatorstype(t::SyntaxTree) = typeintersect(AbstractOperator, tokenstype(t))
propositionstype(t::SyntaxTree) = typeintersect(Proposition, tokenstype(t))

function Base.in(t::AbstractSyntaxToken, tree::SyntaxTree)
    return t == token(tree) || any([Base.in(t, c) for c in children(tree)])
end

"""
    tokens(t::SyntaxTree)::AbstractVector{tokenstype(t)}

Enumerates all tokens appearing in a tree.

See also [`ntokens`](@ref), [`operators`](@ref), [`propositions`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function tokens(t::SyntaxTree)::AbstractVector{tokenstype(t)}
    return AbstractSyntaxToken[vcat(tokens.(children(t))...)..., token(t)]
end

"""
    operators(t::SyntaxTree)::AbstractVector{operatorstype(t)}

Enumerates all operators appearing in a tree

See also [`noperators`](@ref), [`propositions`](@ref), [`tokens`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function operators(t::SyntaxTree)::AbstractVector{operatorstype(t)}
    ops = token(t) isa AbstractOperator ? [token(t)] : []
    return AbstractOperator[vcat(operators.(children(t))...)..., ops...]
end

"""
    propositions(t::SyntaxTree)::AbstractVector{propositionstype(t)}

Enumerates all propositions appearing in a tree

See also [`npropositions`](@ref), [`operators`](@ref), [`tokens`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function propositions(t::SyntaxTree)::AbstractVector{propositionstype(t)}
    ps = token(t) isa Proposition ? [token(t)] : []
    return Proposition[vcat(propositions.(children(t))...)..., ps...]
end

"""
    ntokens(t::SyntaxTree)::Integer

Counts all tokens appearing in a tree

See also [`tokens`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function ntokens(t::SyntaxTree)::Integer
    length(children(t)) == 0 ? 1 : 1 + sum(ntoken(c) for c in children(t))
end

"""
    npropositions(t::SyntaxTree)::Integer

Counts all propositions appearing in a tree

See also [`propositions`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function npropositions(t::SyntaxTree)::Integer
    pr = token(t) isa Proposition ? 1 : 0
    return length(children(t)) == 0 ? pr : pr + sum(npropositions(c) for c in children(t))
end

# We use standard promotion between syntax tokens and trees
Base.promote_rule(::Type{<:AbstractSyntaxToken}, ::Type{S}) where {S<:SyntaxTree} = S
Base.promote_rule(::Type{S}, ::Type{<:AbstractSyntaxToken}) where {S<:SyntaxTree} = S

Base.convert(::Type{<:SyntaxTree}, t::AbstractSyntaxToken) = SyntaxTree(t)
# Base.convert(::Type{SyntaxTree}, t::AbstractSyntaxToken) = SyntaxTree(t)
# Base.convert(::Type{S}, t::T) where {FT<:AbstractSyntaxToken, T<:FT, S<:SyntaxTree{FT, T}} = SyntaxTree(t)

"""
    abstract type AbstractGrammar{A<:AbstractAlphabet, O<:AbstractOperator} end

Abstract type for representing a
[context-free grammar](https://en.m.wikipedia.org/wiki/Context-free_grammar)
based on a *single* alphabet of type `A`, and a set of operators
that consists of all the (singleton) child types of `O`.
A context-free grammar is a simple structure for defining formulas inductively.

See also [`AbstractAlphabet`](@ref), [`AbstractOperator`](@ref).
"""
abstract type AbstractGrammar{A<:AbstractAlphabet,O<:AbstractOperator} end

operatorstype(::AbstractGrammar{A,O}) where {A,O} = O
alphabettype(::AbstractGrammar{A,O}) where {A,O} = A

"""
    alphabet(g::AbstractGrammar{A} where {A})::A

Each grammar must provide a method for accessing its propositional alphabet.

See also [`AbstractGrammar`](@ref).
"""
function alphabet(g::AbstractGrammar{A} where {A})::A
    return error("Please, provide method alphabet(::$(typeof(g))).")
end
propositionstype(g::AbstractGrammar) = eltype(alphabet(g))
tokenstype(g::AbstractGrammar) = Union{operatorstype(g),propositionstype(g)}

"""
    Base.in(t::AbstractSyntaxToken, g::AbstractGrammar)::Bool

Each grammar must provide some methods for establishing whether a syntax token belongs to
it, that is, whether it is a legal token in the grammar's formulas.

These two fallbacks are defined:

    Base.in(p::Proposition, g::AbstractGrammar) = Base.in(p, alphabet(g))
    Base.in(op::AbstractOperator, g::AbstractGrammar) = op <: operatorstype(O)

See also [`AbstractGrammar`](@ref).
"""
function Base.in(t::AbstractSyntaxToken, g::AbstractGrammar)
    return error("Please, provide method Base.in(::$(typeof(t)), ::$(typeof(g))).")
end
# Note: when using this file's syntax tokens, these methods suffice:
Base.in(p::Proposition, g::AbstractGrammar) = Base.in(p, alphabet(g))
Base.in(op::AbstractOperator, g::AbstractGrammar) = (op <: operatorstype(g))


"""
    Base.in(t::SyntaxTree, g::AbstractGrammar)::Bool

Each grammar must provide a method for establishing whether a formula,
encoded as a `SyntaxTree`, belongs to it.

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
    )::Vector{<:SyntaxTree{<:tokenstype(g)}}

Each grammar with a finite and iterable alphabet must provide a method for
enumerating its formulas, encoded as `SyntaxTree`s.

TODO2: see the general TODOs at the beginning
TODO3: which one?

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
# TODO2: ok for me, but when I'm trying to save, it removes the spaces.. perhaps there is a (background) demon that fixes it, I don't know
# TODO: I see; I think you should set your editor so that it does not automate this thing, at least for julia files.
function formulas(
    g::AbstractGrammar{A,O} where {A,O};
    maxdepth::Integer,
    nformulas::Union{Integer,Nothing} = nothing,
    args...
)::Vector{<:SyntaxTree{<:tokenstype(g)}}
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
        if operatorstype(t) <: operatorstype(g)
            true
        else
            all([Base.in(c, g) for c in children(t)])
        end
    end
end

"""
    formulas(
        g::CompleteFlatGrammar{A,O} where {A,O};
        maxdepth::Integer,
        nformulas::Union{Integer,Nothing} = nothing
    )::Vector{SyntaxTree{<:tokenstype(g)}}

Generates all formulas with syntax trees shorter than a given `maxdepth`.

See also [`AbstractGrammar`](@ref).
"""
function formulas(
    g::CompleteFlatGrammar{A,O} where {A,O};
    maxdepth::Integer,
    nformulas::Union{Integer,Nothing} = nothing,
)::Vector{SyntaxTree{<:tokenstype(g)}}
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
"""$(doc_TOP)"""
const TOP = TopOperator()
"""$(doc_TOP)"""
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
"""$(doc_BOTTOM)"""
const BOTTOM = BottomOperator()
"""$(doc_BOTTOM)"""
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

# Note: maybe one day this will have a use?
# Base.in(t::TruthValue, a::AbstractAlgebra) = Base.in(t, domain(a))

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
isfuzzy = !iscrisp

# TODO: Observe that the following docstring is out of bound (92 columns). The BlueStyle sheds light on this.
# TODO-reply: What should we do?
# TODO2: See general TODOs at the beginning
# TODO: I think this should suffice. Remove these TODOs now?
"""
    collate_truth(
        a::AbstractAlgebra,
        op::AbstractOperator,
        t::NTuple{N, T},
    )::T where {N, T<:TruthValue}

Returns the truth value of a composed formula op(φ1, ..., φN), given the `N`
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

operatorstype(l::AbstractLogic) = operatorstype(grammar(l))
alphabet(l::AbstractLogic) = alphabet(grammar(l))
propositionstype(l::AbstractLogic) = propositionstype(alphabet(l))
tokenstype(l::AbstractLogic) = tokenstype(grammar(l))
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
    Base.in(t::AbstractSyntaxToken, f::AbstractFormula)::Bool

Each formula must provide a method for establishing whether a syntax token appears in it.

See also [`AbstractSyntaxToken`](@ref).
"""
function Base.in(::AbstractSyntaxToken, f::AbstractFormula)
    return error("Please, provide method Base.in(::AbstractSyntaxToken, ::$(typeof(f))).")
end

"""
    tree(f::AbstractFormula)::SyntaxTree{<:tokenstype(logic(f))}

A formula must provide a method for extracting its syntax tree representation.

See also [`SyntaxTree`](@ref).
"""
function tree(f::AbstractFormula)::SyntaxTree{<:tokenstype(logic(f))}
    return error("Please, provide method" *
                 " tree(::$(typeof(f)))::SyntaxTree{<:$(tokenstype(logic(f)))}.")
end

Base.convert(::Type{<:SyntaxTree}, f::AbstractFormula) = tree(f)

doc_tokopprop = """
    tokens(f::AbstractFormula)::AbstractVector{tokenstype(logic(f))}
    operators(f::AbstractFormula)::AbstractVector{operatorstype(logic(f))}
    propositions(f::AbstractFormula)::AbstractVector{propositionstype(logic(f))}
    ntokens(f::AbstractFormula)::Integer
    npropositions(f::AbstractFormula)::Integer

A formula can provide a method for extracting its tokens/operators/propositions.
The fallbacks extract the tokens/operators/propositions
appearing in its syntax tree representation.

See also [`SyntaxTree`](@ref).
"""

"""$(doc_tokopprop)"""
function tokens(f::AbstractFormula)::AbstractVector{tokenstype(logic(f))}
    return tokens(tree(f))
end
"""$(doc_tokopprop)"""
function operators(f::AbstractFormula)::AbstractVector{operatorstype(logic(f))}
    return operators(tree(f))
end
"""$(doc_tokopprop)"""
function propositions(f::AbstractFormula)::AbstractVector{propositionstype(logic(f))}
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

# error("Please, provide method propositions(::$(typeof(f)))::AbstractVector{<:$(propositionstype(logic(f)))}.") # TODO: remove it?

"""
A formula can be used for instating other formulas of the same logic.

In order to use operators for composing formulas, along with syntax tokens (e.g.,
propositions) and syntax trees, each formula should specify a method for constructing
formulas of the same logic out of syntax trees. Let F<:AbstractFormula, this method should
have the following signature:

    (f::F)(t::SyntaxTree)::F # TODO: there is no return type in the definition -- please check. I know, I tried it but I cannot make it work. I think it's useful in the docstring though.
    # TODO2: We should talk on this
    # TODO: okay.

# Examples
```julia-repl
julia> f = parseformula("◊(p→q)")
julia> f2 = f("p")
julia> @assert ◊ in operators(logic(f2))
```

See also [`AbstractSyntaxToken`](@ref), [`SyntaxTree`](@ref), [`AbstractOperator`](@ref).
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

function Base._promote(x::F, y::AbstractSyntaxToken) where {F<:AbstractFormula}
    Base._promote(x, Base.convert(SyntaxTree, y))
end
Base._promote(x::Union{SyntaxTree,AbstractSyntaxToken}, y::AbstractFormula) = reverse(Base._promote(y, x))

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

See also [`AbstractLogic`](@ref).
"""
struct Formula{L<:AbstractLogic} <: AbstractFormula{L}
    _logic::Base.RefValue{L}
    tree::SyntaxTree # SyntaxTree{FT} where {FT<:tokenstype(_logic[])}

    _l(l::AbstractLogic) = Base.RefValue(l)
    _l(l::Base.RefValue) = l

    function Formula{L}(
        l::Union{L,Base.RefValue{L}},
        ttf::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula};
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
            @assert all([p in alphabet(_logic[])
                         for p in propositions(tree)]) "Cannot" *
                           " instantiate Formula{$(L)} with illegal propositions:" *
                           " $(filter((p)->!(p in alphabet(_logic[])), propositions(tree)))"
        end

        # Check that the token types of the tree are a subset of the tokens
        #  allowed by the logic
        @assert tokenstype(tree) <: tokenstype(_logic[]) "Cannot" *
                             " instantiate Formula{$(L)} with illegal token types $(tokenstype(tree))." *
                             " Token types should be <: $(tokenstype(_logic[]))."

        return new{L}(_logic, tree)
    end

    function Formula(
        l::Union{L,Base.RefValue{L}},
        ttf::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula};
        args...
    ) where {L<:AbstractLogic}
        return Formula{L}(l, ttf; args...)
    end
end

_logic(f::Formula) = f._logic # TODO: so _logic is a "private" function? Yes.
logic(f::Formula) = f._logic[] # TODO: why []? Dereferencing.
tree(f::Formula) = f.tree

Base.in(t::AbstractSyntaxToken, f::Formula) = Base.in(t, tree(f))

# When constructing a new formula from a syntax tree, the logic is passed by reference.
(f::Formula)(t::SyntaxTree) = Formula(_logic(f), t)

function (op::AbstractOperator)(children::NTuple{N,Formula}, args...) where {N}
    ls = unique(logic.(children))
    @assert length(ls) == 1 "Cannot" *
                " build formula by combination of formulas with different logics: $(ls)."
    l = first(ls)
    @assert typeof(op) <: operatorstype(l) "TODO expand" *
               " logic's set of operators (op is not in it: $(typeof(op)) ∉ $(operatorstype(l)))."
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

"""
    check(
        f::AbstractFormula,
        m::AbstractLogicalModel{A,T},
        args...
    )::T where {A, T<:TruthValue}

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
function Base.in(t::Union{AbstractSyntaxToken,SyntaxTree}, a::AbstractAlphabet)
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
(op::AbstractOperator)(children::Union{AbstractSyntaxToken,SyntaxTree}...) = op(children)
function (op::AbstractOperator)(
    children::NTuple{N,Union{AbstractSyntaxToken,SyntaxTree}}
) where {N}
    return SyntaxTree(op, children...)
end
function (op::AbstractOperator)(children::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula}...)
    return op(children)
end
function (op::AbstractOperator)(
    children::NTuple{N,Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula}}
) where {N}
    _children = map((c) -> begin
            isa(c, AbstractSyntaxToken) ? convert(SyntaxTree, c) : c
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
