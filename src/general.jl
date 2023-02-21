import Base: convert, promote_rule, _promote
import Base: eltype, in, getindex, isiterable, iterate, IteratorSize, length, isequal, hash

############################################################################################
########################################## SYNTAX ##########################################
############################################################################################

"""
    abstract type AbstractSyntaxToken end

A token in a syntax tree.
A syntax tree is a tree-like structure representing a logical formula, where each
node holds a *token*, and has as many children as the `arity` of the token.

See also [`SyntaxTree`](@ref), [`arity`](@ref), [`syntaxstring`](@ref).
"""
abstract type AbstractSyntaxToken end

"""
    arity(::Type{<:AbstractSyntaxToken})::Integer
    arity(t::AbstractSyntaxToken)::Integer = arity(typeof(t))

Provides the `arity` of a syntax token. The arity of a syntax token is an integer
representing the number of allowed children in a `SyntaxTree`. Tokens with `arity` equal
to 0, 1 or 2 are called `nullary`, `unary` and `binary`, respectively.

See also [`AbstractSyntaxToken`](@ref).
"""
arity(TOK::Type{<:AbstractSyntaxToken})::Integer = error("Please, provide method arity(::$(typeof(TOK))).")
arity(tok::AbstractSyntaxToken)::Integer = arity(typeof(tok))

# Helpers: TODO move to SoleBase?
isnullary(a) = arity(a) == 0
isunary(a) = arity(a) == 1
isbinary(a) = arity(a) == 2

"""
    syntaxstring(φ::AbstractFormula; kwargs...)::String
    syntaxstring(t::SyntaxTree; kwargs...)::String
    syntaxstring(tok::AbstractSyntaxToken; kwargs...)::String

Produces the string representation of a formula, syntax token or syntax tree by performing
a tree traversal. Note that this representation may introduce redundant parenthesis.
`kwargs` can be used to specify how to display syntax tokens/trees under
some specific conditions.

The following `kwargs` are currently supported:
- `function_notation = false::Bool`: when set to `true`, it forces the use of
function notation for binary operators.
See (here)[https://en.m.wikipedia.org/wiki/Infix_notation].

See also [`parseformula`](@ref), [`parseformulatree`](@ref),
[`SyntaxTree`](@ref), [`AbstractSyntaxToken`](@ref).

# Examples
```julia-repl
julia> syntaxstring((parseformula("◊((p∧s)→q)")))
"(◊(p ∧ s)) → q"

julia> syntaxstring((parseformula("◊((p∧s)→q)")); function_notation = true)
"→(◊(∧(p, s)), q)"
```

# Extended help

In the case of a syntax tree or formula, `syntaxstring` is a recursive function that calls
itself on the syntax children of each node. For a correct functioning, the `syntaxstring` 
must be defined (including `kwargs...`) for every newly defined
`AbstractSyntaxToken` (e.g., operators and `Proposition`s).
In particular, for the case of `Proposition`s, the function calls itself on the atom:

    syntaxstring(p::Proposition; kwargs...) = syntaxstring(atom(p); kwargs...)

Then, the syntaxstring for a given atom can be defined. For example, with string atoms,
the function can simply be:
    
    syntaxstring(atom::String; kwargs...) = atom

"""
function syntaxstring(tok::AbstractSyntaxToken; kwargs...)::String
    error("Please, provide method syntaxstring(::$(typeof(tok)); kwargs...).")
end

# Helper
syntaxstring(atom::Union{String,Number}; kwargs...) = string(atom)

############################################################################################

"""
    struct Proposition{A} <: AbstractSyntaxToken
        atom::A
    end

A proposition, sometimes called a propositional letter (or simply *letter*), of type
`Proposition{A}` wraps a value
`atom::A` representing a fact which truth can be assessed on a logical interpretation.

Propositions are nullary tokens (i.e, they are at the leaves of a syntax tree).
Note that their atom cannot be a Proposition.

See also [`AbstractSyntaxToken`](@ref), [`AbstractInterpretation`](@ref), [`check`](@ref).
"""
struct Proposition{A} <: AbstractSyntaxToken
    atom::A

    function Proposition{A}(atom::A) where {A}
        @assert !(atom isa Proposition) "Illegal nesting." *
            " Cannot instantiate Proposition with atom of type $(typeof(atom))"
        new{A}(atom)
    end
    function Proposition(atom::A) where {A}
        Proposition{A}(atom)
    end
end

atom(p::Proposition) = p.atom

arity(::Type{<:Proposition}) = 0
atomtype(::Proposition{A}) where {A} = A
atomtype(::Type{Proposition{A}}) where {A} = A

# TODO remove:
# Base.show(io::IO, t::Proposition) = print(io, atom(t))

# Helper
Base.convert(::Type{P1}, t::P2) where {P1<:Proposition,P2<:Proposition} = P1(atom(t))

syntaxstring(p::Proposition; kwargs...) = syntaxstring(atom(p); kwargs...)

"""
    inverse(p::Proposition) = Proposition(inverse(atom(p)))

Returns the inverse of a proposition `p`, that is, a `Proposition` which inverted semantics
with respect to `p`. In a crisp propositional logic, for example, the inverse proposition
is the one which is true whenever `p` is false, and viceversa.

See also [`Proposition`](@ref), [`check`](@ref).
"""
inverse(p::Proposition) = Proposition(inverse(atom(p)))

############################################################################################

"""
    abstract type AbstractOperator <: AbstractSyntaxToken end

An operator is a [logical constant](https://en.m.wikipedia.org/wiki/Logical_connective)
which establishes a relation between propositions (i.e., facts).
For example, the boolean operators AND, OR and IMPLIES (stylized as ∧, ∨ and →)
are used to connect propositions and express derived concepts.

Since operators display very different algorithmic behaviors,
all `struct`s that are subtypes of `AbstractOperator` must
be parametric singleton types, which can be dispatched upon.

See also [`AbstractSyntaxToken`](@ref), [`NamedOperator`](@ref), [`check`](@ref).
"""
abstract type AbstractOperator <: AbstractSyntaxToken end

# Since, in general, operators are singletons, we show them via their syntaxstring
Base.show(io::IO, o::AbstractOperator) = print(io, syntaxstring(o))

doc_iscommutative = """
    iscommutative(::Type{AbstractOperator}) = false
    iscommutative(o::AbstractOperator) = iscommutative(typeof(o))

Returns whether it is known that an `AbstractOperator` is commutative.

# Examples
```julia-repl
julia> iscommutative(∧)
true
julia> iscommutative(→)
false
```

# Extended help

Since nullary and unary operators are always commutative,
this function is actually implemented as:

    iscommutative(O::Type{<:AbstractOperator}) = isnullary(O) || isunary(O) || _iscommutative(O)
    iscommutative(o::AbstractOperator) = iscommutative(typeof(o))
    _iscommutative(::Type{<:AbstractOperator}) = false

When defining new operators `O`, provide a method `_iscommutative`, such as:
    
    _iscommutative(::Type{typeof(∧)}) = true
    # TODO example with xor?

See also [`isunary`](@ref), [`isnullary`](@ref).
"""

"""$(doc_iscommutative)"""
iscommutative(O::Type{<:AbstractOperator}) = isnullary(O) || isunary(O) || _iscommutative(O)
iscommutative(o::AbstractOperator) = iscommutative(typeof(o))
"""$(doc_iscommutative)"""
_iscommutative(::Type{<:AbstractOperator}) = false

############################################################################################

"""
    abstract type AbstractAlphabet{A} end

Abstract type for representing an alphabet of propositions with atoms of type `A`.
An alphabet (or propositional alphabet) is assumed to be a
[countable](https://en.m.wikipedia.org/wiki/Countable_set) set of propositions.

See also [`ExplicitAlphabet`](@ref), [`AlphabetOfAny`](@ref),
[`propositionstype`](@ref), [`atomtype`](@ref),
[`Proposition`](@ref), [`AbstractGrammar`](@ref).
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

# Helper
function Base.in(atom::Union{String,Number}, a::AbstractAlphabet)
    @warn "Please, use Base.in(Proposition($(atom)), alphabet::$(typeof(a))) instead of" *
        " Base.in($(atom), alphabet::$(typeof(a)))"
    Base.in(Proposition(atom), a)
end

doc_iterable = """
Each alphabet must specify whether it is *iterable* or not.
An alphabet is iterable if it provides the (two) `iterate` methods required by the
[iteration interface](https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration).

By default, an alphabet is considered iterable:

    Base.isiterable(::Type{<:AbstractAlphabet}) = true
    Base.isiterable(a::AbstractAlphabet) = Base.isiterable(typeof(a))
    Base.iterate(a::AbstractAlphabet) = error(...)
    Base.iterate(a::AbstractAlphabet, state) = error(...)

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""

"""$(doc_iterable)"""
Base.isiterable(::Type{<:AbstractAlphabet}) = true
Base.isiterable(a::AbstractAlphabet) = Base.isiterable(typeof(a))
"""$(doc_iterable)"""
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

doc_finite = """
Each alphabet must specify whether it is finite.
An alphabet is finite iff it provides the `length` method.

By default, an alphabet is considered finite:

    Base.isfinite(::Type{<:AbstractAlphabet}) = true
    Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))
    Base.length(a::AbstractAlphabet) = error(...)

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""

"""$(doc_finite)"""
Base.isfinite(::Type{<:AbstractAlphabet}) = true
Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))

"""$(doc_finite)"""
function Base.length(a::AbstractAlphabet)
    if isfinite(a)
        return error("Please, provide method Base.length(::$(typeof(a)))," *
                     " or define Base.isfinite(::$(typeof(a))) = false.")
    else
        return error("Cannot compute length of alphabet of type $(typeof(a)).")
    end
end

# [Iteration interface](https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration) util.
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

An alphabet wrapping propositions in a (finite) `Vector`.

See also [`propositions`](@ref), [`AbstractAlphabet`](@ref).
"""
struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
    propositions::Vector{Proposition{A}}

    function ExplicitAlphabet{A}(propositions) where {A}
        return new{A}(collect(propositions))
    end

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

An implicit, infinite alphabet that includes all propositions with atoms of a subtype of A.

See also [`AbstractAlphabet`](@ref).
"""
struct AlphabetOfAny{A} <: AbstractAlphabet{A} end
Base.in(::Proposition{PA}, ::AlphabetOfAny{AA}) where {PA,AA} = (PA <: AA)
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.isiterable(::Type{<:AlphabetOfAny}) = false

############################################################################################

"""
    struct SyntaxTree{FT<:AbstractSyntaxToken,T<:FT}
        token::T
        children::NTuple{N,SyntaxTree} where {N}
    end

A syntax tree encoding a logical formula.
Each node of the syntax tree holds a `token::T`, and
has as many children as the `arity` of the token.

This implementation is *arity-compliant*, in that, upon construction, 
the arity is checked against the number of children provided.
An additional type parameter `FT` ensures that the token types of the sub-tree are
constrained to a predefined set of types.
When it is not specified, this parameter defaults to the `Union` between `T`, and the `FT`s
of the child nodes. Note: if not handled correctly, this can cause an abuse of Julia's
[multiple dispatch engine](https://docs.julialang.org/en/v1/manual/performance-tips/#The-dangers-of-abusing-multiple-dispatch-(aka,-more-on-types-with-values-as-parameters)).

See also [`token`](@ref), [`children`](@ref), [`tokentype`](@ref),
[`tokens`](@ref), [`operators`](@ref), [`propositions`](@ref),
[`ntokens`](@ref), [`npropositions`](@ref), [`height`](@ref),
[`tokenstype`](@ref), [`operatorstype`](@ref), [`propositionstype`](@ref),
[`AbstractSyntaxToken`](@ref), [`arity`](@ref), [`Proposition`](@ref), [`Operator`](@ref).
"""
struct SyntaxTree{FT<:AbstractSyntaxToken,T<:AbstractSyntaxToken} # T<:FT

    # The syntax token at the current node
    token::T
    
    # The child nodes of the current node
    children::NTuple{N,SyntaxTree} where {N}

    function _boundchecks(FT, N, T, token, children)
        @assert arity(token) == N "Cannot instantiate SyntaxTree{$(FT),$(T)} with token" *
                                  " $(token) of arity $(arity(token)) and $(N) children."
        @assert all([T, tokenstype.(children)...] .<: FT) "" *
                "Cannot instantiate SyntaxTree{$(FT),$(T)} with token::$(T) and" *
                " tokenstype.(children)::$(tokenstype.(children))."
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

    function SyntaxTree{FT}(
        t::SyntaxTree{FT2,T},
    ) where {FT<:AbstractSyntaxToken,T<:FT,FT2}
        return SyntaxTree{FT,T}(token(t), children(t))
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

# Shows the type of the syntax tree and its syntaxstring.
# Base.show(io::IO, t::SyntaxTree) = print(io, "$(typeof(t))($(syntaxstring(t)))")
function Base.show(io::IO, t::SyntaxTree)
    println(io, "SyntaxTree: $(syntaxstring(t))")
    print(io, "Allowed token types: $(tokenstype(t))")
end


# Getters
token(t::SyntaxTree) = t.token
children(t::SyntaxTree) = t.children

tokentype(::SyntaxTree{FT,T}) where {FT,T} = T
tokenstype(::SyntaxTree{FT,T}) where {FT,T} = FT
operatorstype(t::SyntaxTree) = typeintersect(AbstractOperator, tokenstype(t))
propositionstype(t::SyntaxTree) = typeintersect(Proposition, tokenstype(t))

"""
    Base.in(t::AbstractSyntaxToken, tree::SyntaxTree)::Bool

Returns whether a token appears in a tree or not.

See also [`tokens`](@ref), [`SyntaxTree`](@ref).
"""
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

See also [`noperators`](@ref), [`propositions`](@ref), [`tokens`](@ref), [`AbstractOperator`](@ref).
""" # TODO explain that when applied to formula/tree, it does this, but with a logic it shows the operators in the grammar.
function operators(t::SyntaxTree)::AbstractVector{operatorstype(t)}
    ops = token(t) isa AbstractOperator ? [token(t)] : []
    return AbstractOperator[vcat(operators.(children(t))...)..., ops...]
end

"""
    propositions(t::SyntaxTree)::AbstractVector{Proposition}

Enumerates all propositions appearing in a tree

See also [`npropositions`](@ref), [`operators`](@ref), [`tokens`](@ref), [`Proposition`](@ref).
"""
function propositions(t::SyntaxTree)::AbstractVector{Proposition}
    ps = token(t) isa Proposition ? Proposition[token(t)] : Proposition[]
    return Proposition[vcat(propositions.(children(t))...)..., ps...]
end

# TODO why doesn't this work?
# function propositions(t::SyntaxTree)::AbstractVector{propositionstype(t)}
#     ps = token(t) isa Proposition ? propositionstype(t)[token(t)] : propositionstype(t)[]
#     return propositionstype(t)[vcat(propositions.(children(t))...)..., ps...]
# end

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

"""
    height(t::SyntaxTree)::Integer

Counts all tokens appearing in a tree

See also [`tokens`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function height(t::SyntaxTree)::Integer
    length(children(t)) == 0 ? 0 : 1 + maximum(height(c) for c in children(t))
end

# We use standard promotion between syntax tokens and trees
Base.promote_rule(::Type{<:AbstractSyntaxToken}, ::Type{S}) where {S<:SyntaxTree} = S
Base.promote_rule(::Type{S}, ::Type{<:AbstractSyntaxToken}) where {S<:SyntaxTree} = S

Base.convert(::Type{S}, t::AbstractSyntaxToken) where {S<:SyntaxTree} = S(t)
# TODO remove
# Base.convert(::Type{SyntaxTree}, t::AbstractSyntaxToken) = SyntaxTree(t)
# Base.convert(::Type{S}, t::T) where {FT<:AbstractSyntaxToken, T<:FT, S<:SyntaxTree{FT,T}} = SyntaxTree(t)

# Helpers that make SyntaxTree's map to the same dictionary key. Useful for checking formulas on interpretations.
function Base.isequal(a::SyntaxTree, b::SyntaxTree)
    Base.isequal(token(a), token(b)) && all(((c1,c2),)->Base.isequal(c1,c2), zip(children(a), children(b)))
end
Base.hash(a::SyntaxTree) = Base.hash(syntaxstring(a))

# Refer to syntaxstring(tok::AbstractSyntaxToken; kwargs...) for documentation
function syntaxstring(t::SyntaxTree; function_notation = false, kwargs...)

    tok = token(t)
    if arity(tok) == 0
        syntaxstring(tok; function_notation = function_notation, kwargs...)
    elseif arity(tok) == 2 && !function_notation
        f = ch->arity(token(ch)) == 0 ? "$(syntaxstring(ch; function_notation = function_notation, kwargs...))" : "($(syntaxstring(ch; function_notation = function_notation, kwargs...)))"
        # Infix notation for binary operator
        "$(f(children(t)[1])) $(syntaxstring(tok; function_notation = function_notation, kwargs...)) $(f(children(t)[2]))"
    else
        # Function notation for higher arity operator
        length(children(t)) == 0 ?
               syntaxstring(tok; function_notation = function_notation, kwargs...) :
               syntaxstring(tok; function_notation = function_notation, kwargs...) * "(" *
                    join([syntaxstring(c; function_notation = function_notation, kwargs...) for c in children(t)], ", ") *
                ")"
        # "$(syntaxstring(tok; kwargs...))(" * join(map((c)->("($(syntaxstring(c; kwargs...)))"), children(t)), ",") * ")"
    end
end

############################################################################################

"""
    abstract type AbstractGrammar{A<:AbstractAlphabet,O<:AbstractOperator} end

Abstract type for representing a
[context-free grammar](https://en.m.wikipedia.org/wiki/Context-free_grammar)
based on a *single* alphabet of type `A`, and a set of operators
that consists of all the (singleton) child types of `O`.
A context-free grammar is a simple structure for defining formulas inductively.

See also [`alphabet`](@ref),
[`propositionstype`](@ref), [`tokenstype`](@ref), 
[`operatorstype`](@ref), [`alphabettype`](@ref),
[`AbstractAlphabet`](@ref), [`AbstractOperator`](@ref).
"""
abstract type AbstractGrammar{A<:AbstractAlphabet,O<:AbstractOperator} end

operatorstype(::AbstractGrammar{A,O}) where {A,O} = O
alphabettype(::AbstractGrammar{A,O}) where {A,O} = A

"""
    alphabet(g::AbstractGrammar{A} where {A})::A

Returns the propositional alphabet of a grammar.

See also [`AbstractAlphabet`](@ref), [`AbstractGrammar`](@ref).
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
        nformulas::Union{Integer,Nothing} = nothing,
        args...
    )::Vector{<:SyntaxTree{<:tokenstype(g)}}

Each grammar with a finite and iterable alphabet must provide a method for
enumerating its formulas, encoded as `SyntaxTree`s.

Additional `args` can be used to model the function's behavior.
At least these two arguments should be covered:
- a `nformulas` argument can be used to limit the size of the returned `Vector`;
- a `maxdepth` argument can be used to limit the result to syntax trees of a given
maximum depth;

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
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
    struct CompleteFlatGrammar{A<:AbstractAlphabet,O<:AbstractOperator} <: AbstractGrammar{A,O}
        alphabet::A
        operators::Vector{<:O}
    end

Grammar that generates all well-formed formulas obtained by the arity-complying composition
of propositions of an alphabet of type `A`, and all operators in `operators`.
With n operators, this grammar has exactly n+1 production rules, and
m+1 terminal symbols, where m is the number of nullary operators.
For example, with `operators = [⊥,∧,∨]`, the grammar is:

    T ::= p | ⊥ | T ∧ T | T ∨ T

with p ∈ alphabet. Note: it is *flat* in the sense that all rules substitute the same
(unique and starting) non-terminal symbol T.

See also [`alphabet`](@ref), [`operators`](@ref),
[`nonterminals`](@ref), [`terminals`](@ref),
[`formulas`](@ref),
[`AbstractOperator`](@ref), [`AbstractGrammar`](@ref).
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
    # With increasing `depth`, accumulate all formulas of length `depth` by combining all
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
In the crisp case, `Bool` values are used. In the fuzzy case, other values can be used.
For example, `AbstractFloat`s can be used with chain algebras,
and 0.0 and 1.0 are the `bottom` and `top`.

See also [`top`](@ref), [`bottom`](@ref), [`tops`](@ref), [`bottoms`](@ref), [`Algebra`](@ref).
"""
const TruthValue = Any

"""
    tops(::TruthValue)::Bool

Returns true if the truth value is the top of its algebra.
For example, in the crisp case, with `Bool` truth values, it is:

    tops(t::Bool)::Bool = (t == true)

See also [`bottoms`](@ref), [`TruthValue`](@ref).
"""
tops(t::TruthValue)::Bool = error("Please, provide method tops(truthvalue::$(typeof(t))).")

"""
    bottoms(::TruthValue)::Bool

Returns true if the truth value is the bottom of its algebra.
For example, in the crisp case, with `Bool` truth values, it is:

    bottoms(t::Bool)::Bool = (t == false)

"""
bottoms(t::TruthValue)::Bool = error("Please, provide method bottoms(truthvalue::$(typeof(t))).")

"""
   default_algebra(::Type{T})::AbstractAlgebra{<:T} where {T<:TruthValue}

In order to check syntax trees without algebras, each truth value should provide
a default algebra it works with.
"""
function default_algebra(::Type{T})::AbstractAlgebra{<:T} where {T<:TruthValue}
    return error("Please, provide method" *
                 " default_algebra(::$(typeof(T)))::AbstractAlgebra{<:$(T)}.")
end

############################################################################################

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

syntaxstring(o::TopOperator; kwargs...) = "⊤"

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

syntaxstring(o::BottomOperator; kwargs...) = "⊥"

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

value(op::TruthOperator) = op.value

syntaxstring(o::TruthOperator; kwargs...) = syntaxstring(value(o))

############################################################################################

"""
    abstract type AbstractAlgebra{T<:TruthValue} end

Abstract type for representing algebras. Algebras are used for grounding the
truth of propositions and the semantics of operators. They typically encode a
[lattice structure](https://en.m.wikipedia.org/wiki/Lattice_(order)) where two
elements(or nodes) *⊤* and *⊥* are referred to as *top* (or maximum)
and *bottom* (or minimum). Each node in the lattice represents a truth value
that a proposition or a formula can have on an interpretation, and the
semantics of operators is given in terms of operations between truth values.

See also [`domain`](@ref), [`top`](@ref), [`bottom`](@ref),
[`truthtype`](@ref), [`iscrisp`](@ref),
[``BooleanAlgebra`](@ref), [`AbstractOperator`](@ref), [`collatetruth`](@ref).
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

############################################################################################

"""
    abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

Abstract type of a logic, which comprehends a context-free grammar (syntax) and
an algebra (semantics).

See also [`AbstractGrammar`](@ref), [`AbstractAlgebra`](@ref).
"""
abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

"""
    grammar(l::AbstractLogic{G})::G where {G<:AbstractGrammar}

A logic must provide a method for accessing its grammar.

See also [`grammar`](@ref), [`algebra`](@ref),
[`operators`](@ref), [`alphabet`](@ref),
[`truthtype`](@ref),
[`formulas`](@ref),
[`AbstractGrammar`](@ref), [`AbstractLogic`](@ref).
"""
function grammar(l::AbstractLogic{G})::G where {G}
    return error("Please, provide method grammar(::$(typeof(l))).")
end

operatorstype(l::AbstractLogic) = operatorstype(grammar(l))
alphabettype(l::AbstractLogic) = alphabettype(grammar(l))
operators(l::AbstractLogic) = operators(grammar(l))
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

# TODO @Mauro, in cases like these, can we use @forward?
truthtype(l::AbstractLogic) = truthtype(algebra(l))
top(l::AbstractLogic) = top(algebra(l))
bottom(l::AbstractLogic) = bottom(algebra(l))
iscrisp(l::AbstractLogic) = iscrisp(algebra(l))

############################################################################################

"""
    abstract type AbstractFormula{L<:AbstractLogic} end

A formula encodes a statement, anchored to a certain logic,
which truth can be evaluated on interpretations (or models) of the logic.

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

function syntaxstring(f::AbstractFormula; kwargs...)
    syntaxstring(tree(f); kwargs...)
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
"""$(doc_tokopprop)"""
function height(f::AbstractFormula)::Integer
    return height(tree(f))
end

"""
*Cool feature!*

A formula can be used for instating other formulas of the same logic.

In order to use operators for composing formulas, along with syntax tokens (e.g.,
propositions) and syntax trees, each formula should specify a method for constructing
formulas of the same logic out of syntax trees. Let F<:AbstractFormula, this method should
have the following signature:

    (f::F)(t::SyntaxTree)::F

# Examples
```julia-repl
julia> f = parseformula("◊(p→q)");

julia> f2 = f(parseformulatree("p"));

julia> syntaxstring(f)
"◊(→(p, q))"

julia> syntaxstring(f2)
"p"

julia> @assert logic(f) == logic(f2)

julia> @assert ◊ in operators(logic(f2))

julia> @assert ◊ isa operatorstype(logic(f2))

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

    (op::AbstractOperator)(children::NTuple{N,F}, args...) where {N,F<:AbstractFormula}

Note that, since `op` might not be in the logic of the child formulas,
the resulting formula may be of a different logic.

See also [`AbstractFormula`](@ref), [`SyntaxTree`](@ref), [`AbstractOperator`](@ref).
"""
function (op::AbstractOperator)(::NTuple{N,F}, args...)::F where {N,F<:AbstractFormula}
    return error("Please, provide method
        (op::AbstractOperator)(children::NTuple{N,$(F)}, args...) where {N}.")
end

"""
    struct Formula{L<:AbstractLogic} <: AbstractFormula{L}
        _logic::Base.RefValue{L}
        tree::SyntaxTree
    end

In the most general case, a formula encodes a syntax tree that is anchored to
a certain logic; that is: a) the tree encodes a formula belonging to the grammar
of the logic; b) the truth of the formula can be evaluated
on interpretations of the logic. Note that, here, the logic is represented by a reference.

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

_logic(f::Formula) = f._logic
logic(f::Formula) = f._logic[]
tree(f::Formula) = f.tree
# Helper
tree(f::SyntaxTree) = f

Base.in(t::AbstractSyntaxToken, f::Formula) = Base.in(t, tree(f))

function Base.show(io::IO, f::Formula)
    println(io, "Formula: $(syntaxstring(f))")
    print(io, "Anchored to ")
    Base.show(io, logic(f))
end

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

# Helpers that make SyntaxTree's and Formula's map to the same
#  dictionary key. Useful for checking formulas on interpretations.
function Base.isequal(a::Union{Formula,SyntaxTree}, b::Union{Formula,SyntaxTree})
    Base.isequal(tree(a), tree(b))
end
Base.hash(a::Formula) = Base.hash(tree(a))

############################################################################################

"""
    abstract type AbstractInterpretation{A,T<:TruthValue} end

Abstract type for representing a propositional
[interpretation](https://en.m.wikipedia.org/wiki/Interpretation_(logic))
(or propositional model)
that associates truth values of a type `T` to propositional letters of atom type `A`.
In the case of
[propositional logic](https://simple.m.wikipedia.org/wiki/Propositional_logic),
is essentially a map *proposition → truth value*.

Properties expressed via logical formulas can be `check`ed on logical interpretations.

See also [`check`](@ref), [`Assigment`](@ref), [`AbstractKripkeStructure`](@ref).
"""
abstract type AbstractInterpretation{A,T<:TruthValue} end

atomtype(::AbstractInterpretation{A,T}) where {A,T} = A
truthtype(::AbstractInterpretation{A,T}) where {A,T} = T

"""
    check(
        f::AbstractFormula,
        m::AbstractInterpretation{A,T},
        args...
    )::T where {A,T<:TruthValue}

Checks a formula on a logical interpretation (or model), returning a truth value.
This process is referred to as
[model checking](https://en.m.wikipedia.org/wiki/Model_checking), and there are many
algorithms for it, typically depending on the complexity of the logic.

See also [`AbstractFormula`](@ref), [`AbstractInterpretation`](@ref).

"""
function check(
    f::AbstractFormula,
    m::AbstractInterpretation{A,T},
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


# Helper: use default algebra when checking a syntax tree.
function check(tree::SyntaxTree, m::AbstractInterpretation, args...)
    @warn "Checking SyntaxTree on an $(typeof(m)). Please, consider wrapping" *
        " the tree into a Formula before checking."
    return check(default_algebra(truthtype(m)), tree, m, args...)
end
