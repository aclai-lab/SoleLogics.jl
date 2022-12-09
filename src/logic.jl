import Base: convert, promote_rule, _promote
import Base: eltype, in, isiterable, iterate, IteratorSize, length

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

    arity(t::Type{<:SyntaxToken})::Integer
    arity(t::SyntaxToken)::Integer

The arity of a token is the expected number of children of a node that
wraps it in a syntax tree.
See also [`SyntaxToken`](@ref).
"""
arity(t::Type{<:SyntaxToken})::Integer = error("Please, provide method arity(::$(typeof(t)).")
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
atom(p::Proposition) = p.atom

Base.convert(::Type{P1}, t::P2) where {P1<:Proposition, P2<:Proposition} = P1(atom(t))

"""
    abstract type AbstractOperator <: SyntaxToken end

An operator is a [logical constant](https://en.m.wikipedia.org/wiki/Logical_connective)
which establishes a relation between propositions (i.e., facts).
For example, the boolean operators AND, OR and IMPLIES (stylized as ∧, ∨ and ⟹)
are used to connect propositions and express derived concepts.

Since operators display very different algorithmic behaviors,
all `struct`'s that are subtypes of `AbstractOperator` must 
be parametric singleton types, which can be dispatched upon.

See also [`SyntaxToken`](@ref), [`NamedOperator`](@ref), [`check`](@ref).
"""
abstract type AbstractOperator <: SyntaxToken end

isnullary(op::Type{<:AbstractOperator}) = arity(op) == 0
isunary(op::Type{<:AbstractOperator}) = arity(op) == 1
isbinary(op::Type{<:AbstractOperator}) = arity(op) == 2


"""
    abstract type AbstractAlphabet{A} end

Abstract type for representing an alphabet of propositions with atoms of type `A`.
An alphabet (or propositional alphabet) is a
[https://en.m.wikipedia.org/wiki/Countable_set](countable) set of propositions.

See also [`Proposition`](@ref), [`AbstractGrammar`](@ref).
"""
abstract type AbstractAlphabet{A} end

Base.eltype(::Type{<:AbstractAlphabet{A}}) where {A} = Proposition{A}
propositiontype(a::Type{<:AbstractAlphabet}) = eltype(a)
propositiontype(a::AbstractAlphabet) = propositiontype(typeof(a))

"""
Each alphabet must provide a method for establishing whether
a proposition belongs or not to it:

    Base.in(p::Proposition, a::AbstractAlphabet)::Bool
    
See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
function Base.in(p::Proposition{A}, a::AbstractAlphabet)::Bool where {A}
    if A <: eltype(a)
        error("Please, provide method Base.in(::Proposition, ::$(typeof(a))).")
    else
        # or `false`?
        error("Cannot establish whether proposition $(p) of type $(typeof(p)) is in alphabet $(a) of type $(typeof(a)) and eltype $(eltype(a)).")
    end
end

# Helper
Base.in(o::Any, a::AbstractAlphabet) = Base.in(Proposition(o), a) # error("Attempting Base.in($(typeof(o)), ::$(typeof(a))), but only Proposition's can belong to alphabets.")

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
Base.iterate(a::AbstractAlphabet) = error(isiterable(a) ? "Please, provide method Base.iterate(::$(typeof(a))), or define Base.isiterable(::$(typeof(a))) = false." : "Cannot iterate infinite alphabet of type $(typeof(a)).")
Base.iterate(a::AbstractAlphabet, state) = error(isiterable(a) ? "Please, provide method Base.iterate(::$(typeof(a)), state), or define Base.isiterable(::$(typeof(a))) = false." : "Cannot iterate infinite alphabet of type $(typeof(a)).")

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
Base.length(a::AbstractAlphabet) = error(Base.isfinite(a) ? "Please, provide method Base.length(::$(typeof(a))), or define Base.isfinite(::$(typeof(a))) = false." : "Cannot compute length of alphabet of type $(typeof(a)).")

# [https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration](Iteration interface) util.
Base.IteratorSize(::Type{M}) where {M<:AbstractAlphabet} = Base.isfinite(M) ? Base.HasLength() : Base.IsInfinite()

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
            collect(a)
        else
            error("Please, provide method propositions(::$(typeof(a))). Note: attempting at iterating through an infinite alphabet.")
        end
    else
       error("Cannot list propositions of an alphabet of type $(typeof(a)).")
    end
end

function propositions(a::AbstractAlphabet, args...)::AbstractVector{propositiontype(a)}
    error("Please, provide method propositions(::$(typeof(a)), args...) for a bounded iteration through an infinite alphabet.")
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
        new{A}(collect(propositions))
    end

    function ExplicitAlphabet(propositions::AbstractVector{Proposition{A}}) where {A}
        ExplicitAlphabet{A}(collect(propositions))
    end

    function ExplicitAlphabet(propositions::AbstractVector{A}) where {A}
        ExplicitAlphabet{A}(Proposition.(collect(propositions)))
    end
end
Base.in(p::Proposition, a::ExplicitAlphabet) = Base.in(p, a.propositions)
Base.iterate(a::ExplicitAlphabet) = Base.iterate(a.propositions)
Base.iterate(a::ExplicitAlphabet, state) = Base.iterate(a.propositions, state)
Base.length(a::ExplicitAlphabet) = length(a.propositions)

# TODO
# """
#     struct ExplicitAlphabet{A} <: AbstractAlphabet{A}

# An alphabet wrapping a generator of propositions.
# See also [`AbstractAlphabet`](@ref).
# """
# struct LazyAlphabet{A, G} <: AbstractAlphabet{A}
#     propositions::G

#     function LazyAlphabet{A, G}(propositions::G) where {A, G}
#         new{A, G}(propositions)
#     end

#     function LazyAlphabet{A}(propositions::G) where {A, G}
#         LazyAlphabet{A, G}(propositions)
#     end

#     function LazyAlphabet(propositions::G) where {G}
#         A = eltype(propositions)
#         LazyAlphabet{A}(propositions)
#     end
# end
# Base.in(p::Proposition, a::ExplicitAlphabet) = Base.in(p, a.propositions)
# Base.iterate(a::ExplicitAlphabet) = Base.iterate(a.propositions)
# Base.iterate(a::ExplicitAlphabet, state) = Base.iterate(a.propositions, a)
# Base.length(a::ExplicitAlphabet) = length(a.propositions)

"""
    struct ExplicitAlphabet{A} <: AbstractAlphabet{A}

An implicit infinite alphabet that includes all propositions with atoms of a subtype of A.
"""
struct AlphabetOfAny{A} <: AbstractAlphabet{A} end
Base.in(p::Proposition{AA}, a::AlphabetOfAny{A}) where {A, AA} = (AA<:A)
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.isiterable(::Type{<:AlphabetOfAny}) = false

############################################################################################
############################################################################################
############################################################################################

"""
    struct SyntaxTree{FT<:SyntaxToken, T<:FT}
        token::T
        children::NTuple{N, SyntaxTree} where {N}
    end

A syntax tree encoding a logical formula.
Each node of the syntax tree holds a `token::T`, and
has as many children as the `arity` of the token.

This implementation is *safe*, in that, this arity check is performed upon construction.
An additional type parameter `FT` ensures that the the token types of the sub-tree
are constrained to a predefined set of types.
When it is not specified, this parameter defaults to the `Union` between `T`, and the `FT`'s
of the child nodes.

See also [`SyntaxToken`](@ref), [`arity`](@ref), [`Proposition`](@ref), [`Operator`](@ref).
"""
struct SyntaxTree{FT<:SyntaxToken, T<:SyntaxToken} # T<:FT
    token::T
    children::NTuple{N, SyntaxTree} where {N}

    function _boundchecks(FT, N, T, token, children)
        @assert arity(token) == N "Cannot instantiate SyntaxTree{$(FT), $(T)} with token $(token) of arity $(arity(token)) and $(N) children."
        @assert all([T, tokentypes.(children)...] .<: FT) "Cannot instantiate SyntaxTree{$(FT), $(T)} with token::$(T) and children of tokens::$(tokentypes.(children))."
    end

    function SyntaxTree{FT, T}(
        token::T,
        children::NTuple{N, Union{SyntaxToken, SyntaxTree}} = (),
    ) where {FT<:SyntaxToken, T<:FT, N}
        children = convert.(SyntaxTree, children)
        _boundchecks(FT, N, T, token, children)
        new{FT, T}(token, children)
    end

    function SyntaxTree{FT}(
        token::T,
        children::NTuple{N, Union{SyntaxToken, SyntaxTree}} = (),
    ) where {FT<:SyntaxToken, T<:FT, N}
        children = convert.(SyntaxTree, children)
        _boundchecks(FT, N, T, token, children)
        new{FT, T}(token, children)
    end

    function SyntaxTree(
        token::T,
        children::NTuple{N, Union{SyntaxToken, SyntaxTree}} = (),
    ) where {T<:SyntaxToken, N}
        children = convert.(SyntaxTree, children)
        FT = Union{T,tokentypes.(children)...}
        # FT = SyntaxToken
        _boundchecks(FT, N, T, token, children)
        new{FT, T}(token, children)
    end
end

# Helpers
function SyntaxTree{FT, T}(token::T, children...) where {FT, T<:SyntaxToken}
    SyntaxTree{FT, T}(token, children)
end
function SyntaxTree{FT}(token::T, children...) where {FT, T<:SyntaxToken}
    SyntaxTree{FT}(token, children)
end
function SyntaxTree(token::T, children...) where {T<:SyntaxToken}
    SyntaxTree(token, children)
end

# Getters
token(t::SyntaxTree) = t.token
children(t::SyntaxTree) = t.children

tokentype(::SyntaxTree{FT, T}) where {FT, T} = T
tokentypes(::SyntaxTree{FT}) where {FT} = FT
propositiontypes(t::SyntaxTree) = typeintersect(Proposition, tokentypes(t))

Base.in(t::SyntaxToken, tree::SyntaxTree) =
    t == token(tree) || any([Base.in(t, c) for c in children(tree)])

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
abstract type AbstractGrammar{A<:AbstractAlphabet, O<:AbstractOperator} end

operatortypes(g::AbstractGrammar{A, O}) where {A, O} = O

"""
    alphabet(g::AbstractGrammar{A} where {A})::A

Each grammar must provide a method for accessing its propositional `alphabet`.

See also [`AbstractGrammar`](@ref).
"""
alphabet(g::AbstractGrammar{A} where {A})::A = error("Please, provide method alphabet(::$(typeof(g))).")
propositiontype(g::AbstractGrammar) = eltype(alphabet(g))
tokentypes(g::AbstractGrammar) = Union{operatortypes(g), propositiontype(g)}

"""
    Base.in(t::SyntaxToken, g::AbstractGrammar)::Bool

Each grammar must provide some methods for establishing whether a syntax token belongs to it,
that is, whether it is a legal token in the grammar's formulas.

These two fallbacks are defined:

    Base.in(p::Proposition, g::AbstractGrammar) = Base.in(p, alphabet(g))
    Base.in(op::AbstractOperator, g::AbstractGrammar) = op <: operatortypes(O)

See also [`AbstractGrammar`](@ref).
"""
Base.in(t::SyntaxToken, g::AbstractGrammar) = error("Please, provide method Base.in(::$(typeof(t)), ::$(typeof(g))).")
# Note: when using this file's sintax tokens, these methods suffice:
Base.in(p::Proposition, g::AbstractGrammar) = Base.in(p, alphabet(g))
Base.in(op::AbstractOperator, g::AbstractGrammar) = op <: operatortypes(O)


"""
    Base.in(t::SyntaxTree, g::AbstractGrammar)::Bool

Each grammar must provide a method for establishing whether a formula,
encoded as a syntax tree, belongs to it.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
Base.in(t::SyntaxTree, g::AbstractGrammar)::Bool =
    error("Please, provide method Base.in(::SyntaxTree, ::$(typeof(g))).")

"""
    formulas(g::AbstractGrammar)::Vector{<:SyntaxTree{<:tokentypes(g)}}

Each grammar must provide a method for enumerating its formulas, encoded as syntax trees.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
function formulas(
    g::AbstractGrammar{A, O} where {A, O},
)::Vector{<:SyntaxTree{<:tokentypes(g)}}
    error("Please, provide method formulas(::$(typeof(g))).")
end

"""
    struct CompleteGrammar{A<:AbstractAlphabet, O<:AbstractOperator} <: AbstractGrammar{A, O}
        alphabet::A
        operators::Vector{<:O}
    end

Grammar that generates all well-formed formulas obtained by the arity-complying composition
of propositions of an alphabet of type `A`, and all operators with type in `O`.
With n operators, this grammar has exactly n+1 production rules, and
m+1 non-terminal symbols, where m is the number of nullary operators.
for example, with `O = Union{⊥,∧,∨}`, the grammar is:
    
    T ::= p | ⊥ | T ∧ T | T ∨ T

with p ∈ alphabet.
"""
struct CompleteGrammar{A<:AbstractAlphabet, O<:AbstractOperator} <: AbstractGrammar{A, O}
    alphabet::A
    operators::Vector{<:O}

    function CompleteGrammar{A, O}(
        alphabet::A,
        operators::Vector{O},
    ) where {A<:AbstractAlphabet, O<:AbstractOperator}
        new{A, O}(alphabet, operators)
    end

    function CompleteGrammar{A}(
        alphabet::A,
        operators::Vector{O},
    ) where {A<:AbstractAlphabet, O<:AbstractOperator}
        CompleteGrammar{A, O}(alphabet, operators)
    end

    function CompleteGrammar(
        alphabet::A,
        operators::Vector{O},
    ) where {A<:AbstractAlphabet, O<:AbstractOperator}
        CompleteGrammar{A, O}(alphabet, operators)
    end
end

alphabet(g::CompleteGrammar{A} where {A}) = g.alphabet

# A complete grammar includes any *safe* syntax tree that can be built with
#  the grammar token types.
function Base.in(t::SyntaxTree, g::CompleteGrammar)::Bool
    if token(t) isa Proposition
        token(t) in alphabet(g)
    elseif token(t) isa AbstractOperator
        if operatortypes(t) <: operatortypes(g)
            true
        else
            all([Base.in(c, g) for c in children(t)])
        end
    end
end

# TODO write enumerating algorithm!
function formulas(
    g::CompleteGrammar{A, O} where {A, O},
)::Vector{SyntaxTree{<:Union{propositiontype(A),O}}} end

############################################################################################
######################################## SEMANTICS #########################################
############################################################################################

"""
Type alias for any Julia type that may instantiate truth values.

See also [`Algebra`](@ref).
"""
const Truth = Any

"""
    struct TruthOperator{T<:Truth} <: AbstractOperator end
    
A truth value, that can be used in formulas as a nullary operator.
Two canonical truth values that are used as nullary operators are
`⊤` (*top*) and `⊥` (*bottom*), representing truth (`true`) and falsity (`false`), respectively.

See also [`TOP`](@ref), [`BOTTOM`](@ref), [`Truth`](@ref).
"""
struct TruthOperator{T<:Truth} <: AbstractOperator end
arity(::TruthOperator) = 0

doc_TOP = """
    const TOP = TruthOperator{:⊤}()
    const ⊤ = TOP

Truth operator representing the boolean value `true`.

See also [`BOTTOM`](@ref), [`TruthOperator`](@ref), [`Truth`](@ref).
"""
"""
$(doc_TOP)
"""
const TOP = TruthOperator{:⊤}()
"""
$(doc_TOP)
"""
const ⊤ = TOP

doc_BOTTOM = """
    const BOTTOM = TruthOperator{:⊥}()
    const ⊥ = BOTTOM

Truth operator representing the boolean value `false`.

See also [`TOP`](@ref), [`TruthOperator`](@ref), [`Truth`](@ref).
"""
"""
$(doc_BOTTOM)
"""
const BOTTOM = TruthOperator{:⊥}()
"""
$(doc_BOTTOM)
"""
const ⊥ = BOTTOM

"""
    abstract type AbstractAlgebra end

Abstract type for representing algebras. Algebras are used for grounding the truth of propositions
and the semantics of operators. They typically encode a
[https://en.m.wikipedia.org/wiki/Lattice_(order)](lattice structure) where two elements
(or nodes) *⊤* and *⊥* are referred to as *top* (or maximum) and *bottom* (or minimum).
Each node in the lattice represents a truth value that a proposition or a formula can have
on a model, and the semantics of operators is given in terms of operations between truth
values.

See also [`BooleanAlgebra`](@ref), [`AbstractOperator`](@ref), [`collate_truth`](@ref).
"""
abstract type AbstractAlgebra end

"""
    domain(a::AbstractAlgebra)

Each algebra must provide a method for accessing its `domain`.

See also [`AbstractAlgebra`](@ref).
"""
domain(a::AbstractAlgebra)::AbstractVector{<:truthtype(a)} = error("Please, provide method domain(::$(typeof(a))).")
truthtype(a::AbstractAlgebra)::Truth = eltype(domain(a))
# Base.in(t::Truth, a::AbstractAlgebra) = Base.in(t, domain(a)) maybe one day this will have a use?

"""
    top(a::AbstractAlgebra)

Each algebra must provide a method for accessing its `top`.

See also [`AbstractAlgebra`](@ref).
"""
top(a::AbstractAlgebra)::truthtype(a) = error("Please, provide method top(::$(typeof(a))).")

"""
    bottom(a::AbstractAlgebra)

Each algebra must provide a method for accessing its `bottom`.

See also [`AbstractAlgebra`](@ref).
"""
bottom(a::AbstractAlgebra)::truthtype(a) = error("Please, provide method bottom(::$(typeof(a))).")


"""
    iscrisp(a::AbstractAlgebra) = (truthtype(a) == Bool)

An algebra is crisp (or *boolean*) when its domain type is... `Bool`, quite literally!

See also [`AbstractAlgebra`](@ref).
"""
iscrisp(a::AbstractAlgebra) = (truthtype(a) == Bool)
isfuzzy = !iscrisp

"""
    collate_truth(a::AbstractAlgebra,op::AbstractOperator,t::NTuple{N, T})::T where {N, T<:Truth}

This function computes a truth value of a composed formula op(φ1, ..., φN), given the `N`
truth values of its immediate sub-formulas.
An algebra must provide a `collate_truth` method for each operator that can be
interpreted on it.

See also [`AbstractAlgebra`](@ref) [`AbstractOperator`](@ref), [`Truth`](@ref).
"""
function collate_truth(
    a::AbstractAlgebra,
    op::AbstractOperator,
    t::NTuple{N, T},
)::T where {N, T<:Truth}
    if truthtype(a) != T
        error("Cannot collate $(length(t)) truth values of type $(T) with algebra $(typeof(a)) with truth type $(truthtype(a))).")
    elseif arity(op) != length(t)
        error("Cannot collate $(length(t)) truth values for operator $(typeof(op)) with arity $(arity(op))).")
    else
        error("Please, provide method collate_truth(::$(typeof(a)), ::$(typeof(op)), ::NTuple{$(arity(op)), $(truthtype(a))}.")
    end
end

# Helper
collate_truth(a::AbstractAlgebra, op::AbstractOperator, t::NTuple{N, T}...) where {N, T<:Truth} =
    collate_truth(a, op, t)

# Note: `collate_truth` for TOP and BOTTOM relies on the `top` and `bottom` methods.
collate_truth(a::AbstractAlgebra, ::typeof(⊤), t::NTuple{0}) = top(a)
collate_truth(a::AbstractAlgebra, ::typeof(⊥), t::NTuple{0}) = bottom(a)


"""
    abstract type AbstractLogic{G<:AbstractGrammar, A<:AbstractAlgebra} end

Abstract type of a logic, which comprehends a context-free grammar (syntax) and
an algebra (semantics).

See also [`AbstractGrammar`](@ref), [`AbstractAlgebra`](@ref).
"""
abstract type AbstractLogic{G<:AbstractGrammar, A<:AbstractAlgebra} end

"""
    grammar(l::AbstractLogic{G} where {G<:AbstractGrammar})::G

A logic must provide a method for accessing its grammar.

See also [`AbstractGrammar`](@ref), [`AbstractLogic`](@ref).
"""
grammar(l::AbstractLogic{G} where {G})::G = error("Please, provide method grammar(::$(typeof(l))).")

operatortypes(l::AbstractLogic) = operatortypes(grammar(l))
alphabet(l::AbstractLogic) = alphabet(grammar(l))
propositiontype(l::AbstractLogic) = propositiontype(alphabet(l))
tokentypes(l::AbstractLogic) = tokentypes(grammar(l))

Base.in(op::AbstractOperator, l::AbstractLogic) = Base.in(op, grammar(l))
Base.in(t::SyntaxTree, l::AbstractLogic) = Base.in(t, alphabet(l))
Base.in(p::Proposition, l::AbstractLogic) = Base.in(p, alphabet(l))

"""
    algebra(l::AbstractLogic{G, A} where {G<:AbstractGrammar, A<:AbstractAlgebra})::A

A logic must provide a method for accessing its algebra.

See also [`AbstractAlgebra`](@ref), [`AbstractLogic`](@ref).
"""
algebra(l::AbstractLogic{G, A} where {G, A})::A = error("Please, provide method algebra(::$(typeof(l))).")

truthtype(l::AbstractLogic) = truthtype(algebra(l))
top(l::AbstractLogic) = top(algebra(l))
bottom(l::AbstractLogic) = bottom(algebra(l))
iscrisp(l::AbstractLogic) = iscrisp(algebra(l))
collate_truth(l::AbstractAlgebra, args...) = collate_truth(algebra(l, args...))

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
Base.in(t::SyntaxToken, f::AbstractFormula) = error("Please, provide method Base.in(::SyntaxToken, ::$(typeof(f))).")

"""
    tree(f::AbstractFormula)::SyntaxTree{<:tokentypes(logic(f))}

A formula must provide a method for extracting its syntax tree representation.

See also [`SyntaxTree`](@ref).
"""
tree(f::AbstractFormula)::SyntaxTree{<:tokentypes(logic(f))} =
    error("Please, provide method tree(::$(typeof(f))).")

Base.convert(::Type{<:SyntaxTree}, f::AbstractFormula) = tree(f)

"""
In order to use operators for composing formulas, along with syntax tokens (e.g., propositions)
and syntax trees, each formula should specify an explicit promotion method:

    Base._promote(x::F, y::SyntaxTree)::NTuple{2, F} where {F<:AbstractFormula}

This method should return a tuple (x, ŷ) where ŷ is a formula of the same type as x, but
representing the same formula as y.
Refer to https://github.com/JuliaLang/julia/blob/master/base/promotion.jl.

See also [`SyntaxToken`](@ref), [`SyntaxTree`](@ref), [`AbstractOperator`](@ref).
"""
function Base._promote(x::F, y::SyntaxTree)::NTuple{2, F} where {F<:AbstractFormula}
    error("Please, provide method Base._promote(x::$(F), ::SyntaxTree) (refer to https://github.com/JuliaLang/julia/blob/master/base/promotion.jl).")
end
function Base._promote(x::F, y::SyntaxToken) where {F<:AbstractFormula}
    Base._promote(x, Base.convert(SyntaxTree, y))
end
Base._promote(x::Union{SyntaxTree,SyntaxToken}, y::AbstractFormula) = reverse(Base._promote(y, x))

"""
In order to use operators for composing formulas, along with syntax tokens (e.g., propositions)
and syntax trees, each formula should specify a composition method:

    (op::AbstractOperator)(children::NTuple{N, F})::F where {N, F<:AbstractFormula}

See also [`AbstractFormula`](@ref), [`SyntaxTree`](@ref), [`AbstractOperator`](@ref).
"""
function (op::AbstractOperator)(children::NTuple{N, F})::F where {N, F<:AbstractFormula}
    error("Please, provide method (op::AbstractOperator)(children::NTuple{N, AbstractFormula}) where {N}.")
end

"""
    struct Formula{L<:AbstractLogic} <: AbstractFormula{L}
        _logic::Base.RefValue{L}
        tree::SyntaxTree
    end

In the most general case, a formula encodes a syntax tree that is anchored to
a certain logic; that is: a) the tree encodes a formula belonging to the grammar
of the logic; b) the truth of the formula can be evaluated
on models of the logic. Note that, here, the logicis represented by a reference.

See also [`AbstractLogic`](@ref).
"""
struct Formula{L<:AbstractLogic} <: AbstractFormula{L}
    _logic::Base.RefValue{L}
    tree::SyntaxTree # SyntaxTree{FT} where {FT<:tokentypes(_logic[])}

    _l(l::AbstractLogic) = Base.RefValue(l)
    _l(l::Base.RefValue) = l

    function Formula{L}(
        l::Union{L,Base.RefValue{L}},
        tree::SyntaxTree,
    ) where {L<:AbstractLogic}
        _logic = _l(l)

        # Note: this is merely a type-check: it checks that the tokens in the tree
        # satisfy the allowed token types of the logic (e.g., the type of propositions
        # and operators).
        # It does not check that the propositions belong to the alphabet of the logic.
        @assert tokentypes(tree) <: tokentypes(_logic[])
        
        new{L}(_logic, tree)
    end

    function Formula(
        l::Union{L,Base.RefValue{L}},
        tree::SyntaxTree,
    ) where {L<:AbstractLogic}
        Formula{L}(l, tree)
    end
end

_logic(f::Formula) = f._logic
logic(f::Formula) = f._logic[]
tree(f::Formula) = f.tree

Base.in(t::SyntaxToken, f::Formula) = Base.in(t, tree(f))

# Adapted from https://github.com/JuliaLang/julia/blob/master/base/promotion.jl
function Base._promote(x::F, y::SyntaxTree) where {F<:Formula}
    @inline
    return (x, F(_logic(x), y))
end

function (op::AbstractOperator)(children::NTuple{N, Formula}) where {N}
    ls = unique(logic.(children))
    @assert length(ls) == 1 # TODO: use allequal 1.8?
    Formula(first(ls), op(map(tree, children)))
end

############################################################################################
######################################### UTILS ############################################
############################################################################################

# Complementing Base.in for this file's definitions.
Base.in(t::Union{SyntaxToken, SyntaxTree}, a::AbstractAlphabet) =
    error("Attempting Base.in($(typeof(t)), ::$(typeof(a))), but $(typeof(t))'s cannot belong to alphabets.")

"""
An alphabet can be used for instantiating propositions.
"""
(::AbstractAlphabet)(a::A) where {A} = Proposition{A}(a)

"""
An operator can be used to compose syntax tokens (e.g., propositions),
syntax trees and/or formulas. This is quite handy, try it:

    ¬(Proposition(1)) ∨ Proposition(1)
"""
(op::AbstractOperator)(o::Any) = error("Cannot apply operator $(op)::$(typeof(op)) to object $(o)::$(typeof(o))")

(op::AbstractOperator)(children::Union{SyntaxToken, SyntaxTree}...) = op(children)
(op::AbstractOperator)(children::NTuple{N, Union{SyntaxToken, SyntaxTree}}) where {N} =
    SyntaxTree(op, children...)

(op::AbstractOperator)(children::Union{SyntaxToken, SyntaxTree, AbstractFormula}...) = op(children)
function (op::AbstractOperator)(children::NTuple{N, Union{SyntaxToken, SyntaxTree, AbstractFormula}}) where {N}
    _children = map((c)->begin
        isa(c, SyntaxToken) ? convert(SyntaxTree, c) : c
        end, children)
    op(Base.promote(_children...))
end

"""
    abstract type AbstractLogicalModel{L<:AbstractLogic} end

Abstract type for representing a logical model of a certain logic of type `L`.
Classically, a model is referred to an
[interpretation](https://en.m.wikipedia.org/wiki/Interpretation_(logic))
in the case of [propositional logic](https://en.m.wikipedia.org/wiki/Propositional_calculus),
while [Kripke structure](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s
are used for modal and first-order logics.

"""
abstract type AbstractLogicalModel{L<:AbstractLogic} end

"""
A 
"""
goeswith(::AbstractLogicalModel{L}, ::L)::Bool where {L<:AbstractLogic} = true
goeswith(::AbstractLogicalModel, ::Logic)::Bool = false
check(::AbstractFormula, ::AbstractLogicalModel)

check(::SyntaxNode{...}, ::TruthDictionary) # SyntaxNode is converted to Formula, then the new dispatch is applied
check(::Proposition, ::TruthDict)

provides_specific_check(::AbstractLogicalModel) = false
function check(f::AbstractFormula, m::AbstractLogicalModel, args...)::Truth
    if provides_specific_check(m)
        error("Please, provide method check(::AbstractFormula, ::$(typeof(m)), args...) with args::$(typeof(args)).")
    else
        error("Cannot check formula $(f) of type $(typeof(f)) on model $(m) of type $(typeof(m)) with args $(args) of type $(typeof(args)).")
    end
end
