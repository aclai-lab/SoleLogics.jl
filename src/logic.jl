import Base: eltype, in, isiterable, iterate, IteratorSize, length

############################################################################################
########################################## SYNTAX ##########################################
############################################################################################

"""
    abstract type SyntaxToken end

A token in a syntax tree.
A syntax tree is a tree-like structure where each node holds a *token*, and
has as many children as the `ariety` of the token.
"""
abstract type SyntaxToken end

"""
    ariety(t::SyntaxToken)

Each syntax token must provide a method yielding its `ariety`.
The ariety of a token is the expected number of children of a node that
wraps it in a syntax tree.

See also [`SyntaxToken`](@ref).
"""
ariety(t::SyntaxToken)::Integer = error("Please, provide method ariety(::$(typeof(t)).")

"""
    struct Proposition{A} <: SyntaxToken

A `Proposition{A}` (also called a propositional letter, or simply *letter*) wraps an
`atom::A` representing a fact which truth can be assessed on a logical model.

See also [`SyntaxToken`](@ref), [`AbstractModel`](@ref), [`check`](@ref).
"""
struct Proposition{A} <: SyntaxToken
    atom::A
end

ariety(::Proposition) = 0


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

isconstant(op::AbstractOperator) = ariety(op) == 0
isunary(op::AbstractOperator) = ariety(op) == 1
isbinary(op::AbstractOperator) = ariety(op) == 2


"""
    abstract type AbstractAlphabet{A} end

Abstract type for an alphabet of propositions with atoms of type `A`.
An alphabet (or propositional alphabet) is a
[https://en.m.wikipedia.org/wiki/Countable_set](countable) set of propositions.

See also [`Proposition`](@ref), [`AbstractGrammar`](@ref).
"""
abstract type AbstractAlphabet{A} end

Base.eltype(::Type{<:AbstractAlphabet{A}}) where {A} = Proposition{<:A}
propositiontype(a::AbstractAlphabet) = eltype(a)

"""
Each alphabet must provide a method for establishing whether a proposition belongs or not to it.

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
function Base.in(a::AbstractAlphabet, p::Proposition{A})::Bool where {A}
    if A <: eltype(a)
        error("Please, provide method Base.in(::$(typeof(a)), ::$(typeof(p))).")
    else
        error("Cannot establish whether proposition $(p) of type $(typeof(p)) is in alphabet $(a) of type $(typeof(a)) and eltype $(eltype(a)).")
    end
end

"""
Each alphabet must specify whether it is iterable.
An alphabet is iterable if it provides the `iterate` methods required by the
[https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration](iteration interface).

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
Base.isiterable(::AbstractAlphabet) = true
Base.iterate(a::AbstractAlphabet) = error(isiterable(a) ? "Please, provide method Base.iterate(::$(typeof(a))), or define Base.isiterable(::$(typeof(a))) = false." : "Cannot iterate alphabet of type $(typeof(a)).")
Base.iterate(a::AbstractAlphabet, state) = error(isiterable(a) ? "Please, provide method Base.iterate(::$(typeof(a)), state), or define Base.isiterable(::$(typeof(a))) = false." : "Cannot iterate alphabet of type $(typeof(a)).")

"""
Each alphabet must specify whether it is finite.
An alphabet is finite if it provides the `length` method.
"""
Base.isfinite(::AbstractAlphabet) = true
Base.length(a::AbstractAlphabet) = error(isfinite(a) ? "Please, provide method Base.length(::$(typeof(a))), or define Base.isfinite(::$(typeof(a))) = false." : "Cannot compute length of alphabet of type $(typeof(a)).")

# [https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration](Iteration interface) util.
Base.IteratorSize(::Type{M}) where {M<:AbstractAlphabet} = isfinite(M) ? Base.HasLength() : Base.IsInfinite()

"""
    propositions(a::AbstractAlphabet)::AbstractVector{<:propositiontype(a)}

Provides access to the propositions of an iterable alphabet.
If the alphabet is finite, the default behavior is `collect`ing all the propositions.
If it is not finite, a method for enumerating the propositions should be provided.

    propositions(a::AbstractAlphabet, args...)::AbstractVector{<:propositiontype(a)}

An alphabet can also implement an extended version of this function that only returns
propositions satisfying a given constraint. This is especially useful with infinite alphabets.

See also [`AbstractAlphabet`](@ref), [`isiterable`](@ref), [`isfinite`](@ref).
"""
function propositions(a::AbstractAlphabet)::AbstractVector{<:propositiontype(a)}
    if isiterable(a)
        if isfinite(a)
            collect(a)
        else
            error("Please, provide method propositions(::$(typeof(a))). Note: attempting at iterating through an infinite alphabet.")
        end
    else
       error("Cannot list propositions of an alphabet of type $(typeof(a)).")
    end
end

function propositions(a::AbstractAlphabet, args...)::AbstractVector{<:propositiontype(a)}
    error("Please, provide method propositions(::$(typeof(a)), args...) for a bounded iteration through an infinite alphabet.")
end


############################################################################################
############################################################################################
############################################################################################

"""
A syntax tree encoding a logical formula.
Each node of the syntax tree holds a *token*, and
has as many children as the `ariety` of the token.

See also [`SyntaxToken`](@ref), [`ariety`](@ref), [`Proposition`](@ref), [`Operator`](@ref).
"""
struct SyntaxTree{T<:SyntaxToken}
    token::T
    children::NTuple{N, SyntaxTree} where {N}

    function SyntaxTree{T}(
        token::T,
        children::NTuple{N, SyntaxTree},
    ) where {T, N}
        @assert ariety(token) == N "Cannot instantiate SyntaxTree with token $(token) of ariety $(ariety(token)) and $(N) children."
        new{T}(token, children)
    end

    function SyntaxTree(
        token::T,
        children::NTuple{N, SyntaxTree},
    ) where {T, N}
        SyntaxTree{T}(token, children)
    end
end

"""
    abstract type AbstractGrammar{A<:AbstractAlphabet, O<:AbstractOperator} end

Abstract type for a
[context-free grammar](https://en.m.wikipedia.org/wiki/Context-free_grammar)
based on a *single* alphabet of type A, and a set of operators
that consists of all the (singleton) child types of `O`.

See also [`AbstractAlphabet`](@ref), [`AbstractOperator`](@ref).
"""
abstract type AbstractGrammar{A<:AbstractAlphabet, O<:AbstractOperator} end

operators(g::AbstractGrammar{A, O}) where {A, O} = O

"""
Each grammar must provide a method for accessing its propositional `alphabet`.
"""
alphabet(g::AbstractGrammar{A} where {A})::A = error("Please, provide method alphabet(::$(typeof(g))).")
propositiontype(g::AbstractGrammar) = eltype(alphabet(g))

"""
Each grammar must provide a method for establishing whether a formula
(encoded as a syntax tree) belongs to it.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
Base.in(g::AbstractGrammar, t::SyntaxTree)::Bool = error("Please, provide method Base.in(::$(typeof(g)), ::$(typeof(t))).")

"""
Each grammar must provide a method for enumerating its formulas, encoded as syntax trees.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
function formulas(
    g::AbstractGrammar{A, O} where {A, O},
)::Vector{SyntaxTree{<:Union{propositiontype(A),O}}}
    error("Please, provide method formulas(::$(typeof(g))).")
end

############################################################################################
######################################## SEMANTICS #########################################
############################################################################################

"""
Type alias for any Julia type that may instantiate truth values.

See also [`Algebra`](@ref).
"""
const Truth = Type

"""
Abstract type for algebras. Algebras are used for grounding the truth of propositions
and the semantics of operators.

See also [`Truth`](@ref).
"""
abstract type AbstractAlgebra end

"""
Each algebra must provide a method for accessing its `domain`.

See also [`AbstractAlgebra`](@ref).
"""
domain(a::AbstractAlgebra)::AbstractVector{<:Truth} = error("Please, provide method domain(::$(typeof(a))).")
truth_type(a::AbstractAlgebra)::Truth = eltype(domain(a))

"""
An algebra is crisp (or *boolean*) when its domain type is... `Bool`, quite literally!

See also [`AbstractAlgebra`](@ref).
"""
iscrisp(a::AbstractAlgebra)::Bool = (truth_type(a) == Bool)

"""
An algebra must provide a `collate_truth` method for all operators that can be
interpreted on it.

See also [`AbstractAlgebra`](@ref).
"""
function collate_truth(
    a::AbstractAlgebra,
    o::AbstractOperator,
    t::NTuple{N, T},
) where {N, T<:Truth}
    if truth_type(a) != length(t)
        error("Cannot collate $(length(t)) truth values of type $(T) with algebra $(typeof(a)) with truth type $(truth_type(a))).")
    elseif ariety(o) != length(t)
        error("Cannot collate $(length(t)) truth values for operator $(typeof(o)) with ariety $(ariety(o))).")
    else
        error("Please, provide method collate_truth(::$(typeof(a)), ::$(typeof(o)), ::$(typeof(t))).")
    end
end

"""
    abstract type AbstractLogic{G<:AbstractGrammar, A<:AbstractAlgebra} end

Abstract type of a logic, which comprehends a context-free grammar (syntax) and
an algebra (semantics).

See also [`AbstractGrammar`](@ref), [`AbstractAlgebra`](@ref).
"""
abstract type AbstractLogic{G<:AbstractGrammar, A<:AbstractAlgebra} end

# # TODO fails here:

# # """
# #     struct Formula{L<:AbstractLogic}
# #         logic::Base.RefValue{L}
# #         tree::SyntaxTree
# #     end

# # A formula is a syntax tree that is anchored to a certain logic; that is:
# # a) the tree encodes a 
# # formula belonging to the grammar of the logic; b) the truth of the formula can be evaluated
# # on models of the logic.
# # """
# struct Formula{L}
#     logic::Base.RefValue{L}
#     tree::SyntaxTree
# end

# logic(l::Formula) = f.logic[]
# tree(::Formula{L}) = f.tree

# convert(::Type{<:SyntaxTree}, f::Formula) = tree(f)
# convert(::Type{Formula{L}}, t::SyntaxTree) where {L} = Formula{L}(L(), t)
