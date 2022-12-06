import Base: eltype, in, isiterable, iterate, IteratorSize, length, convert, promote_rule

############################################################################################
########################################## SYNTAX ##########################################
############################################################################################

"""
    abstract type SyntaxToken end

A token in a syntax tree.
A syntax tree is a tree-like structure where each node holds a *token*, and
has as many children as the `arity` of the token.
"""
abstract type SyntaxToken end

"""
    arity(t::SyntaxToken)

Each syntax token must provide a method yielding its `arity`.
The arity of a token is the expected number of children of a node that
wraps it in a syntax tree.

See also [`SyntaxToken`](@ref).
"""
arity(t::Type{<:SyntaxToken})::Integer = error("Please, provide method arity(::$(typeof(t)).")
arity(t::SyntaxToken)::Integer = arity(typeof(t))

"""
    struct Proposition{A} <: SyntaxToken

A `Proposition{A}` (also called a propositional letter, or simply *letter*) wraps an
`atom::A` representing a fact which truth can be assessed on a logical model.

See also [`SyntaxToken`](@ref), [`AbstractModel`](@ref), [`check`](@ref).
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

Abstract type for an alphabet of propositions with atoms of type `A`.
An alphabet (or propositional alphabet) is a
[https://en.m.wikipedia.org/wiki/Countable_set](countable) set of propositions.

See also [`Proposition`](@ref), [`AbstractGrammar`](@ref).
"""
abstract type AbstractAlphabet{A} end

Base.eltype(::Type{<:AbstractAlphabet{A}}) where {A} = Proposition{A}
propositiontype(a::Type{<:AbstractAlphabet}) = eltype(a)
propositiontype(a::AbstractAlphabet) = propositiontype(typeof(a))

"""
Each alphabet must provide a method for establishing whether a proposition belongs or not to it.

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
function Base.in(p::Proposition{A}, a::AbstractAlphabet)::Bool where {A}
    if A <: eltype(a)
        error("Please, provide method Base.in(::$(typeof(p)), ::$(typeof(a))).")
    else
        error("Cannot establish whether proposition $(p) of type $(typeof(p)) is in alphabet $(a) of type $(typeof(a)) and eltype $(eltype(a)).")
    end
end

# Helper
Base.in(o::Any, a::AbstractAlphabet) = Base.in(Proposition(o), a) # error("Attempting Base.in($(typeof(o)), ::$(typeof(a))), but only Proposition's can belong to alphabets.")

"""
Each alphabet must specify whether it is iterable.
An alphabet is iterable if it provides the `iterate` methods required by the
[https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration](iteration interface).
    
    Base.isiterable(::Type{<:AbstractAlphabet}) = true

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
Base.isiterable(::Type{<:AbstractAlphabet}) = true
Base.isiterable(a::AbstractAlphabet) = Base.isiterable(typeof(a))
Base.iterate(a::AbstractAlphabet) = error(isiterable(a) ? "Please, provide method Base.iterate(::$(typeof(a))), or define Base.isiterable(::$(typeof(a))) = false." : "Cannot iterate infinite alphabet of type $(typeof(a)).")
Base.iterate(a::AbstractAlphabet, state) = error(isiterable(a) ? "Please, provide method Base.iterate(::$(typeof(a)), state), or define Base.isiterable(::$(typeof(a))) = false." : "Cannot iterate infinite alphabet of type $(typeof(a)).")

"""
Each alphabet must specify whether it is finite.
An alphabet is finite if it provides the `length` method.

    Base.isfinite(::Type{<:AbstractAlphabet}) = true

See also [`AbstractAlphabet`](@ref), [`Proposition`](@ref).
"""
Base.isfinite(::Type{<:AbstractAlphabet}) = true
Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))
Base.length(a::AbstractAlphabet) = error(Base.isfinite(a) ? "Please, provide method Base.length(::$(typeof(a))), or define Base.isfinite(::$(typeof(a))) = false." : "Cannot compute length of alphabet of type $(typeof(a)).")

# function (A::Type{<:AbstractAlphabet})(v::AbstractVector)
#     if Base.isfinite(A)
#         A(begin
#             # if v isa Vector
#                 map(Proposition, v)
#             # else
#             #     Iterators.map(Proposition, v)
#             # end
#         end)
#     else
#         error("Please, provide method (::$(typeof(A)))(::$(typeof(v))), or define Base.isfinite(::$(typeof(A))) = false.")
#     end
# end

# [https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration](Iteration interface) util.
Base.IteratorSize(::Type{M}) where {M<:AbstractAlphabet} = Base.isfinite(M) ? Base.HasLength() : Base.IsInfinite()

"""
    propositions(a::AbstractAlphabet)::AbstractVector{<:propositiontype(a)}

Provides access to the propositions of an iterable alphabet.
If the alphabet is finite, the default behavior is `collect`ing all the propositions.
If it is not finite, a method for enumerating the propositions should be provided.

    propositions(a::AbstractAlphabet, args...)::AbstractVector{<:propositiontype(a)}

An alphabet can also implement an extended version of this function that only returns
propositions satisfying a given constraint. This is especially useful with infinite alphabets.

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

An alphabet wrapping propositions in a Vector{A}
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

# """
#     struct ExplicitAlphabet{A} <: AbstractAlphabet{A}

# An alphabet wrapping a generator of propositions.
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

An implicit infitine alphabet that includes all propositions with atoms of a subtype of A.
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

A syntax tree encoding a logical formula.
Each node of the syntax tree holds a `token::T`, and
has as many children as the `arity` of the token.

This implementation is *safe*, in that, this arity check is performed upon construction.
An additional type parameter `FT` ensures that the the token type is constrained to a
predefined set..

See also [`SyntaxToken`](@ref), [`arity`](@ref), [`Proposition`](@ref), [`Operator`](@ref).
"""
struct SyntaxTree{FT<:SyntaxToken, T<:FT}
    token::T
    children::NTuple{N, SyntaxTree} where {N}

    # function SyntaxTree{FT, T}(
    #     token::T,
    #     children::NTuple{N, SyntaxTree} = (),
    # ) where {FT<:SyntaxToken, T<:FT, N}
    #     @assert arity(token) == N "Cannot instantiate SyntaxTree{$(FT), $(T)} with token $(token) of arity $(arity(token)) and $(N) children."
    #     @assert all(tokentypes.(children) .<: FT) "Cannot instantiate SyntaxTree{$(FT), $(T)} with children of feasible tokens $(tokentypes.(children))."
    #     new{FT, T<:SyntaxToken}(token, children)
    # end

    # function SyntaxTree{FT}(
    #     token::T,
    #     children::NTuple{N, SyntaxTree} = (),
    # ) where {FT<:SyntaxToken, T<:FT, N}
    #     SyntaxTree{FT, T}(token, children)
    # end

    function SyntaxTree(
        token::T,
        children::NTuple{N, SyntaxTree} = (),
    ) where {T<:SyntaxToken, N}
        @assert arity(token) == N "Cannot instantiate SyntaxTree with token $(token) of arity $(arity(token)) and $(N) children."
        new{SyntaxToken, T}(token, children)
    end

    # function SyntaxTree{FT, T}(
    #     token::T,
    #     children::SyntaxTree...,
    # ) where {FT, T<:SyntaxToken}
    #     SyntaxTree{FT, T}(token, children)
    # end

    # function SyntaxTree{FT}(
    #     token::T,
    #     children::SyntaxTree...,
    # ) where {FT, T<:SyntaxToken}
    #     SyntaxTree{FT}(token, children)
    # end

    function SyntaxTree(
        token::T,
        children::SyntaxTree...,
    ) where {T<:SyntaxToken}
        SyntaxTree(token, children)
    end
end

token(t::SyntaxTree) = t.token
children(t::SyntaxTree) = t.children

tokentype(::SyntaxTree{FT, T}) where {FT, T} = T
tokentypes(::SyntaxTree{FT}) where {FT} = FT

Base.in(t::SyntaxToken, tree::SyntaxTree) =
    t == token(tree) || any([Base.in(t, c) for c in children(tree)])

"""
    abstract type AbstractGrammar{A<:AbstractAlphabet, O<:AbstractOperator} end

Abstract type for a
[context-free grammar](https://en.m.wikipedia.org/wiki/Context-free_grammar)
based on a *single* alphabet of type A, and a set of operators
that consists of all the (singleton) child types of `O`.

See also [`AbstractAlphabet`](@ref), [`AbstractOperator`](@ref).
"""
abstract type AbstractGrammar{A<:AbstractAlphabet, O<:AbstractOperator} end

operatortypes(g::AbstractGrammar{A, O}) where {A, O} = O

"""
Each grammar must provide a method for accessing its propositional `alphabet`.
"""
alphabet(g::AbstractGrammar{A} where {A})::A = error("Please, provide method alphabet(::$(typeof(g))).")
propositiontype(g::AbstractGrammar) = eltype(alphabet(g))
tokentypes(g::AbstractGrammar) = Union{operatortypes(g),propositiontype(g)}

Base.in(p::Proposition, g::AbstractGrammar) = Base.in(p, alphabet(g))
# Base.in(o::Any, g::AbstractGrammar) = Base.in(o::Any, alphabet(g)) # better not

Base.in(op::AbstractOperator, g::AbstractGrammar) = op <: operatortypes(O)


"""
Each grammar must provide a method for establishing whether a formula
(encoded as a syntax tree) belongs to it.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
Base.in(t::SyntaxTree, g::AbstractGrammar)::Bool =
    error("Please, provide method Base.in(::$(typeof(t)), ::$(typeof(g))).")

"""
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

Grammar that includes all well-formed formulas obtained by the arity-complying composition
of propositions of an alphabet and operators in `O`.
With n operators, this grammar has exactly one non-terminal symbol, and n+1 production
rules; for example, with `O = Union{∧,∨}`, the grammar is:
    
    T ::= p | T ∧ T | T ∨ T

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
        if tokentypes(t) <: operatortypes(g)
            true
        else
            all([Base.in(c, g) for c in children(t)])
        end
    end
end

# TODO write enumerating algorithm?
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
A truth value can be used as a nullary operator.

See also [`Truth`](@ref).
"""
struct TruthOperator{T<:Truth} <: AbstractOperator end
arity(::TruthOperator) = 0

const TOP = TruthOperator{:⊤}()
const ⊤ = TOP

const BOTTOM = TruthOperator{:⊥}()
const ⊥ = BOTTOM

"""
Abstract type for algebras. Algebras are used for grounding the truth of propositions
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
Each algebra must provide a method for accessing its `domain`.

See also [`AbstractAlgebra`](@ref).
"""
domain(a::AbstractAlgebra)::AbstractVector{<:truthtype(a)} = error("Please, provide method domain(::$(typeof(a))).")
truthtype(a::AbstractAlgebra)::Truth = eltype(domain(a))
# Base.in(t::Truth, a::AbstractAlgebra) = Base.in(t, domain(a)) maybe one day?

"""
Each algebra must provide a method for accessing its `top`.

See also [`AbstractAlgebra`](@ref).
"""
top(a::AbstractAlgebra)::truthtype(a) = error("Please, provide method top(::$(typeof(a))).")

"""
Each algebra must provide a method for accessing its `bottom`.

See also [`AbstractAlgebra`](@ref).
"""
bottom(a::AbstractAlgebra)::truthtype(a) = error("Please, provide method bottom(::$(typeof(a))).")


"""
An algebra is crisp (or *boolean*) when its domain type is... `Bool`, quite literally!

See also [`AbstractAlgebra`](@ref).
"""
iscrisp(a::AbstractAlgebra) = (truthtype(a) == Bool)

"""
An algebra must provide a `collate_truth` method for each operator that can be
interpreted on it.

See also [`AbstractAlgebra`](@ref).
"""
function collate_truth(
    a::AbstractAlgebra,
    op::AbstractOperator,
    t::NTuple{N, T},
) where {N, T<:Truth}
    if truthtype(a) != length(t)
        error("Cannot collate $(length(t)) truth values of type $(T) with algebra $(typeof(a)) with truth type $(truthtype(a))).")
    elseif arity(op) != length(t)
        error("Cannot collate $(length(t)) truth values for operator $(typeof(op)) with arity $(arity(op))).")
    else
        error("Please, provide method collate_truth(::$(typeof(a)), ::$(typeof(op)), ::$(typeof(t))).")
    end
end

# Note that `collate_truth` for TOP and BOTTOM relies on the `top` and `bottom` methods.
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
A logic must provide a method for accessing its grammar.
"""
function grammar(::AbstractLogic) end

alphabet(l::AbstractLogic) = alphabet(grammar(l))
propositiontype(l::AbstractLogic) = propositiontype(alphabet(l))
tokentypes(l::AbstractLogic) = tokentypes(grammar(l))

Base.in(op::AbstractOperator, l::AbstractLogic) = Base.in(op, grammar(l))
Base.in(t::SyntaxTree, l::AbstractLogic) = Base.in(t, alphabet(l))
Base.in(p::Proposition, l::AbstractLogic) = Base.in(p, alphabet(l))

"""
A logic must provide a method for accessing its algebra.
"""
function algebra(::AbstractLogic) end

truthtype(l::AbstractLogic) = truthtype(algebra(l))
top(l::AbstractLogic) = top(algebra(l))
bottom(l::AbstractLogic) = bottom(algebra(l))
iscrisp(l::AbstractLogic) = iscrisp(algebra(l))
collate_truth(l::AbstractAlgebra, args...) = collate_truth(algebra(l, args...))

"""
    abstract type AbstractFormula{L<:AbstractLogic} end

A formula encodes a syntax tree that is anchored to a certain logic; that is:
a) the tree encodes a 
formula belonging to the grammar of the logic; b) the truth of the formula can be evaluated
on models of the logic.
"""
abstract type AbstractFormula{L<:AbstractLogic} end

"""
Each formula must provide a method for accessing its `logic`.

See also [`AbstractLogic`](@ref).
"""
logic(f::AbstractFormula) = error("Please, provide method logic(::$(typeof(f))).")
iscrisp(f::AbstractFormula) = iscrisp(logic(f))
grammar(f::AbstractFormula) = grammar(logic(f))
algebra(f::AbstractFormula) = algebra(logic(f))

"""
Each formula must provide a method for establishing whether a proposition appears in it.

See also [`Proposition`](@ref).
"""
Base.in(p::Proposition, f::AbstractFormula) = error("Please, provide method Base.in(::$(typeof(p)), ::$(typeof(f))).")

# Maybe?
# Base.in(op::AbstractOperator, f::AbstractFormula) = error("Please, provide method Base.in(::$(typeof(p)), ::$(typeof(f))).")

# TODO
Base.convert(::Type{F}, t::SyntaxTree) where {L, F<:AbstractFormula{L}} = F(L(), t)



"""
A formula must provide a method for extracting its syntax tree.
"""
Base.convert(S::Type{<:SyntaxTree}, f::AbstractFormula) =
    error("Please, provide method Base.convert(::$(typeof(S)), ::$(typeof(f))).")

"""
    struct Formula{L<:AbstractLogic} <: AbstractFormula{L}
        _logic::Base.RefValue{L}
        tree::SyntaxTree
    end

A formula encodes a syntax tree that is anchored to a certain logic; that is:
a) the tree encodes a 
formula belonging to the grammar of the logic; b) the truth of the formula can be evaluated
on models of the logic.
"""
struct Formula{L<:AbstractLogic} <: AbstractFormula{L}
    _logic::Base.RefValue{L}
    tree::SyntaxTree

    _logic(l::AbstractLogic) = Base.RefValue(l)
    _logic(l::Base.RefValue) = l

    function Formula{L}(
        l::Union{L,Base.RefValue{L}},
        tree::SyntaxTree,
    ) where {L<:AbstractLogic}
        new{L}(_logic(l), tree)
    end

    function Formula(
        l::Union{L,Base.RefValue{L}},
        tree::SyntaxTree,
    ) where {L<:AbstractLogic}
        Formula{L}(_logic(l), tree)
    end
end

_logic(f::Formula) = f._logic
logic(f::Formula) = f._logic[]
tree(f::Formula) = f.tree

Base.in(t::SyntaxToken, f::Formula) = Base.in(t, tree(f))
Base.in(p::Proposition, f::Formula) = Base.in(p, tree(f))
Base.in(op::AbstractOperator, f::Formula) = Base.in(op, tree(f))

Base.convert(::Type{<:SyntaxTree}, f::Formula) = tree(f)

############################################################################################
######################################### UTILS ############################################
############################################################################################

Base.in(t::Union{SyntaxToken, SyntaxTree}, a::AbstractAlphabet) =
    error("Attempting Base.in($(typeof(t)), ::$(typeof(a))), but $(typeof(t))'s cannot belong to alphabets.")

"""
An alphabet can be used for instantiating propositions
"""
(::AbstractAlphabet)(a::A) where {A} = Proposition{A}(a)

"""
Operator can be used to compose a syntax trees or formulas, the child nodes.
"""

(op::AbstractOperator)(o::Any) = error("Cannot apply operator $(op)::$(typeof(op)) to object $(o)::$(typeof(o))")

# (op::AbstractOperator)(p::Proposition, children::NTuple{0, SyntaxTree}) = op(p)
(op::AbstractOperator)(p::Proposition) = op(SyntaxTree(p))

(op::AbstractOperator)(children::SyntaxTree...) = op(children)
(op::AbstractOperator)(children::NTuple{N, SyntaxTree}) where {N} =
    SyntaxTree(typeof(op)(), children)

# TODO
(op::AbstractOperator)(children::Formula...) = op(children)
(op::AbstractOperator)(children::NTuple{N, F}) where {N, F<:AbstractFormula} = 
    error("Please, provide method (op::$(typeof(op)))(children::NTuple{$(N), F})) where {N, F<:AbstractFormula}.")

Base.promote_rule(::Type{L}, ::Type{SyntaxTree}) where {L<:AbstractLogic} = L
Base.promote_rule(::Type{SyntaxTree}, ::Type{L}) where {L<:AbstractLogic} = L

(op::AbstractOperator)(children::NTuple{N, Formula}) where {N} =
    Formula(logic(f), op(map(tree, children)))
