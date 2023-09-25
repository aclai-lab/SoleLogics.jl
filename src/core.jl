import Base: convert, promote_rule, _promote
import Base: eltype, in, getindex, isiterable, iterate, IteratorSize, length, isequal, hash

############################################################################################
########################################## SYNTAX ##########################################
############################################################################################

"""
    abstract type AbstractSyntaxToken end

A token in a syntactic structure.

See also [`SyntaxTree`](@ref), [`AbstractSyntaxStructure`](@ref),
[`arity`](@ref), [`syntaxstring`](@ref).
"""
abstract type AbstractSyntaxToken end

"""
    arity(::Type{<:AbstractSyntaxToken})::Integer
    arity(tok::AbstractSyntaxToken)::Integer = arity(typeof(tok))

Return the `arity` of a syntax token. The arity of a syntax token is an integer
representing the number of allowed children in a `SyntaxTree`. Tokens with `arity` equal
to 0, 1 or 2 are called `nullary`, `unary` and `binary`, respectively.

See also [`AbstractSyntaxToken`](@ref).
"""
arity(T::Type{<:AbstractSyntaxToken})::Integer = error("Please, provide method arity(::$(Type{T})).")
arity(t::AbstractSyntaxToken)::Integer = arity(typeof(t))

isnullary(a) = arity(a) == 0
isunary(a) = arity(a) == 1
isbinary(a) = arity(a) == 2

"""
    dual(tok::AbstractSyntaxToken)

Return the `dual` of a syntax token.
Given a token `tok` of arity `n`, the dual `dtok` is such that,
on a boolean algebra,
`tok(ch_1, ..., ch_n)` ≡ `¬dtok(¬ch_1, ..., ¬ch_n)`.

Duality can be used to perform synctactic simplifications on formulas.
For example, since `∧` and `∨` are `dual`s, `¬(¬p ∧ ¬q)` can be simplified to `(p ∧ q)`.
Duality also applies to nullary operators (`⊤`/`⊥`), operators with
existential/universal semantics (`◊`/`□`), and `Atom`s.

# Implementation

When providing a `dual` for an operator of type `O`, please also provide:

    hasdual(::O) = true

The dual of an `Atom` (that is, the atom with inverted semantics)
is defined as:

    dual(p::Atom{A}) where {A} = Atom(dual(value(p)))

As such, `hasdual(::A)` and `dual(::A)` should be defined when wrapping objects of type `A`.

See also [`normalize`](@ref), [`AbstractSyntaxToken`](@ref).
"""
dual(t::AbstractSyntaxToken) = error("Please, provide method dual(::$(typeof(t))).")
hasdual(t::AbstractSyntaxToken) = false

"""
    syntaxstring(φ::AbstractFormula; kwargs...)::String
    syntaxstring(tok::AbstractSyntaxToken; kwargs...)::String

Produce the string representation of a formula or syntax token by performing
a tree traversal of the syntax tree representation of the formula.
Note that this representation may introduce redundant parentheses.
`kwargs` can be used to specify how to display syntax tokens/trees under
some specific conditions.

The following `kwargs` are currently supported:
- `function_notation = false::Bool`: when set to `true`, it forces the use of
   function notation for binary operators
   (see [here](https://en.wikipedia.org/wiki/Infix_notation)).
- `remove_redundant_parentheses = true::Bool`: when set to `false`, it prints a syntaxstring
   where each syntactical element is wrapped in parentheses.
- `parenthesize_atoms = !remove_redundant_parentheses::Bool`: when set to `true`,
   it forces the atoms (which are the leafs of a formula's tree structure) to be
   wrapped in parentheses.

# Examples
```julia-repl
julia> syntaxstring(parsebaseformula("p∧q∧r∧s∧t"))
"p ∧ q ∧ r ∧ s ∧ t"

julia> syntaxstring(parsebaseformula("p∧q∧r∧s∧t"), function_notation=true)
"∧(p, ∧(q, ∧(r, ∧(s, t))))"

julia> syntaxstring(parsebaseformula("p∧q∧r∧s∧t"), remove_redundant_parentheses=false)
"(p) ∧ ((q) ∧ ((r) ∧ ((s) ∧ (t))))"

julia> syntaxstring(parsebaseformula("p∧q∧r∧s∧t"), remove_redundant_parentheses=true, parenthesize_atoms=true)
"(p) ∧ (q) ∧ (r) ∧ (s) ∧ (t)"

julia> syntaxstring(parsebaseformula("◊((p∧s)→q)"))
"◊((p ∧ s) → q)"

julia> syntaxstring(parsebaseformula("◊((p∧s)→q)"); function_notation = true)
"◊(→(∧(p, s), q))"
```

See also [`parsebaseformula`](@ref), [`parsetree`](@ref),
[`SyntaxTree`](@ref), [`AbstractSyntaxToken`](@ref).

# Implementation

In the case of a syntax tree, `syntaxstring` is a recursive function that calls
itself on the syntax children of each node. For a correct functioning, the `syntaxstring`
must be defined (including `kwargs...`) for every newly defined
`AbstractSyntaxToken` (e.g., operators and `Atom`s),
in a way that it produces a
*unique* string representation, since `Base.hash` and `Base.isequal`, at least for
`SyntaxTree`s, rely on it.

In particular, for the case of `Atom`s, the function calls itself on the wrapped value:

    syntaxstring(p::Atom; kwargs...) = syntaxstring(value(p); kwargs...)

The `syntaxstring` for any value defaults to its `string` representation, but it can be
defined by defining the appropriate `syntaxstring` method.

!!! warning
    The `syntaxstring` for syntax tokens (e.g., atoms, operators) should not be
    prefixed/suffixed by whitespaces, as this may cause ambiguities upon *parsing*.
    For similar reasons, `syntaxstring`s should not contain parentheses (`'('`, `')'`),
    and, when parsing in function notation, commas (`','`).
    See also [`parsebaseformula`](@ref).

"""
function syntaxstring(tok::AbstractSyntaxToken; kwargs...)::String
    return error("Please, provide method syntaxstring(::$(typeof(tok)); kwargs...).")
end

syntaxstring(value; kwargs...) = string(value)
# syntaxstring(value::Union{AbstractString,Number,AbstractChar}; kwargs...) = string(value)

############################################################################################
############################################################################################
############################################################################################

"""
    struct Atom{A} <: AbstractSyntaxToken
        value::A
    end

An atom, sometimes called an atomic proposition,
propositional letter (or simply *letter*), of type
`Atom{A}` wraps a `value::A` representing a fact which truth can be assessed on
a logical interpretation.

Atoms are nullary tokens (i.e, they are at the leaves of a syntax tree);
note that their atoms cannot be `Atom`s.

See also [`AbstractSyntaxToken`](@ref), [`AbstractInterpretation`](@ref), [`check`](@ref).
"""
struct Atom{A} <: AbstractSyntaxToken
    value::A

    function Atom{A}(value::A) where {A}
        @assert !(value isa Union{AbstractSyntaxToken,AbstractFormula}) "Illegal nesting. " *
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

value(p::Atom) = p.value

arity(::Type{<:Atom}) = 0
valuetype(::Atom{A}) where {A} = A
valuetype(::Type{Atom{A}}) where {A} = A

# Helpers
Base.convert(::Type{P}, p::Atom) where {P<:Atom} = P(p)
Base.convert(::Type{P}, a) where {P<:Atom} = P(a)
# Base.promote_rule(::Type{Union{AbstractString,Number,AbstractChar}}, ::Type{<:Atom}) = Atom
# Base.promote_rule(::Type{<:Atom}, ::Type{Union{AbstractString,Number,AbstractChar}}) = Atom

syntaxstring(p::Atom; kwargs...) = syntaxstring(value(p); kwargs...)

Base.isequal(a::Atom, b::Atom) = Base.isequal(value(a), value(b))
Base.isequal(a::Atom, b) = Base.isequal(value(a), b)
Base.isequal(a, b::Atom) = Base.isequal(a, value(b))
Base.hash(a::Atom) = Base.hash(value(a))

dual(p::Atom) = Atom(dual(value(p)))

function dual(atom::Any)
    return error("Please, provide method " *
        "SoleLogics.dual(::$(typeof(atom))).")
end

"""
@atoms(cast, ps...)

Instantiate a collection of [`Atom`](@ref)s and return them as a vector.

!!! info
    Atoms instantiated with this macro are defined in the global scope as constants.

# Examples
```julia-repl
julia> SoleLogics.@atoms String p q r s
4-element Vector{Atom{String}}:
 Atom{String}("p")
 Atom{String}("q")
 Atom{String}("r")
 Atom{String}("s")

julia> p
Atom{String}("p")
```
"""
macro atoms(ps...)
    quote
        $(map(p -> :(const $p = $(string(p) |> Atom)), ps)...)
        [$(ps...)]
    end |> esc
end

# Source:
#   Symbolics.jl  (https://github.com/JuliaSymbolics/Symbolics.jl)
#   PAndQ.jl      (https://github.com/jakobjpeters/PAndQ.jl)
atomize(p::Symbol) = :((@isdefined $p) ? $p : $(string(p) |> Atom))
atomize(x) = x
atomize(x::Expr) = Meta.isexpr(x, [:(=), :kw]) ?
    Expr(x.head, x.args[1], map(atomize, x.args[2:end])...) :
    Expr(x.head, map(atomize, x.args)...)

"""
    @synexpr(expression)

Return an expression after automatically instantiating undefined [`Atom`](@ref)s.

!!! info
Every identified atom is of type `Atom{String}`.

# Examples
```julia-repl
julia> @synexpr x = p # Atom{String}("p") is assigned to the global variable x
Atom{String}("p")

julia> @synexpr st = p ∧ q → r
(p ∧ q) → r

julia> typeof(st)
SyntaxTree{SoleLogics.NamedOperator{:→}}
```
"""
macro synexpr(expression)
    quote
        $(expression |> atomize)
    end |> esc
end

############################################################################################

"""
    abstract type AbstractOperator <: AbstractSyntaxToken end

An operator is a [logical constant](https://en.wikipedia.org/wiki/Logical_connective)
which establishes a relation between atoms (i.e., facts).
For example, the boolean operators AND, OR and IMPLIES (stylized as ∧, ∨ and →)
are used to connect atoms and/or formulas to express derived concepts.

Since operators often display very different algorithmic behaviors,
leaf subtypes of `AbstractOperator` are
often singleton types, which can be easily dispatched upon.

# Implementation

When implementing a new custom operator, think about changing its default [precedence and
associativity](https://docs.julialang.org/en/v1/manual/mathematical-operations/#Operator-Precedence-and-Associativity)
by providing the methods `Base.operator_precedence(::Type{AbstractOperator})` and
`isrightassociative(::Type{AbstractOperator})`.

When implementing a new type for a *commutative* operator `O` with arity higher than 1,
please provide a method `iscommutative(::Type{O})`. This can help model checking operations.

See also [`AbstractSyntaxToken`](@ref), [`NamedOperator`](@ref),
[`Base.operator_precedence`](@ref), [`isrightassociative`](@ref), [`iscommutative`](@ref),
[`check`](@ref).
"""
abstract type AbstractOperator <: AbstractSyntaxToken end

# Since, in general, operators are singletons, we show them via their syntaxstring
Base.show(io::IO, o::AbstractOperator) = print(io, syntaxstring(o))

doc_iscommutative = """
    iscommutative(::Type{AbstractOperator})
    iscommutative(o::AbstractOperator) = iscommutative(typeof(o))

Return whether an operator is known to be commutative.

# Examples
```julia-repl
julia> iscommutative(∧)
true

julia> iscommutative(→)
false
```

Note that nullary and unary operators are considered commutative.

See also [`AbstractOperator`](@ref).

# Implementation

When implementing a new type for a *commutative* operator `O` with arity higher than 1,
please provide a method `iscommutative(::Type{O})`. This can help model checking operations.
"""

"""$(doc_iscommutative)"""
function iscommutative(O::Type{<:AbstractOperator})
    if arity(O) <= 1
        return true
    else
        return false
        # return error("Please, provide method iscommutative(O::Type{$(O)}).")
    end
end

iscommutative(o::AbstractOperator) = iscommutative(typeof(o))

doc_precedence = """
    const MAX_PRECEDENCE  = Base.operator_precedence(:(::))
    const HIGH_PRECEDENCE = Base.operator_precedence(:^)
    const BASE_PRECEDENCE = Base.operator_precedence(:*)
    const LOW_PRECEDENCE  = Base.operator_precedence(:+)

Standard integers representing operator precedence;
operators with high values take precedence over operators with lower values.
This is needed to establish unambiguous implementations of parsing-related algorithms.

By default, all operators are assigned a `BASE_PRECEDENCE`, except for:
- nullary operators (e.g., ⊤, ⊥), that are assigned a `MAX_PRECEDENCE`;
- unary operators (e.g., ¬, ◊), that are assigned a `HIGH_PRECEDENCE`;
- the implication (→), that is assigned a `LOW_PRECEDENCE`.

In case of tie, operators are evaluated in the left-to-right order.

It is possible to assign a specific precedence to an operator type `O` by providing a method
`Base.operator_precedence(::Type{O})`.

# Examples
```julia-repl
julia> syntaxstring(parseformula("¬a ∧ b ∧ c"))
"¬a ∧ b ∧ c"

julia> syntaxstring(parseformula("¬a → b ∧ c"))
"(¬a) → (b ∧ c)"

julia> syntaxstring(parseformula("a ∧ b → c ∧ d"))
"(a ∧ b) → (c ∧ d)"
```

See also [`parseformula`](@ref), [`syntaxstring`](@ref).
"""

"""$(doc_precedence)"""
const MAX_PRECEDENCE = Base.operator_precedence(:(::))
"""$(doc_precedence)"""
const HIGH_PRECEDENCE = Base.operator_precedence(:^)
"""$(doc_precedence)"""
const BASE_PRECEDENCE = Base.operator_precedence(:*)
"""$(doc_precedence)"""
const LOW_PRECEDENCE  = Base.operator_precedence(:+)


"""
    Base.operator_precedence(op::AbstractOperator)
    Base.operator_precedence(::typeof(IMPLICATION))

Assign a precedence to an operator.

See also [`AbstractOperator`](@ref), [`MAX_PRECEDENCE`](@ref), [`HIGH_PRECEDENCE`](@ref),
[`BASE_PRECEDENCE`](@ref), [`LOW_PRECEDENCE`](@ref).
"""
function Base.operator_precedence(op::AbstractOperator)
    if isunary(op)
        HIGH_PRECEDENCE
    elseif isnullary(op)
        MAX_PRECEDENCE
    else
        BASE_PRECEDENCE
    end
end


"""
    isrightassociative(::Type{AbstractOperator})
    isrightassociative(o::AbstractOperator) = isrightassociative(typeof(o))

Return whether an `AbstractOperator` is right associative or no.

Associativity establishes how operators of the same precedence are grouped in the absence of
the parentheses.

Conjunction and disjunction are commutative operators, thus, the left associativity case
"(p ∧ q) ∧ r" and the right associativity case "p ∧ (q ∧ r)" are equivalent; by convention
we consider the latter form.
Implication is right associative, meaning that "p → q → r" is grouped as "p → (q → r)".

By default, an operator is right associative.

# Examples
```julia-repl
julia> isrightassociative(∧)
true

julia> isrightassociative(→)
true
```

See also [`AbstractOperator`](@ref).
"""
isrightassociative(::Type{<:AbstractOperator}) = true
isrightassociative(o::AbstractOperator) = isrightassociative(typeof(o))

############################################################################################

"""
    abstract type AbstractFormula end

A logical formula encoding a statement
which truth can be evaluated on interpretations (or models) of the logic.

Its syntactic component is canonically encoded via a syntax tree (see [`SyntaxTree`](@ref)),
and it can be anchored to a logic (see [`Formula`](@ref)).

# Implementation

When implementing a new formula type `MyCustomFormulaType`, please provide a method `tree`
for extracting its syntax tree representation:

    function tree(f::MyCustomFormulaType)::SyntaxTree
        ...
    end

As well as a method used for composing formulas:

    function (op::AbstractOperator)(
        children::NTuple{N,Union{AbstractSyntaxToken,MyCustomFormulaType}},
    )::AbstractFormula where {N}
        # Composed formula
    end

See also
[`Formula`](@ref), [`SyntaxTree`](@ref),
[`AbstractSyntaxStructure`](@ref), [`AbstractLogic`](@ref).
"""
abstract type AbstractFormula end

"""
    joinformulas(
        op::AbstractOperator,
        ::NTuple{N,F}
    )::F where {N,F<:AbstractFormula}

Return a new formula of type `F` by composing `N` formulas of the same type
via an operator `op`. This function allows one to use operators for flexibly composing
formulas (see *Implementation*).

# Examples
```julia-repl
julia> f = parseformula("◊(p→q)");

julia> p = Atom("p");

julia> ∧(f, p)  # Easy way to compose a formula
SyntaxTree: ◊(p → q) ∧ p

julia> f ∧ ¬p   # Leverage infix notation ;)
SyntaxTree: ◊(p → q) ∧ ¬p

julia> ∧(f, p, ¬p) # Shortcut for ∧(f, ∧(p, ¬p))
SyntaxTree: ◊(p → q) ∧ p ∧ ¬p
```

# Implementation

Upon `joinformulas` lies a flexible way of using operators for composing
formulas and syntax tokens (e.g., atoms), given by methods like the following:

    function (op::AbstractOperator)(
        children::NTuple{N,Union{AbstractSyntaxToken,AbstractFormula}},
    ) where {N}
        ...
    end

These allow composing formulas as in `∧(f, ¬p)`, and in order to access this composition
with any newly defined subtype of `AbstractFormula`,
a new method for `joinformulas` should be defined, together with
promotion from/to other `AbstractFormula`s should be taken care of (see
[here](https://docs.julialang.org/en/v1/manual/conversion-and-promotion/)
and [here](https://github.com/JuliaLang/julia/blob/master/base/promotion.jl)).

Similarly,
for allowing a (possibly newly defined) operator to be applied on a number of
syntax tokens/formulas that differs from its arity,
for any newly defined operator `op`, new methods
similar to the two above should be defined.
For example, although ∧ and ∨ are binary, (i.e., have arity equal to 2),
compositions such as `∧(f, f2, f3, ...)` and `∨(f, f2, f3, ...)` can be done
thanks to the following two methods that were defined in SoleLogics:

    function ∧(
        c1::Union{AbstractSyntaxToken,AbstractFormula},
        c2::Union{AbstractSyntaxToken,AbstractFormula},
        c3::Union{AbstractSyntaxToken,AbstractFormula},
        cs::Union{AbstractSyntaxToken,AbstractFormula}...
    )
        return ∧(c1, ∧(c2, c3, cs...))
    end
    function ∨(
        c1::Union{AbstractSyntaxToken,AbstractFormula},
        c2::Union{AbstractSyntaxToken,AbstractFormula},
        c3::Union{AbstractSyntaxToken,AbstractFormula},
        cs::Union{AbstractSyntaxToken,AbstractFormula}...
    )
        return ∨(c1, ∨(c2, c3, cs...))
    end

See also
[`AbstractFormula`](@ref),
[`AbstractOperator`](@ref).
"""
function joinformulas(op::AbstractOperator, ::NTuple{N,F})::F where {N,F<:AbstractFormula}
    return error("Please, provide method " *
        "joinformulas(op::AbstractOperator, children::NTuple{N,$(F)}) where {N}.")
end

function joinformulas(op::AbstractOperator, children::Vararg{F,N})::F where {N,F<:AbstractFormula}
    joinformulas(op, children)
end

# Resolve ambiguity with nullary operators
function joinformulas(op::AbstractOperator, children::NTuple{0})
    return SyntaxTree(op, children)
end

function joinformulas(op::AbstractOperator, children::NTuple{N,AbstractSyntaxToken}) where {N}
    return SyntaxTree(op, children)
end

"""
    abstract type AbstractSyntaxStructure <: AbstractFormula end

A logical formula, represented by its syntactic component.
The typical representation is the [`SyntaxTree`](@ref);
however, different implementations can cover specific synctactic forms
(e.g., conjuctive/disjuctive normal forms).

See also
[`tree`](@ref),
[`SyntaxTree`](@ref),
[`AbstractFormula`](@ref),
[`AbstractLogic`](@ref).
"""
abstract type AbstractSyntaxStructure <: AbstractFormula end

"""
    Base.in(tok::AbstractSyntaxToken, f::AbstractFormula)::Bool

Return whether a syntax token appears in a formula.

See also [`AbstractSyntaxToken`](@ref).
"""
function Base.in(tok::AbstractSyntaxToken, f::AbstractFormula)::Bool
    return Base.in(tok, tree(f))
end


function syntaxstring(f::AbstractFormula; kwargs...)
    syntaxstring(tree(f); kwargs...)
end

function Base.show(io::IO, f::AbstractFormula)
    print(io, "$(typeof(f))\nsyntaxstring: $(syntaxstring(f))")
end


doc_tokopprop = """
    tokens(f::AbstractFormula)::AbstractVector{<:AbstractSyntaxToken}
    operators(f::AbstractFormula)::AbstractVector{<:AbstractOperator}
    atoms(f::AbstractFormula)::AbstractVector{<:Atom}
    ntokens(f::AbstractFormula)::Integer
    noperators(f::AbstractFormula)::Integer
    natoms(f::AbstractFormula)::Integer

Return the list or the number of (unique) syntax
tokens/operators/atoms appearing in a formula.

See also [`AbstractSyntaxStructure`](@ref).
"""

"""$(doc_tokopprop)"""
function tokens(f::AbstractFormula)::AbstractVector{<:AbstractSyntaxToken}
    return tokens(tree(f))
end
"""$(doc_tokopprop)"""
function operators(f::AbstractFormula)::AbstractVector{<:AbstractOperator}
    return operators(tree(f))
end
"""$(doc_tokopprop)"""
function atoms(f::AbstractFormula)::AbstractVector{<:Atom}
    return atoms(tree(f))
end
"""$(doc_tokopprop)"""
function ntokens(f::AbstractFormula)::Integer
    return ntokens(tree(f))
end
"""$(doc_tokopprop)"""
function natoms(f::AbstractFormula)::Integer
    return natoms(tree(f))
end
"""$(doc_tokopprop)"""
function height(f::AbstractFormula)::Integer
    return height(tree(f))
end

# Helpers that make all AbstractFormula's map to the same
#  dictionary key. Useful when checking formulas on interpretations.
function Base.isequal(a::AbstractFormula, b::AbstractFormula)
    Base.isequal(tree(a), tree(b))
end
Base.hash(a::AbstractFormula) = Base.hash(tree(a))

Base.promote_rule(::Type{SS}, ::Type{<:AbstractSyntaxToken}) where {SS<:AbstractSyntaxStructure} = SS
Base.promote_rule(::Type{<:AbstractSyntaxToken}, ::Type{SS}) where {SS<:AbstractSyntaxStructure} = SS

############################################################################################

"""
    struct SyntaxTree{
        T<:AbstractSyntaxToken,
    } <: AbstractSyntaxStructure
        token::T
        children::NTuple{N,SyntaxTree} where {N}
    end

A syntax tree encoding a logical formula.
Each node of the syntax tree holds a `token`, and
has as many children as the `arity` of the token.

This implementation is *arity-compliant*, in that, upon construction,
the arity is checked against the number of children provided.

See also [`token`](@ref), [`children`](@ref), [`tokentype`](@ref),
[`tokens`](@ref), [`operators`](@ref), [`atoms`](@ref),
[`ntokens`](@ref), [`natoms`](@ref), [`height`](@ref),
[`tokenstype`](@ref), [`operatorstype`](@ref), [`atomstype`](@ref),
[`AbstractSyntaxToken`](@ref), [`arity`](@ref), [`Atom`](@ref), [`AbstractOperator`](@ref).
"""
struct SyntaxTree{
    T<:AbstractSyntaxToken,
} <: AbstractSyntaxStructure

    # The syntax token at the current node
    token::T

    # The child nodes of the current node
    children::NTuple{N,SyntaxTree} where {N}

    function _aritycheck(N, T, token, children)
        @assert arity(token) == N "Cannot instantiate SyntaxTree{$(T)} with token " *
                                  "$(token) of arity $(arity(token)) and $(N) children."
        return nothing
    end

    function SyntaxTree{T}(
        token::T,
        children::NTuple{N,Union{AbstractSyntaxToken,AbstractSyntaxStructure}} = (),
    ) where {T<:AbstractSyntaxToken,N}
        children = convert.(SyntaxTree, children)
        _aritycheck(N, T, token, children)
        return new{T}(token, children)
    end

    function SyntaxTree{T}(
        t::SyntaxTree{T},
    ) where {T<:AbstractSyntaxToken}
        return SyntaxTree{T}(token(t), children(t))
    end

    function SyntaxTree(
        token::T,
        children::NTuple{N,Union{AbstractSyntaxToken,AbstractSyntaxStructure}} = (),
    ) where {T<:AbstractSyntaxToken,N}
        children = convert.(SyntaxTree, children)
        _aritycheck(N, T, token, children)
        return new{T}(token, children)
    end
end

# Helpers
function SyntaxTree{T}(token::T, children...) where {T<:AbstractSyntaxToken}
    return SyntaxTree{T}(token, children)
end
function SyntaxTree(token::T, children...) where {T<:AbstractSyntaxToken}
    return SyntaxTree(token, children)
end

# Getters
token(t::SyntaxTree) = t.token
children(t::SyntaxTree) = t.children

tokentype(::SyntaxTree{T}) where {T} = T
tokenstype(t::SyntaxTree) = Union{tokentype(t),tokenstype.(children(t))...}
operatorstype(t::SyntaxTree) = typeintersect(AbstractOperator, tokenstype(t))
atomstype(t::SyntaxTree) = typeintersect(Atom, tokenstype(t))

# Shows the type of the syntax tree and its syntaxstring.
# Base.show(io::IO, t::SyntaxTree) = print(io, "$(typeof(t))($(syntaxstring(t)))")
function Base.show(io::IO, t::SyntaxTree)
    print(io, "$(syntaxstring(t))")
    # print(io, "Allowed token types: $(tokenstype(t))")
end


"""
    Base.in(tok::AbstractSyntaxToken, tree::SyntaxTree)::Bool

Return whether a token appears in a syntax tree or not.

See also [`tokens`](@ref), [`SyntaxTree`](@ref).
"""
function Base.in(tok::AbstractSyntaxToken, tree::SyntaxTree)
    return tok == token(tree) || any([Base.in(tok, c) for c in children(tree)])
end

"""
    tokens(t::SyntaxTree)::AbstractVector{AbstractSyntaxToken}

List all tokens appearing in a syntax tree.

See also [`ntokens`](@ref), [`operators`](@ref), [`atoms`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function tokens(t::SyntaxTree)::AbstractVector{AbstractSyntaxToken}
    return AbstractSyntaxToken[vcat(tokens.(children(t))...)..., token(t)]
end

"""
    operators(t::SyntaxTree)::AbstractVector{AbstractOperator}

List all operators appearing in a syntax tree.

See also [`noperators`](@ref), [`atoms`](@ref), [`tokens`](@ref), [`AbstractOperator`](@ref).
"""
function operators(t::SyntaxTree)::AbstractVector{AbstractOperator}
    ops = token(t) isa AbstractOperator ? [token(t)] : []
    return AbstractOperator[vcat(operators.(children(t))...)..., ops...]
end

"""
    atoms(t::SyntaxTree)::AbstractVector{Atom}

List all atoms appearing in a syntax tree.

See also [`natoms`](@ref), [`operators`](@ref), [`tokens`](@ref), [`Atom`](@ref).
"""
function atoms(t::SyntaxTree)::AbstractVector{Atom}
    ps = token(t) isa Atom ? Atom[token(t)] : Atom[]
    return Atom[vcat(atoms.(children(t))...)..., ps...]
end

"""
    ntokens(t::SyntaxTree)::Integer

Return the count of all tokens appearing in a syntax tree.

See also [`tokens`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function ntokens(t::SyntaxTree)::Integer
    length(children(t)) == 0 ? 1 : 1 + sum(ntokens(c) for c in children(t))
end

"""
    noperators(t::SyntaxTree)::Integer

Return the count of all operators appearing in a syntax tree.

See also [`operaters`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function noperators(t::SyntaxTree)::Integer
    op = token(t) isa AbstractOperator ? 1 : 0
    return length(children(t)) == 0 ? op : op + sum(noperators(c) for c in children(t))
end

"""
    natoms(t::SyntaxTree)::Integer

Return the count of all atoms appearing in a syntax tree.

See also [`atoms`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function natoms(t::SyntaxTree)::Integer
    pr = token(t) isa Atom ? 1 : 0
    return length(children(t)) == 0 ? pr : pr + sum(natoms(c) for c in children(t))
end

"""
    height(t::SyntaxTree)::Integer

Return the height of a syntax tree.

See also [`tokens`](@ref), [`AbstractSyntaxToken`](@ref).
"""
function height(t::SyntaxTree)::Integer
    length(children(t)) == 0 ? 0 : 1 + maximum(height(c) for c in children(t))
end

# Helpers that make SyntaxTree's map to the same dictionary key.
# Useful for checking formulas on interpretations.
function Base.isequal(a::SyntaxTree, b::SyntaxTree)
    return Base.isequal(token(a), token(b)) &&
        all(((c1,c2),)->Base.isequal(c1, c2), zip(children(a), children(b)))
end
Base.hash(a::SyntaxTree) = Base.hash(syntaxstring(a))

# Refer to syntaxstring(tok::AbstractSyntaxToken; kwargs...) for documentation
function syntaxstring(
    t::SyntaxTree;
    function_notation = false,
    remove_redundant_parentheses = true,
    parenthesize_atoms = !remove_redundant_parentheses,
    kwargs...
)
    ch_kwargs = merge((; kwargs...), (;
        function_notation = function_notation,
        remove_redundant_parentheses = remove_redundant_parentheses,
        parenthesize_atoms = parenthesize_atoms,
    ))

    # Parenthesization rules for binary operators in infix notation
    function _binary_infix_syntaxstring(tok::AbstractSyntaxToken, ch::SyntaxTree; relation::Symbol=:left)
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

        tprec = Base.operator_precedence(tok)
        chprec = Base.operator_precedence(chtok)

        if ((!iscommutative(tok) || tok != chtok) && (tprec > chprec)) || # "◊¬p ∧ ¬q" instead of "(◊¬p) ∧ (¬q)"
            (!iscommutative(tok) && tprec <= chprec)                      # "(q → p) → ¬q" instead of "q → p → ¬q"
            lpar, rpar = "(", ")"
        end

        return "$(lpar)$(chtokstring)$(rpar)"
    end

    tok = token(t)
    tokstr = syntaxstring(tok; ch_kwargs...)

    if arity(tok) == 0 # A leaf nodes parenthesization is responsability of its parent
        return tokstr
    elseif arity(tok) == 2 && !function_notation # Infix notation for binary operators
        "$(_binary_infix_syntaxstring(tok, children(t)[1]; relation=:left)) "*
        "$tokstr $(_binary_infix_syntaxstring(tok, children(t)[2]; relation=:right))"
    else # Infix notation with arity != 2, or function notation
        lpar, rpar = "(", ")"
        charity = arity(token(children(t)[1]))
        if !function_notation && arity(tok) == 1 &&
            (charity == 1 || (charity == 0 && !parenthesize_atoms))
            # When not in function notation, print "¬p" instead of "¬(p)";
            # note that "◊((p ∧ q) → s)" must not be simplified as "◊(p ∧ q) → s".
            lpar, rpar = "", ""
        end

        length(children(t)) == 0 ?
               tokstr :
               tokstr * "$(lpar)" * join(
                    [syntaxstring(c; ch_kwargs...) for c in children(t)], ", ") * "$(rpar)"
    end
end

# Syntax tree, the universal syntax structure representation,
#  wins when promoted with syntax structures/tokens and syntax trees
Base.promote_rule(::Type{<:SyntaxTree}, ::Type{<:SyntaxTree}) = SyntaxTree
Base.promote_rule(::Type{<:AbstractSyntaxStructure}, ::Type{S}) where {S<:SyntaxTree} = S
Base.promote_rule(::Type{S}, ::Type{<:AbstractSyntaxStructure}) where {S<:SyntaxTree} = S

# Helper
Base.convert(::Type{S}, tok::AbstractSyntaxToken) where {S<:SyntaxTree} = S(tok)
Base.convert(::Type{AbstractSyntaxStructure}, tok::AbstractSyntaxToken) = SyntaxTree(tok)

function joinformulas(op::AbstractOperator, children::NTuple{N,SyntaxTree}) where {N}
    return SyntaxTree(op, children)
end


"""
    tree(f::AbstractFormula)::SyntaxTree

Extract the `SyntaxTree` representation of a formula
(equivalent to `Base.convert(SyntaxTree, f)`).

See also
[`SyntaxTree`](@ref),
[`AbstractSyntaxStructure`](@ref).
[`AbstractFormula`](@ref),
"""
function tree(f::AbstractFormula)::SyntaxTree
    return error("Please, provide method tree(::$(typeof(f)))::SyntaxTree.")
end
Base.convert(::Type{SyntaxTree}, f::AbstractFormula) = tree(f)

tree(t::SyntaxTree) = t

tree(t::AbstractSyntaxToken) = SyntaxTree(t)

############################################################################################

"""
    abstract type AbstractAlphabet{A} end

Abstract type for representing an alphabet of atoms with values of type `A`.
An alphabet (or *propositional alphabet*) is a set of atoms
(assumed to be
[countable](https://en.wikipedia.org/wiki/Countable_set)).

See also [`ExplicitAlphabet`](@ref), [`AlphabetOfAny`](@ref),
[`atomstype`](@ref), [`valuetype`](@ref),
[`Atom`](@ref), [`AbstractGrammar`](@ref).

# Examples

```julia-repl
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
function Base.in(atom::Union{AbstractString,Number,AbstractChar}, a::AbstractAlphabet)
    @warn "Please, use Base.in(Atom($(atom)), alphabet::$(typeof(a))) instead of " *
        "Base.in($(atom), alphabet::$(typeof(a)))"
    Base.in(Atom(atom), a)
end

"""
    Base.length(a::AbstractAlphabet)::Bool

Return the alphabet length, if it is finite.

See also [`AbstractAlphabet`](@ref), [`SyntaxTree`](@ref).
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

See also [`AbstractAlphabet`](@ref), [`SyntaxTree`](@ref).
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

"""
    struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
        atoms::Vector{Atom{A}}
    end

An alphabet wrapping atoms in a (finite) `Vector`.

See also [`atoms`](@ref), [`AbstractAlphabet`](@ref).
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

Base.convert(::Type{AbstractAlphabet}, alphabet::Vector{<:Atom}) = ExplicitAlphabet(alphabet)
"""
    struct AlphabetOfAny{A} <: AbstractAlphabet{A} end

An implicit, infinite alphabet that includes all atoms with values of a subtype of A.

See also [`AbstractAlphabet`](@ref).
"""
struct AlphabetOfAny{A} <: AbstractAlphabet{A} end
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.in(::Atom{PA}, ::AlphabetOfAny{AA}) where {PA,AA} = (PA <: AA)

############################################################################################

"""
    abstract type AbstractGrammar{A<:AbstractAlphabet,O<:AbstractOperator} end

Abstract type for representing a
[context-free grammar](https://en.wikipedia.org/wiki/Context-free_grammar)
based on a *single* alphabet of type `A`, and a set of operators
that consists of all the (singleton) child types of `O`.
A context-free grammar is a simple structure for defining formulas inductively.

See also [`alphabet`](@ref),
[`atomstype`](@ref), [`tokenstype`](@ref),
[`operatorstype`](@ref), [`alphabettype`](@ref),
[`AbstractAlphabet`](@ref), [`AbstractOperator`](@ref).
"""
abstract type AbstractGrammar{A<:AbstractAlphabet,O<:AbstractOperator} end

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

function Base.in(tok::AbstractSyntaxToken, g::AbstractGrammar)
    return error("Please, provide method Base.in(::$(typeof(tok)), ::$(typeof(g))).")
end

# Note: when using this file's syntax tokens, these methods suffice:
Base.in(p::Atom, g::AbstractGrammar) = Base.in(p, alphabet(g))
Base.in(op::AbstractOperator, g::AbstractGrammar) = (op <: operatorstype(g))


"""
    Base.in(t::SyntaxTree, g::AbstractGrammar)::Bool

Return whether a formula,
encoded as a `SyntaxTree`, belongs to a grammar.

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
function Base.in(::SyntaxTree, g::AbstractGrammar)::Bool
    return error("Please, provide method Base.in(::SyntaxTree, ::$(typeof(g))).")
end

"""
    formulas(
        g::AbstractGrammar;
        maxdepth::Integer,
        nformulas::Union{Nothing,Integer} = nothing,
        args...
    )::Vector{<:SyntaxTree}

Enumerate the formulas produced by a given grammar with a finite and iterable alphabet.

# Implementation

Additional `args` can be used to model the function's behavior.
At least these two arguments should be covered:
- a `nformulas` argument can be used to limit the size of the returned `Vector`;
- a `maxdepth` argument can be used to limit the syntactic component, represented as a syntax tree,
to a given maximum depth;

See also [`AbstractGrammar`](@ref), [`SyntaxTree`](@ref).
"""
function formulas(
    g::AbstractGrammar{A,O} where {A,O};
    maxdepth::Integer,
    nformulas::Union{Nothing,Integer} = nothing,
    args...
)::Vector{<:SyntaxTree}
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


"""
    struct CompleteFlatGrammar{A<:AbstractAlphabet,O<:AbstractOperator} <: AbstractGrammar{A,O}
        alphabet::A
        operators::Vector{<:O}
    end

A grammar of all well-formed formulas obtained by the arity-complying composition
of atoms of an alphabet of type `A`, and all operators in `operators`.
With n operators, this grammar has exactly n+1 production rules.
For example, with `operators = [⊥,∧,∨]`, the grammar (in Backus-Naur form) is:

    φ ::= p | ⊥ | φ ∧ φ | φ ∨ φ

with p ∈ alphabet. Note: it is *flat* in the sense that all rules substitute the same
(unique and starting) non-terminal symbol φ.

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
    return [atoms(alphabet(g))..., filter(isnullary, operators(g))...]
end

# A complete grammar includes any *safe* syntax tree that can be built with
#  the grammar token types.
function Base.in(t::SyntaxTree, g::CompleteFlatGrammar)::Bool
    return if token(t) isa Atom
        token(t) in alphabet(g)
    elseif token(t) isa AbstractOperator
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
    )::Vector{SyntaxTree}

Generate all formulas with syntax trees that are not taller than a given `maxdepth`.

See also [`AbstractGrammar`](@ref).
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
    cur_formulas = convert.(SyntaxTree, terminals(g))
    all_formulas = SyntaxTree[cur_formulas...]
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

See also [`top`](@ref), [`bottom`](@ref), [`istop`](@ref), [`isbottom`](@ref), [`Algebra`](@ref).
"""
const TruthValue = Any

"""
    istop(::TruthValue)::Bool

Return true if the truth value is the top of its algebra.
For example, in the crisp case, with `Bool` truth values, it is:

    istop(t::Bool)::Bool = (t == true)

See also [`isbottom`](@ref), [`TruthValue`](@ref).
"""
istop(t::TruthValue)::Bool = error("Please, provide method istop(truthvalue::$(typeof(t))).")

"""
    isbottom(::TruthValue)::Bool

Return true if the truth value is the bottom of its algebra.
For example, in the crisp case, with `Bool` truth values, it is:

    isbottom(t::Bool)::Bool = (t == false)

See also [`istop`](@ref), [`TruthValue`](@ref).
"""
isbottom(t::TruthValue)::Bool = error("Please, provide method isbottom(truthvalue::$(typeof(t))).")

"""
   default_algebra(::Type{T})::AbstractAlgebra{<:T} where {T<:TruthValue}

Return the fallback algebra for a given truth value type.

# Implementation

In order to check syntax trees without algebras, truth values should provide
a default algebra it works with.
"""
function default_algebra(::Type{T})::AbstractAlgebra{<:T} where {T<:TruthValue}
    return error("Please, provide method " *
                 "default_algebra(::$(typeof(T)))::AbstractAlgebra{<:$(T)}.")
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
It can be typed by `\\top<tab>`.

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
It can be typed by `\\bot<tab>`.

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
truth of atoms and the semantics of operators. They typically encode a
[lattice structure](https://en.wikipedia.org/wiki/Lattice_(order)) where two
elements(or nodes) *⊤* and *⊥* are referred to as *top* (or maximum)
and *bottom* (or minimum). Each node in the lattice represents a truth value
that an atom or a formula can have on an interpretation, and the
semantics of operators is given in terms of operations between truth values.

# Implementation

When implementing a new algebra type, the methods `domain`,
`top`, and `bottom` should be implemented.

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

Return the `domain` of a given algebra.

See also [`AbstractAlgebra`](@ref).
"""
function domain(a::AbstractAlgebra{T} where {T<:TruthValue})::AbstractVector{T}
    return error("Please, provide method domain(::$(typeof(a))).")
end

# Note: maybe one day this will have a use?
# Base.in(t::TruthValue, a::AbstractAlgebra) = Base.in(t, domain(a))

"""
    top(a::AbstractAlgebra)

Return the `top` of a given algebra.

See also [`AbstractAlgebra`](@ref).
"""
function top(a::AbstractAlgebra{T} where {T})::T
    return error("Please, provide method top(::$(typeof(a))).")
end

"""
    bottom(a::AbstractAlgebra)

Return the `bottom` of a given algebra.

See also [`AbstractAlgebra`](@ref).
"""
function bottom(a::AbstractAlgebra{T} where {T})::T
    return error("Please, provide method bottom(::$(typeof(a))).")
end

"""
    iscrisp(A::Type{<:AbstractAlgebra}) = (truthtype(A) == Bool)
    iscrisp(a::AbstractAlgebra) = iscrisp(typeof(a))

An algebra is crisp (or *boolean*) when its domain type is... `Bool`, quite literally!
The antonym of crisp is *fuzzy*.

See also [`AbstractAlgebra`](@ref).
"""
iscrisp(A::Type{<:AbstractAlgebra}) = (truthtype(A) == Bool)
iscrisp(a::AbstractAlgebra) = iscrisp(typeof(a))

############################################################################################

"""
    abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

Abstract type of a logic, which comprehends a context-free grammar (*syntax*) and
an algebra (*semantics*).

# Implementation

When implementing a new logic type,
the methods `grammar` and `algebra` should be implemented.

See also [`AbstractGrammar`](@ref), [`AbstractAlgebra`](@ref).
"""
abstract type AbstractLogic{G<:AbstractGrammar,A<:AbstractAlgebra} end

"""
    grammar(l::AbstractLogic{G})::G where {G<:AbstractGrammar}

Return the `grammar` of a given logic.

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
atomstype(l::AbstractLogic) = atomstype(alphabet(l))
tokenstype(l::AbstractLogic) = tokenstype(grammar(l))
formulas(l::AbstractLogic, args...; kwargs...) = formulas(grammar(l), args...; kwargs...)

Base.in(op::AbstractOperator, l::AbstractLogic) = Base.in(op, grammar(l))
Base.in(t::SyntaxTree, l::AbstractLogic) = Base.in(t, grammar(l))
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
bottom(l::AbstractLogic) = bottom(algebra(l))
iscrisp(l::AbstractLogic) = iscrisp(algebra(l))

function Base.isequal(a::AbstractLogic, b::AbstractLogic)
    Base.isequal(grammar(a), grammar(b)) &&
    Base.isequal(algebra(a), algebra(b))
end
Base.hash(a::AbstractLogic) = Base.hash(grammar(a)) + Base.hash(algebra(a))

############################################################################################

"""
    struct Formula{L<:AbstractLogic} <: AbstractFormula
        _logic::Base.RefValue{L}
        synstruct::AbstractSyntaxStructure
    end

A formula anchored to a logic of type `L`,
and wrapping a syntax structure.
The structure encodes a formula belonging to the grammar
of the logic, and the truth of the formula can be evaluated
on interpretations of the same logic. Note that, here, the logic is represented by a reference.

Upon construction, the logic can be passed either directly, or via a RefValue.
Additionally, the following keyword arguments may be specified:
- `check_atoms::Bool = false`: whether to perform or not a check that the atoms
    belong to the alphabet of the logic;
- `check_tree::Bool = false`: whether to perform or not a check that the formula's syntactic structure
    honors the grammar (includes the check performed with `check_atoms = true`) (TODO);

*Cool feature*: a `Formula` can be used for instating other formulas of the same logic.
See the examples.

# Examples
```julia-repl
julia> f = parsebaseformula("◊(p→q)");

julia> f2 = f(parseformula("p"));

julia> syntaxstring(f)
"◊(→(p, q))"

julia> syntaxstring(f2)
"p"

julia> @assert logic(f) == logic(f2)

julia> @assert ◊ in operators(logic(f2))

julia> @assert ◊ isa operatorstype(logic(f2))

```

See also
[`tree`](@ref), [`logic`](@ref),
[`AbstractSyntaxToken`](@ref), [`SyntaxTree`](@ref),
[`AbstractLogic`](@ref).
"""
struct Formula{L<:AbstractLogic} <: AbstractFormula
    _logic::Base.RefValue{L}
    synstruct::AbstractSyntaxStructure

    _l(l::AbstractLogic) = Base.RefValue(l)
    _l(l::Base.RefValue) = l

    function Formula{L}(
        l::Union{L,Base.RefValue{L}},
        tokt::Union{AbstractSyntaxToken,AbstractSyntaxStructure};
        check_atoms::Bool = false,
        check_tree::Bool = false,
    ) where {L<:AbstractLogic}
        _logic = _l(l)
        synstruct = convert(AbstractSyntaxStructure, tokt)

        if check_tree
            return error("TODO implement check_tree parameter when constructing Formula's!")
        end
        # Check that the atoms belong to the alphabet of the logic
        if !check_tree && check_atoms
            @assert all([p in alphabet(_logic[])
                         for p in atoms(synstruct)]) "Cannot " *
                           "instantiate Formula{$(L)} with illegal atoms: " *
                           "$(filter((p)->!(p in alphabet(_logic[])), atoms(synstruct)))"
        end

        # Check that the token types of the tree are a subset of the tokens
        #  allowed by the logic
        @assert tokenstype(synstruct) <: tokenstype(_logic[]) "Cannot " *
                             "instantiate Formula{$(L)} with illegal token types $(tokenstype(synstruct)). " *
                             "Token types should be <: $(tokenstype(_logic[]))."

        return new{L}(_logic, synstruct)
    end

    # function Formula{L}(
    #     l::Union{L,Base.RefValue{L}},
    #     tokt::Union{AbstractSyntaxToken,AbstractSyntaxStructure};
    #     kwargs...
    # ) where {L<:AbstractLogic}
    #     t = convert(SyntaxTree, tokt)
    #     return Formula{L,typeof(t)}(l, t; kwargs...)
    # end

    function Formula(
        l::Union{L,Base.RefValue{L}},
        tokt;
        kwargs...
    ) where {L<:AbstractLogic}
        return Formula{L}(l, tokt; kwargs...)
    end
end

_logic(f::Formula) = f._logic
logic(f::Formula) = f._logic[]
synstruct(f::Formula) = f.synstruct
tree(f::Formula) = tree(f.synstruct)

function Base.show(io::IO, f::Formula)
    println(io, "Formula: $(syntaxstring(f))")
    print(io, "Anchored to logic: ")
    Base.show(io, logic(f))
end

# Note that, since `op` might not be in the logic of the child formulas,
#  the resulting formula may be of a different logic.
function joinformulas(op::AbstractOperator, children::NTuple{N,Formula}) where {N}
    ls = unique(logic.(children)) # Uses Base.isequal
    @assert length(ls) == 1 "Cannot " *
                "build formula by combination of formulas with different logics: $(ls)."
    l = first(ls)
    # "TODO expand logic's set of operators (op is not in it: $(typeof(op)) ∉ $(operatorstype(l)))."
    @assert typeof(op) <: operatorstype(l) "Cannot join $(N) formulas via operator $(op): " *
        "this operator does not belong to the logic. $(typeof(op)) <: $(operatorstype(l)) should hold!"
    return Formula(l, joinformulas(op, map(synstruct, children)))
end

# When constructing a new formula from a syntax tree, the logic is passed by reference.
(f::Formula)(t::AbstractSyntaxStructure, args...) = Formula(_logic(f), t, args...)

# A logic can be used to instantiate `Formula`s out of syntax trees.
(l::AbstractLogic)(t::AbstractSyntaxStructure, args...) = Formula(Base.RefValue(l), t; args...)

# Adapted from https://github.com/JuliaLang/julia/blob/master/base/promotion.jl
function Base._promote(x::Formula, y::AbstractSyntaxStructure)
    @inline
    return (x, x(y))
end

function Base._promote(x::Formula, y::AbstractSyntaxToken)
    Base._promote(x, Base.convert(SyntaxTree, y))
end
Base._promote(x::Union{AbstractSyntaxToken,AbstractSyntaxStructure}, y::Formula) = reverse(Base._promote(y, x))

iscrisp(f::Formula) = iscrisp(logic(f))
grammar(f::Formula) = grammar(logic(f))
algebra(f::Formula) = algebra(logic(f))


############################################################################################

"""
    abstract type AbstractInterpretation{A,T<:TruthValue} end

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
abstract type AbstractInterpretation{A,T<:TruthValue} end

valuetype(::AbstractInterpretation{A,T}) where {A,T} = A
truthtype(::AbstractInterpretation{A,T}) where {A,T} = T

"""
    check(
        f::AbstractFormula,
        m::AbstractInterpretation{A,T},
        args...
    )::T where {A,T<:TruthValue}

Check a formula on a logical interpretation (or model), returning a truth value.
This process is referred to as
[model checking](https://en.wikipedia.org/wiki/Model_checking), and there are many
algorithms for it, typically depending on the complexity of the logic.

# Examples
```julia-repl
julia> @atoms String p q
2-element Vector{Atom{String}}:
 Atom{String}("p")
 Atom{String}("q")

julia> td = TruthDict([p => true, q => false])
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│  false │   true │
└────────┴────────┘

julia> check(CONJUNCTION(p,q), td)
false
```

See also [`AbstractFormula`](@ref), [`AbstractInterpretation`](@ref).
"""
function check(
    f::AbstractFormula,
    m::AbstractInterpretation{A,T},
    args...,
)::T where {A,T<:TruthValue}
    return error("Please, provide method " *
                 "check(f::$(typeof(f)), m::$(typeof(m)), " *
                 "args...::$(typeof(args))::$(truthtype(m)).")
end

# Helper: use default algebra when checking on an abstract syntax tree
function check(t::AbstractSyntaxStructure, m::AbstractInterpretation, args...)
    return check(default_algebra(truthtype(m)), t, m, args...)
end

############################################################################################
######################################### UTILS ############################################
############################################################################################

# We provide an extra safety layer by complementing Base.in with syntax tokens/trees and alphabets.
function Base.in(t::Union{AbstractSyntaxToken,AbstractSyntaxStructure}, a::AbstractAlphabet)
    return error("Attempting Base.in($(typeof(t)), ::$(typeof(a))), " *
                 "but objects of type $(typeof(t)) cannot belong to alphabets.")
end

"""
An alphabet of `valuetype` `A` can be used for instantiating atoms of valuetype `A`.
"""
(::AbstractAlphabet{A})(a) where {A} = Atom{A}(a)

"""
An operator can be used to compose syntax tokens (e.g., atoms),
syntax trees and/or formulas. This is quite handy, try it:

    ¬(Atom(1)) ∨ Atom(1) ∧ ⊤
    ∧(⊤,⊤)
    ⊤()
"""
function (op::AbstractOperator)(o::Any)
    return error("Cannot apply operator $(op)::$(typeof(op)) to object $(o)::$(typeof(o))")
end

function (op::AbstractOperator)(children::Union{AbstractSyntaxToken,AbstractFormula}...)
    return op(children)
end
function (op::AbstractOperator)(
    children::NTuple{N,Union{AbstractSyntaxToken,AbstractFormula}},
) where {N}
    T = Base.promote_type((typeof.(children))...)
    if T <: Union{SyntaxTree,AbstractSyntaxToken}
        return joinformulas(op, tree.(children))
    elseif T <: AbstractSyntaxStructure
        return joinformulas(op, children) # Force SyntaxTree?
        # return joinformulas(op, Base.promote(children...))
        # println(typeof.(children))
        # println(typeof.(Base.promote(children...)))
        # return joinformulas(op, children)
    else
        # println(typeof.(children))
        return joinformulas(op, Base.promote(children...))
    end
end

"""
    const SYNTACTICAL = Union{AbstractSyntaxStructure,AbstractSyntaxToken}

Union type representing a generic [`AbstractSyntaxStructure`](@ref) or a single token
in it.

See also [`AbstractSyntaxStructure`](@ref), [`AbstractSyntaxToken`](@ref).
"""
const SYNTACTICAL = Union{AbstractSyntaxStructure,AbstractSyntaxToken}
