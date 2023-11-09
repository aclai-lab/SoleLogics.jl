# This file collects some of the SoleLogics methods docstrings.
# The only purpose of this file is to make code easier to scroll and read.

# If you are a user, please, consider using the "?" section in the Julia REPL to read
# docstrings.

doc_syntaxstring = """
    syntaxstring(s::Syntactical; kwargs...)::String

Return the string representation of any syntactic object (e.g.,
`Formula`, `SyntaxTree`, `SyntaxToken`, `Atom`, `Truth`, etc).
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
   it forces the atoms (which are the leaves of a formula's tree structure) to be
   wrapped in parentheses.

# Examples
```jldoctest
julia> syntaxstring(parseformula("p∧q∧r∧s∧t"))
"p ∧ q ∧ r ∧ s ∧ t"

julia> syntaxstring(parseformula("p∧q∧r∧s∧t"), function_notation=true)
"∧(∧(∧(∧(p, q), r), s), t)"

julia> syntaxstring(parseformula("p∧q∧r∧s∧t"), remove_redundant_parentheses=false)
"((((p) ∧ (q)) ∧ (r)) ∧ (s)) ∧ (t)"

julia> syntaxstring(parseformula("p∧q∧r∧s∧t"), remove_redundant_parentheses=true, parenthesize_atoms=true)
"(p) ∧ (q) ∧ (r) ∧ (s) ∧ (t)"

julia> syntaxstring(parseformula("◊((p∧s)→q)"))
"◊((p ∧ s) → q)"

julia> syntaxstring(parseformula("◊((p∧s)→q)"); function_notation = true)
"◊(→(∧(p, s), q))"
```

See also [`parseformula`](@ref),
[`SyntaxBranch`](@ref), [`SyntaxToken`](@ref).

# Implementation

In the case of a syntax tree, `syntaxstring` is a recursive function that calls
itself on the syntax children of each node. For a correct functioning, the `syntaxstring`
must be defined (including `kwargs...`) for every newly defined
`SyntaxToken` (e.g., `SyntaxLeaf`s, that is, `Atom`s and `Truth` values, and `Operator`s),
in a way that it produces a
*unique* string representation, since `Base.hash` and `Base.isequal`, at least for
`SyntaxBranch`s, rely on it.

In particular, for the case of `Atom`s, the function calls itself on the wrapped value:

    syntaxstring(a::Atom; kwargs...) = syntaxstring(value(a); kwargs...)

The `syntaxstring` for any value defaults to its `string` representation, but it can be
defined by defining the appropriate `syntaxstring` method.

!!! warning
    The `syntaxstring` for syntax tokens (e.g., atoms, operators) should not be
    prefixed/suffixed by whitespaces, as this may cause ambiguities upon *parsing*.
    For similar reasons, `syntaxstring`s should not contain parentheses (`'('`, `')'`),
    and, when parsing in function notation, commas (`','`).

See also [`SyntaxLeaf`](@ref), [`Operator`](@ref), [`parseformula`](@ref).
"""

doc_arity = """
    arity(tok::Connective)::Integer
    arity(φ::SyntaxLeaf)::Integer # TODO extend to SyntaxTree SyntaxBranch

Return the `arity` of a `Connective` or an `SyntaxLeaf`. The `arity` is an integer
representing the number of allowed children in a `SyntaxBranch`. `Connective`s with `arity`
equal to 0, 1 or 2 are called `nullary`, `unary` and `binary`, respectively.
`SyntaxLeaf`s (`Atom`s and `Truth` values) are always nullary.

See also [`SyntaxLeaf`](@ref), [`Connective`](@ref), [`SyntaxBranch`](@ref).
"""

doc_precedence = """
    precedence(c::Connective)

Return the precedence of a binary connective.

When using infix notation, and in the absence of parentheses,
`precedence` establishes how binary connectives are interpreted.
A precedence value is a standard integer, and
connectives with high precedence take precedence over connectives with lower precedences.
This affects how formulas are shown (via `syntaxstring`) and parsed (via `parseformula`).

By default,
the value for a `NamedConnective` is derived from the `Base.operator_precedence`
of its symbol (`name`); there are some exceptions (e.g., ¬).
Because of this, when dealing with a custom connective `⊙`,
it will be the case that `parseformula("p ⊙ q ∧ r") == (@synexpr p ⊙ q ∧ r)`.

It is possible to assign a specific precedence to a connective type `C` by providing a method
`Base.operator_precedence(::C)`.

# Examples
```jldoctest
julia> precedence(∧) == Base.operator_precedence(:∧)
true

julia> precedence(∧), precedence(∨), precedence(→)
∨(12, 11, 4)

julia> syntaxstring(parseformula("¬a ∧ b ∧ c"))
"¬a ∧ b ∧ c"

julia> syntaxstring(parseformula("¬a → b ∧ c"))
"(¬a) → (b ∧ c)"

julia> syntaxstring(parseformula("a ∧ b → c ∧ d"))
"(a ∧ b) → (c ∧ d)"
```

See also [`associativity`](@ref), [`Connective`](@ref).
"""

doc_iscommutative = """
    iscommutative(c::Connective)

Return whether a connective is known to be commutative.

# Examples
```jldoctest
julia> iscommutative(∧)
true

julia> iscommutative(→)
false
```

Note that nullary and unary connectives are considered commutative.

See also [`Connective`](@ref).

# Implementation

When implementing a new type for a *commutative* connective `C` with arity higher than 1,
please provide a method `iscommutative(::C)`. This can help model checking operations.

See also [`Connective`](@ref).
"""

doc_associativity = """
    associativity(::Connective)

Return whether a (binary) connective is right-associative.

When using infix notation, and in the absence of parentheses,
`associativity establishes how binary connectives of the same `precedence`
are interpreted. This affects how formulas are
shown (via `syntaxstring`) and parsed (via `parseformula`).

By default, the value for a `NamedConnective` is derived from the `Base.operator_precedence`
of its symbol (`name`); thus, for example, most connectives are left-associative
(e.g., `∧` and `∨`), while `→` is right-associative.
Because of this, when dealing with a custom connective `⊙`,
it will be the case that `parseformula("p ⊙ q ∧ r") == (@synexpr p ⊙ q ∧ r)`.

# Examples
```jldoctest
julia> associativity(∧)
:left

julia> associativity(→)
:right

julia> syntaxstring(parseformula("p → q → r"); remove_redundant_parentheses = false)
"p → (q → r)"

julia> syntaxstring(parseformula("p ∧ q ∨ r"); remove_redundant_parentheses = false)
"(p ∧ q) ∨ r"
```

See also [`Connective`](@ref), [`parseformula`](@ref), [`precedence`](@ref),
[`syntaxstring`](@ref).
"""

doc_composeformulas = """
    composeformulas(c::Connective, φs::NTuple{N,F})::F where {N,F<:Formula}

Return a new formula of type `F` by composing `N` formulas of the same type
via a connective `c`. This function allows one to use connectives for flexibly composing
formulas (see *Implementation* section).

# Examples
```jldoctest
julia> f = parseformula("◊(p→q)");

julia> p = Atom("p");

julia> ∧(f, p)  # Easy way to compose a formula
SyntaxBranch: ◊(p → q) ∧ p

julia> f ∧ ¬p   # Leverage infix notation ;) (see https://stackoverflow.com/a/60321302/5646732)
SyntaxBranch: ◊(p → q) ∧ ¬p

julia> ∧(f, p, ¬p) # Shortcut for ∧(f, ∧(p, ¬p))
SyntaxBranch: ◊(p → q) ∧ p ∧ ¬p
```

# Implementation

Upon `composeformulas` lies a flexible way of using connectives for composing
formulas and syntax tokens (e.g., atoms), given by methods like the following:

    function (c::Connective)(φs::NTuple{N,Formula}) where {N}
        ...
    end

These allow composing formulas as in `∧(f, ¬p)`, and in order to access this composition
with any newly defined subtype of `Formula`,
a new method for `composeformulas` should be defined, together with
promotion from/to other `Formula`s should be taken care of (see
[here](https://docs.julialang.org/en/v1/manual/conversion-and-promotion/)
and [here](https://github.com/JuliaLang/julia/blob/master/base/promotion.jl)).

Similarly,
for allowing a (possibly newly defined) connective to be applied on a number of
syntax tokens/formulas that differs from its arity,
for any newly defined connective `c`, new methods
similar to the two above should be defined.
For example, although ∧ and ∨ are binary, (i.e., have arity equal to 2),
compositions such as `∧(f, f2, f3, ...)` and `∨(f, f2, f3, ...)` can be done
thanks to the following two methods that were defined in SoleLogics:

    function ∧(
        c1::Formula,
        c2::Formula,
        c3::Formula,
        cs::Formula...
    )
        return ∧(c1, ∧(c2, c3, cs...))
    end
    function ∨(
        c1::Formula,
        c2::Formula,
        c3::Formula,
        cs::Formula...
    )
        return ∨(c1, ∨(c2, c3, cs...))
    end

!!! note
To allow for the composition of `Formula`s of different types,
promotion rules should be provided.

See also [`Formula`](@ref), [`Connective`](@ref).
"""

# TODO this piece of doc will be useful when we define simplify(φ), to explain
#  the differences between the two
# !!! info
#     Note that `composeformulas`
#     does not apply, simplifications/minimizations of any kind.
# `composeformulas(∧, ⊤,⊤)` returns a
# `SyntaxBranch` whose root value is ∧, instead of returning just ⊤.

doc_tokopprop = """
    tokens(φ::Formula)::AbstractVector{<:SyntaxToken}
    atoms(φ::Formula)::AbstractVector{<:Atom}
    truths(φ::Formula)::AbstractVector{<:Truth}
    leaves(φ::Formula)::AbstractVector{<:SyntaxLeaf}
    connectives(φ::Formula)::AbstractVector{<:Connective}
    operators(φ::Formula)::AbstractVector{<:Operator}
    ntokens(φ::Formula)::Integer
    natoms(φ::Formula)::Integer
    ntruths(φ::Formula)::Integer
    nleaves(φ::Formula)::Integer
    nconnectives(φ::Formula)::Integer
    noperators(φ::Formula)::Integer

Return the list/number of (non-unique) `SyntaxToken`s, `Atoms`s, etc...
appearing in a formula.

See also [`Formula`](@ref), [`SyntaxToken`](@ref).
"""

doc_syntaxtree_children = """
    children(φ::SyntaxTree)

Getter for `φ` children.

See also [`SyntaxBranch`](@ref), [`SyntaxTree`](@ref).
"""

doc_syntaxtree_token = """
    token(φ::SyntaxTree)::SyntaxToken

Getter for the token wrapped in a `SyntaxTree`.

See also [`SyntaxBranch`](@ref), [`SyntaxTree`](@ref).
"""

doc_formula_basein = """
    Base.in(tok::SyntaxToken, φ::Formula)::Bool

Return whether a syntax token appears in a formula.

See also [`Formula`](@ref), [`SyntaxToken`](@ref).
"""

doc_dual = """
    dual(op::SyntaxToken)

Return the `dual` of an `Operator`.
Given an operator `op` of arity `n`, the dual `dop` is such that, on a Boolean algebra,
`op(ch_1, ..., ch_n)` ≡ `¬dop(¬ch_1, ..., ¬ch_n)`.

Duality can be used to perform syntactic simplifications on formulas.
For example, since `∧` and `∨` are `dual`s, `¬(¬p ∧ ¬q)` can be simplified to `(p ∧ q)`.
Duality also applies to `Truth` values (`⊤`/`⊥`), with existential/universal
semantics (`◊`/`□`), and `Atom`s.

# Implementation

When providing a `dual` for an operator of type `O`, please also provide:

    hasdual(::O) = true

The dual of an `Atom` (that is, the atom with inverted semantics)
is defined as:

    dual(p::Atom{V}) where {V} = Atom(dual(value(p)))

As such, `hasdual(::V)` and `dual(::V)` should be defined when wrapping objects of type `A`.

See also [`normalize`](@ref), [`SyntaxToken`](@ref).
"""
