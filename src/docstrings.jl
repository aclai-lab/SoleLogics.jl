# This file collects some of the SoleLogics methods docstrings.
# The only purpose of this file is to make code easier to scroll and read.

# If you are a user, please, consider using the "?" section in the Julia REPL to read
# docstrings.

# @giopaglia by @mauro-milella: is this a good idea? Maybe this should live in a proper
# directory inside ../docs ? I think this monolithic file is useful, look at how smooth
# is core.jl now. Anyway, I would use this strange "include-the-docstrings" pattern only
# for huge files such as core.jl (maybe, ONLY for core.jl). What do you think?

doc_syntaxstring = """
    syntaxstring(φ::Syntactical; kwargs...)::String
    syntaxstring(φ::Formula; kwargs...)::String
    syntaxstring(tok::SyntaxToken; kwargs...)::String
    syntaxstring(a::Atom; kwargs...)::String
    syntaxstring(φ::SyntaxBranch; function_notation=false,
        remove_redundant_parentheses=true, kwargs...)::String
    syntaxstring(φ::Any; kwargs...)::String


Produce the string representation of a `Formula` or a `SyntaxToken` by performing
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
   it forces the atoms (which are the leaves of a formula's tree structure) to be
   wrapped in parentheses.

# Examples
```jldoctest
julia> syntaxstring(parsetree("p∧q∧r∧s∧t"))
"p ∧ q ∧ r ∧ s ∧ t"

julia> syntaxstring(parsetree("p∧q∧r∧s∧t"), function_notation=true)
"∧(∧(∧(∧(p, q), r), s), t)"

julia> syntaxstring(parsetree("p∧q∧r∧s∧t"), remove_redundant_parentheses=false)
"((((p) ∧ (q)) ∧ (r)) ∧ (s)) ∧ (t)""

julia> syntaxstring(parsetree("p∧q∧r∧s∧t"), remove_redundant_parentheses=true, parenthesize_atoms=true)
"(p) ∧ (q) ∧ (r) ∧ (s) ∧ (t)"

julia> syntaxstring(parsetree("◊((p∧s)→q)"))
"◊((p ∧ s) → q)"

julia> syntaxstring(parsetree("◊((p∧s)→q)"); function_notation = true)
"◊(→(∧(p, s), q))"
```

See also [`parsetree`](@ref), [`parsetree`](@ref),
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

    syntaxstring(p::Atom; kwargs...) = syntaxstring(value(p); kwargs...)

The `syntaxstring` for any value defaults to its `string` representation, but it can be
defined by defining the appropriate `syntaxstring` method.

!!! warning
    The `syntaxstring` for syntax tokens (e.g., atoms, operators) should not be
    prefixed/suffixed by whitespaces, as this may cause ambiguities upon *parsing*.
    For similar reasons, `syntaxstring`s should not contain parentheses (`'('`, `')'`),
    and, when parsing in function notation, commas (`','`).

See also [`SyntaxLeaf`](@ref), [`Operator`](@ref), [`parsetree`](@ref).
"""

doc_arity = """
    arity(tok::Connective)::Integer
    arity(l::SyntaxLeaf)::Integer

Return the `arity` of a `Connective` or an `SyntaxLeaf`. The `arity` is an integer
representing the number of allowed children in a `SyntaxBranch`. `Connective`s with `arity`
equal to 0, 1 or 2 are called `nullary`, `unary` and `binary`, respectively.
`SyntaxLeaf`s (`Atom`s and `Truth` values) are always nullary.

See also [`SyntaxLeaf`](@ref), [`Connective`](@ref), [`SyntaxBranch`](@ref).
"""

doc_precedence = """
    precedence(c::Connective)

Return the precedence of a (binary) connective.

When using infix notation, and in the absence of parentheses,
`precedence` establishes how binary connectives are interpreted.
A precedence value is a standard integer, and
connectives with high precedence take precedence over connectives with lower precedences.
This affects how formulas are shown (via `syntaxstring`) and parsed (via `parsetree`).

By default, the value for a `NamedConnective` is derived from the `Base.operator_precedence`
of its symbol (`name`).
Because of this, when dealing with a custom connective `⊙`,
it will be the case that `parsetree("p ⊙ q ∧ r") == (@synexpr p ⊙ q ∧ r)`.

It is possible to assign a specific precedence to a connective type `C` by providing a method
`Base.operator_precedence(::C)`.

# Examples
```jldoctest
julia> precedence(∧) == Base.operator_precedence(:∧)
true

julia> precedence(¬) == Base.operator_precedence(:¬)
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
shown (via `syntaxstring`) and parsed (via `parsetree`).

By default, the value for a `NamedConnective` is derived from the `Base.operator_precedence`
of its symbol (`name`); thus, for example, most connectives are left-associative
(e.g., `∧` and `∨`), while `→` is right-associative.
Because of this, when dealing with a custom connective `⊙`,
it will be the case that `parsetree("p ⊙ q ∧ r") == (@synexpr p ⊙ q ∧ r)`.

# Examples
```jldoctest
julia> associativity(∧)
:left

julia> associativity(→)
:right

julia> syntaxstring(parsetree("p → q → r"); remove_redundant_parentheses = false)
"p → (q → r)"

julia> syntaxstring(parsetree("p ∧ q ∨ r"); remove_redundant_parentheses = false)
"(p ∧ q) ∨ r"
```

See also [`Connective`](@ref), [`parsetree`](@ref), [`precedence`](@ref),
[`syntaxstring`](@ref).
"""

doc_joinformulas = """
    joinformulas(c::Connective, φs::NTuple{N,F})::F where {N,F<:Formula}

Return a new formula of type `F` by composing `N` formulas of the same type
via a connective `c`. This function allows one to use connectives for flexibly composing
formulas (see *Implementation* section).

# Examples
```jldoctest
julia> f = parseformula("◊(p→q)");

julia> p = Atom("p");

julia> ∧(f, p)  # Easy way to compose a formula
SyntaxBranch: ◊(p → q) ∧ p

julia> f ∧ ¬p   # Leverage infix notation ;) See https://stackoverflow.com/a/60321302/5646732
SyntaxBranch: ◊(p → q) ∧ ¬p

julia> ∧(f, p, ¬p) # Shortcut for ∧(f, ∧(p, ¬p))
SyntaxBranch: ◊(p → q) ∧ p ∧ ¬p
```

# Implementation

Upon `joinformulas` lies a flexible way of using connectives for composing
formulas and syntax tokens (e.g., atoms), given by methods like the following:

    function (c::Connective)(children::NTuple{N,Formula}) where {N}
        ...
    end

These allow composing formulas as in `∧(f, ¬p)`, and in order to access this composition
with any newly defined subtype of `Formula`,
a new method for `joinformulas` should be defined, together with
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
        c1::Union{SyntaxToken,Formula},
        c2::Union{SyntaxToken,Formula},
        c3::Union{SyntaxToken,Formula},
        cs::Union{SyntaxToken,Formula}...
    )
        return ∧(c1, ∧(c2, c3, cs...))
    end
    function ∨(
        c1::Union{SyntaxToken,Formula},
        c2::Union{SyntaxToken,Formula},
        c3::Union{SyntaxToken,Formula},
        cs::Union{SyntaxToken,Formula}...
    )
        return ∨(c1, ∨(c2, c3, cs...))
    end

!!! info
    The idea behind `joinformulas` is to concatenate syntax tokens without applying
    simplifications/minimizations of any kind. Because of that, ∧(⊤,⊤) returns a
    `SyntaxBranch` whose root value is ∧, instead of returning just a Truth value ⊤.

See also [`Formula`](@ref), [`Connective`](@ref).
"""

doc_tokopprop = """
    tokens(f::Formula)::AbstractVector{<:SyntaxToken}
    operators(f::Formula)::AbstractVector{<:Operator}
    connectives(f::Formula)::AbstractVector{<:Connective}
    leaves(f::Formula)::AbstractVector{<:AbstractLeaf}
    atoms(f::Formula)::AbstractVector{<:Atom}
    truths(f::Formula)::AbstractVector{<:Truth}
    ntokens(f::Formula)::Integer
    noperators(f::Formula)::Integer
    nconnectives(f::Formula)::Integer
    nleaves(f::Formula)::Integer
    natoms(f::Formula)::Integer
    ntruths(f::Formula)::Integer

Return the list or the number of (unique) `SyntaxToken`s appearing in a formula.

See also [`Formula`](@ref), [`SyntaxToken`](@ref).
"""

doc_syntaxtree_tokens = """
    tokens(t::SyntaxTree)::AbstractVector{<:SyntaxToken}
TODO
"""

doc_syntaxtree_operators = """
operators(t::SyntaxTree)::AbstractVector{Operator}

List all operators appearing in a syntax tree.

See also [`atoms`](@ref), [`noperators`](@ref), [`Operator`](@ref), [`tokens`](@ref).
"""

doc_syntaxtree_connectives = """
    connectives(t::SyntaxTree)::AbstractVector{Connective}

List all connectives appearing in a syntax tree.

See also [`atoms`](@ref), [`Connective`](@ref), [`nconnectives`](@ref).
"""

doc_syntaxtree_leaves = """
    leaves(t::SyntaxTree)::AbstractVector{Operator}

List all leaves appearing in a syntax tree.

See also  [`atoms`](@ref), [`nleaves`](@ref), [`SyntaxLeaf`](@ref),.
"""

doc_syntaxtree_atoms = """
    atoms(t::SyntaxTree)::AbstractVector{Atom}

List all `Atom`s appearing in a syntax tree.

See also [`Atom`](@ref), [`natoms`](@ref).
"""

doc_syntaxtree_truths = """
    truths(t::SyntaxTree)::AbstractVector{Truth}

List all `Truth`s appearing in a syntax tree.

See also [`Truth`](@ref), [`ntruths`](@ref).
"""

doc_syntaxbranch_children = """
    children(t::SyntaxBranch)

Getter for `t` children.

See also [`SyntaxBranch`](@ref).
"""

doc_syntaxbranch_token = """
    token(t::SyntaxBranch)::SyntaxToken

Getter for the token wrapped in a `SyntaxBranch`.

See also [`SyntaxBranch`](@ref).
"""

doc_syntaxbranch_tokens = """
    tokens(t::SyntaxBranch)

List all tokens appearing in a syntax tree.

See also [`atoms`](@ref), [`ntokens`](@ref), [`operators`](@ref), [`SyntaxToken`](@ref).
"""

doc_syntaxbranch_operators = """
    operators(t::SyntaxBranch)

TODO
"""

doc_syntaxbranch_connectives = """
    connectives(t::SyntaxBranch)

TODO
"""

doc_syntaxbranch_tokenstype = """
    tokenstype(t::SyntaxBranch)

Return all the different `SyntaxToken` types contained in the `SyntaxTree` rooted at `t`.

See also [`Operator`](@ref), [`SyntaxTree`](@ref), [`SyntaxTree`](@ref).
"""

doc_syntaxbranch_operatorstype = """
    operatorstype(t::SyntaxBranch)

Return all the different `SyntaxToken` types contained in the `SyntaxTree` rooted at `t`.

See also [`SyntaxToken`](@ref), [`SyntaxTree`](@ref), [`SyntaxTree`](@ref).
"""

doc_syntaxbranch_ntokens = """
    ntokens(t::SyntaxBranch)::Integer

Return the count of all `SyntaxToken`s appearing in the `SyntaxTree` rooted at `t`.

See also [`SyntaxBranch`](@ref), [`SyntaxToken`](@ref), [`SyntaxTree`](@ref), [`tokens`](@ref).
"""

doc_syntaxbranch_noperators = """
    noperators(t::SyntaxTree)::Integer

Return the count of all `Operator`s appearing in the [`SyntaxTree`] rooted at `t`.

See also [`operators`](@ref), [`SyntaxToken`](@ref).
"""

doc_syntaxbranch_nconnectives = """
    nconnectives(t::SyntaxTree)::Integer

Return the count of all `Connective`s appearing in the [`SyntaxTree`] rooted at `t`.

See also [`connectives`](@ref), [`SyntaxToken`](@ref).
"""

doc_syntaxbranch_nleaves = """
    nleaves(t::SyntaxBranch)::Integer

Return the count of all `SyntaxLeaf`s appearing in `t`.

See also [`SyntaxBranch`](@ref), [`SyntaxLeafs`](@ref).
"""

doc_syntaxbranch_ntruths = """
    ntruths(t::SyntaxTree)::Integer

Return the count of all `Truth`s appearing in the [`SyntaxTree`] rooted at `t`.

See also [`truths`](@ref), [`SyntaxToken`](@ref).
"""

doc_syntaxbranch_atomstype = """
    atomstype(t::SyntaxBranch)

Return all the different `Atom` types contained in the `SyntaxTree` rooted at `t`.

See also [`Atom`](@ref), [`SyntaxBranch`](@ref), [`SyntaxTree`](@ref), [`SyntaxTree`](@ref).
"""

doc_syntaxbranch_natoms = """
    natoms(t::SyntaxTree)::Integer

Return the count of all `Atom`s appearing in the [`SyntaxTree`] rooted at `t`.

See also [`atoms`](@ref), [`SyntaxToken`](@ref).
"""

doc_syntaxbranch_height = """
    height(t::SyntaxBranch)::Integer

Return the height of the `SyntaxBranch` `t`.

See also [`SyntaxBranch`](@ref).
"""

doc_formula_basein = """
    Base.in(tok::SyntaxToken, φ::Formula)::Bool
    Base.in(tok::SyntaxToken, tree::SyntaxBranch)::Bool

Return whether a syntax token appears in a formula.

See also [`Formula`](@ref), [`SyntaxToken`](@ref).
"""

doc_dual = """
    dual(op::SyntaxToken)

Return the `dual` of an `Operator`.
Given an operator `op` of arity `n`, the dual `dop` is such that, on a boolean algebra,
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

dual(p::Atom{A}) where {A} = Atom(dual(value(p)))

As such, `hasdual(::A)` and `dual(::A)` should be defined when wrapping objects of type `A`.

See also [`normalize`](@ref), [`SyntaxToken`](@ref).
"""
