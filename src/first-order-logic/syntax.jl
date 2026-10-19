############################################################################################
#### Terms #################################################################################
############################################################################################

"""
    abstract type Term <: Syntactical end

A term, built from [`Variable`](@ref)s and [`Constant`](@ref)s by applying
[`FunctionSymbol`](@ref)s (see [`FunctionTerm`](@ref)).

Terms appear as the arguments of a [`PredicateAtom`](@ref).

See also [`Variable`](@ref), [`Constant`](@ref), [`FunctionTerm`](@ref),
[`variables`](@ref), [`evaluate`](@ref).
"""
abstract type Term <: Syntactical end

"""
    struct Variable <: Term
        name::Symbol
    end

A first-order variable, which can be bound by a quantifier ([`ForAll`](@ref),
[`Exists`](@ref)) or assigned a value through an assignment when interpreting a formula.

# Examples
```julia-repl
julia> x = Variable(:x)
Variable: x

julia> syntaxstring(x)
"x"
```

See also [`@variables`](@ref), [`Term`](@ref), [`freevariables`](@ref).
"""
struct Variable <: Term
    name::Symbol
end
Variable(name::AbstractString) = Variable(Symbol(name))

"""
    struct Constant <: Term
        name::Symbol
    end

A first-order constant symbol; its denotation is fixed by a [`FOStructure`](@ref).

See also [`@constants`](@ref), [`Term`](@ref), [`FOStructure`](@ref).
"""
struct Constant <: Term
    name::Symbol
end
Constant(name::AbstractString) = Constant(Symbol(name))

name(t::Union{Variable, Constant}) = t.name

syntaxstring(t::Union{Variable, Constant}; kwargs...) = string(name(t))

"""
    struct FunctionSymbol
        name::Symbol
        arity::Int
    end

A function symbol with a fixed `arity`.
A function symbol is callable on terms, and returns a [`FunctionTerm`](@ref).

# Examples
```julia-repl
julia> succ = FunctionSymbol(:succ, 1); x = Variable(:x);

julia> syntaxstring(succ(succ(x)))
"succ(succ(x))"
```

See also [`FunctionTerm`](@ref), [`FOStructure`](@ref).
"""
struct FunctionSymbol
    name::Symbol
    arity::Int

    function FunctionSymbol(name, arity::Integer)
        arity >= 0 || throw(ArgumentError("Function symbol arity must be non-negative."))
        new(Symbol(name), arity)
    end
end

name(f::FunctionSymbol) = f.name
arity(f::FunctionSymbol) = f.arity
syntaxstring(f::FunctionSymbol; kwargs...) = string(name(f))

"""
    struct FunctionTerm{N} <: Term
        f::FunctionSymbol
        args::NTuple{N,Term}
    end

The term obtained by applying a [`FunctionSymbol`](@ref) of arity `N` to `N` terms.
It is usually built by calling the function symbol itself, e.g., `f(x, c)`.

See also [`FunctionSymbol`](@ref), [`functionsymbol`](@ref), [`arguments`](@ref).
"""
struct FunctionTerm{N} <: Term
    f::FunctionSymbol
    args::NTuple{N, Term}

    function FunctionTerm(f::FunctionSymbol, args::NTuple{N, Term}) where {N}
        if arity(f) != N
            throw(ArgumentError("Cannot apply function symbol $(name(f)) of arity " *
                                "$(arity(f)) to $(N) arguments."))
        end
        new{N}(f, args)
    end
end
FunctionTerm(f::FunctionSymbol, args::Term...) = FunctionTerm(f, args)

(f::FunctionSymbol)(args::Term...) = FunctionTerm(f, args)

"""
    functionsymbol(t::FunctionTerm)::FunctionSymbol

Return the function symbol at the root of a [`FunctionTerm`](@ref).
"""
functionsymbol(t::FunctionTerm) = t.f

"""
    arguments(t::FunctionTerm)
    arguments(a::PredicateAtom)

Return the tuple of argument terms of a function term or of an atomic formula.
"""
arguments(t::FunctionTerm) = t.args

function syntaxstring(t::FunctionTerm; kwargs...)
    isempty(arguments(t)) && return syntaxstring(functionsymbol(t))
    return syntaxstring(functionsymbol(t)) * "(" *
           join(map(a -> syntaxstring(a; kwargs...), arguments(t)), ", ") * ")"
end

############################################################################################
#### Atomic formulas #######################################################################
############################################################################################

"""
    struct Predicate
        name::Symbol
        arity::Int
    end

A predicate (relation) symbol with a fixed `arity`.
A predicate is callable on terms, and returns a [`PredicateAtom`](@ref), that is, a
first-order atomic formula.

# Examples
```julia-repl
julia> Less = Predicate(:Less, 2); x, y = Variable(:x), Variable(:y);

julia> syntaxstring(Less(x, y))
"Less(x, y)"
```

See also [`PredicateAtom`](@ref), [`EQUALITY`](@ref), [`FOStructure`](@ref).
"""
struct Predicate
    name::Symbol
    arity::Int

    function Predicate(name, arity::Integer)
        arity >= 0 || throw(ArgumentError("Predicate arity must be non-negative."))
        new(Symbol(name), arity)
    end
end

name(p::Predicate) = p.name
arity(p::Predicate) = p.arity
syntaxstring(p::Predicate; kwargs...) = string(name(p))

"""
    struct PredicateAtom{N} <: AbstractAtom
        predicate::Predicate
        args::NTuple{N,Term}
    end

A first-order atomic formula `P(t1, ..., tN)`.
Since it is an `AbstractAtom`, it can be composed with every SoleLogics connective, and
utilities such as [`atoms`](@ref), [`height`](@ref) or [`subformulas`](@ref) work
unchanged on first-order formulas.

See also [`Predicate`](@ref), [`predicate`](@ref), [`arguments`](@ref), [`Term`](@ref).
"""
struct PredicateAtom{N} <: AbstractAtom
    predicate::Predicate
    args::NTuple{N, Term}

    function PredicateAtom(p::Predicate, args::NTuple{N, Term}) where {N}
        if arity(p) != N
            throw(ArgumentError("Cannot apply predicate $(name(p)) of arity " *
                                "$(arity(p)) to $(N) arguments."))
        end
        new{N}(p, args)
    end
end
PredicateAtom(p::Predicate, args::Term...) = PredicateAtom(p, args)

(p::Predicate)(args::Term...) = PredicateAtom(p, args)

"""
    predicate(a::PredicateAtom)::Predicate

Return the predicate symbol of a first-order atomic formula.
"""
predicate(a::PredicateAtom) = a.predicate
arguments(a::PredicateAtom) = a.args

function Base.:(==)(a::PredicateAtom, b::PredicateAtom)
    predicate(a) == predicate(b) && arguments(a) == arguments(b)
end
Base.hash(a::PredicateAtom, h::UInt) = hash(arguments(a), hash(predicate(a), h))
Base.hash(a::PredicateAtom) = hash(a, zero(UInt))

"""
    const EQUALITY = Predicate(:(=), 2)
    t1 ≐ t2

The binary equality predicate.
Unless a [`FOStructure`](@ref) explicitly interprets it, it is interpreted as `isequal`
on domain elements.
The infix helper `≐` can be typed by `\\doteq<tab>`.
"""
const EQUALITY = Predicate(:(=), 2)

"""See [`EQUALITY`](@ref)."""
≐(t1::Term, t2::Term) = EQUALITY(t1, t2)

function syntaxstring(a::PredicateAtom; parenthesize_atoms = false, kwargs...)
    args = map(t -> syntaxstring(t; kwargs...), arguments(a))
    s = begin
        if predicate(a) == EQUALITY
            "$(args[1]) = $(args[2])"
        elseif isempty(args)
            syntaxstring(predicate(a))
        else
            syntaxstring(predicate(a)) * "(" * join(args, ", ") * ")"
        end
    end
    return parenthesize_atoms ? "($s)" : s
end

############################################################################################
#### Quantifiers ###########################################################################
############################################################################################

"""
    abstract type AbstractQuantifier <: Connective end

A first-order quantifier binding a [`Variable`](@ref).
A quantifier is a unary `Connective`, hence a quantified formula is a plain
`SyntaxBranch` whose token is the quantifier.

# Interface
- `variable(q)::Variable`
- `rebind(q, v::Variable)`: the same quantifier, binding `v` instead (defaults to
  `typeof(q)(v)`; used by capture-avoiding [`substitute`](@ref))

See also [`ForAll`](@ref), [`Exists`](@ref), [`forall`](@ref), [`exists`](@ref).
"""
abstract type AbstractQuantifier <: Connective end

"""
    variable(q::AbstractQuantifier)::Variable

Return the variable bound by a quantifier.
"""
variable(q::AbstractQuantifier) = q.var

rebind(q::AbstractQuantifier, v::Variable) = typeof(q)(v)

arity(::AbstractQuantifier) = 1
precedence(::AbstractQuantifier) = precedence(NEGATION)
associativity(::AbstractQuantifier) = associativity(NEGATION)

"""
    isquantifier(x)::Bool

Return whether `x` is a first-order quantifier.
"""
isquantifier(x) = x isa AbstractQuantifier

"""
    struct ForAll <: AbstractQuantifier
        var::Variable
    end

The universal quantifier `∀x`.
`ForAll(x)(φ)` (or [`forall`](@ref)`(x, φ)`, or `∀(x, φ)`) builds the formula `∀x φ`.

See also [`Exists`](@ref), [`AbstractQuantifier`](@ref).
"""
struct ForAll <: AbstractQuantifier
    var::Variable
end

"""
    struct Exists <: AbstractQuantifier
        var::Variable
    end

The existential quantifier `∃x`.
`Exists(x)(φ)` (or [`exists`](@ref)`(x, φ)`, or `∃(x, φ)`) builds the formula `∃x φ`.

See also [`ForAll`](@ref), [`AbstractQuantifier`](@ref).
"""
struct Exists <: AbstractQuantifier
    var::Variable
end

hasdual(::ForAll) = true
hasdual(::Exists) = true
dual(q::ForAll) = Exists(variable(q))
dual(q::Exists) = ForAll(variable(q))

# The trailing space separates the quantifier prefix from its scope, so that formulas
# print as "∀x P(x)", "∀x ∃y R(x, y)" and "∀x (P(x) → Q(x))".
syntaxstring(q::ForAll; kwargs...) = "∀" * syntaxstring(variable(q)) * " "
syntaxstring(q::Exists; kwargs...) = "∃" * syntaxstring(variable(q)) * " "

"""
    forall(x::Variable, [y::Variable, ...,] φ::Formula)
    ∀(x, [y, ...,] φ)

Build the universally quantified formula `∀x ∀y ... φ`.

# Examples
```julia-repl
julia> @variables x y; P = Predicate(:P, 2);

julia> syntaxstring(∀(x, y, P(x, y)))
"∀x ∀y P(x, y)"
```

See also [`exists`](@ref), [`ForAll`](@ref).
"""
forall(x::Variable, φ::Formula) = ForAll(x)(φ)
forall(x::Variable, y::Variable, rest...) = forall(x, forall(y, rest...))

"""
    exists(x::Variable, [y::Variable, ...,] φ::Formula)
    ∃(x, [y, ...,] φ)

Build the existentially quantified formula `∃x ∃y ... φ`.

See also [`forall`](@ref), [`Exists`](@ref).
"""
exists(x::Variable, φ::Formula) = Exists(x)(φ)
exists(x::Variable, y::Variable, rest...) = exists(x, exists(y, rest...))

"""See [`forall`](@ref)."""
const ∀ = forall
"""See [`exists`](@ref)."""
const ∃ = exists

############################################################################################
#### Syntactic utilities ###################################################################
############################################################################################

"""
    variables(t::Term)::Set{Variable}
    variables(φ::Formula)::Set{Variable}

Return all the variables occurring in a term or in a formula, both free and bound
(including the variables appearing next to a quantifier).

See also [`freevariables`](@ref), [`boundvariables`](@ref).
"""
variables(t::Variable) = Set{Variable}([t])
variables(::Constant) = Set{Variable}()
variables(t::FunctionTerm) = _unionvars(arguments(t))
variables(a::PredicateAtom) = _unionvars(arguments(a))
variables(φ::Formula) = _variables(tree(φ))

_unionvars(ts) = reduce(union!, map(variables, ts); init = Set{Variable}())

_variables(φ::PredicateAtom) = variables(φ)
_variables(::SyntaxLeaf) = Set{Variable}()
function _variables(φ::AbstractSyntaxBranch)
    vs = reduce(union!, map(_variables, children(φ)); init = Set{Variable}())
    isquantifier(token(φ)) && push!(vs, variable(token(φ)))
    return vs
end

"""
    freevariables(φ::Formula)::Set{Variable}

Return the variables having at least one free (i.e., not bound) occurrence in `φ`.

# Examples
```julia-repl
julia> @variables x y; P = Predicate(:P, 2);

julia> freevariables(∃(x, P(x, y)))
Set{Variable} with 1 element:
  Variable: y
```

See also [`boundvariables`](@ref), [`issentence`](@ref).
"""
freevariables(φ::Formula) = _freevariables(tree(φ))

_freevariables(φ::PredicateAtom) = variables(φ)
_freevariables(::SyntaxLeaf) = Set{Variable}()
function _freevariables(φ::AbstractSyntaxBranch)
    vs = reduce(union!, map(_freevariables, children(φ)); init = Set{Variable}())
    isquantifier(token(φ)) && delete!(vs, variable(token(φ)))
    return vs
end

"""
    boundvariables(φ::Formula)::Set{Variable}

Return the variables bound by some quantifier in `φ`.

See also [`freevariables`](@ref), [`variables`](@ref).
"""
boundvariables(φ::Formula) = _boundvariables(tree(φ))

_boundvariables(::SyntaxLeaf) = Set{Variable}()
function _boundvariables(φ::AbstractSyntaxBranch)
    vs = reduce(union!, map(_boundvariables, children(φ)); init = Set{Variable}())
    isquantifier(token(φ)) && push!(vs, variable(token(φ)))
    return vs
end

"""
    issentence(φ::Formula)::Bool

Return whether `φ` is a sentence (or closed formula), that is, it has no free variables.
The truth of a sentence on a [`FOStructure`](@ref) does not depend on any assignment.

See also [`freevariables`](@ref).
"""
issentence(φ::Formula) = isempty(freevariables(φ))

# Note: SoleLogics.atoms collects into a Vector{Atom}, hence it cannot hold PredicateAtoms
function _predicateatoms(φ::Formula)
    PredicateAtom[a for a in appendatoms!(AbstractAtom[], tree(φ)) if a isa PredicateAtom]
end

"""
    predicates(φ::Formula)::Vector{Predicate}

Return the (unique) predicate symbols occurring in `φ`.
"""
predicates(φ::Formula) = unique(map(predicate, _predicateatoms(φ)))

_terms!(acc, t::Union{Variable, Constant}) = push!(acc, t)
function _terms!(acc, t::FunctionTerm)
    push!(acc, t)
    foreach(a -> _terms!(acc, a), arguments(t))
    return acc
end
function _subterms(φ::Formula)
    acc = Term[]
    foreach(a -> foreach(t -> _terms!(acc, t), arguments(a)), _predicateatoms(φ))
    return acc
end

"""
    constants(φ::Formula)::Vector{Constant}

Return the (unique) constant symbols occurring in `φ`.
"""
constants(φ::Formula) = unique(Constant[t for t in _subterms(φ) if t isa Constant])

"""
    functionsymbols(φ::Formula)::Vector{FunctionSymbol}

Return the (unique) function symbols occurring in `φ`.
"""
functionsymbols(φ::Formula) = unique(FunctionSymbol[functionsymbol(t)
                                                    for t in _subterms(φ)
                                                    if t isa FunctionTerm])

"""
    substitute(t::Term, x::Variable, s::Term)::Term
    substitute(φ::Formula, x => s, [y => r, ...])::SyntaxTree

Replace the free occurrences of the variable `x` with the term `s`.

On formulas, substitution is capture-avoiding: when a quantifier binding `y` would
capture a variable of `s`, the bound variable is renamed to a fresh one first.
Several pairs are applied sequentially, from left to right.

# Examples
```julia-repl
julia> @variables x y; P = Predicate(:P, 2);

julia> syntaxstring(substitute(∃(y, P(x, y)), x => y))
"∃y1 P(y, y1)"
```

See also [`freevariables`](@ref).
"""
substitute(t::Variable, x::Variable, s::Term) = t == x ? s : t
substitute(t::Constant, ::Variable, ::Term) = t
function substitute(t::FunctionTerm, x::Variable, s::Term)
    FunctionTerm(functionsymbol(t), map(a -> substitute(a, x, s), arguments(t)))
end

function substitute(φ::Formula, ps::Pair{<:Variable, <:Term}...)
    return foldl((ψ, p) -> _substitute(ψ, p.first, p.second), ps; init = tree(φ))
end

function _substitute(a::PredicateAtom, x::Variable, s::Term)
    PredicateAtom(predicate(a), map(t -> substitute(t, x, s), arguments(a)))
end
_substitute(l::SyntaxLeaf, ::Variable, ::Term) = l
function _substitute(φ::AbstractSyntaxBranch, x::Variable, s::Term)
    tok = token(φ)
    if isquantifier(tok)
        y = variable(tok)
        body = children(φ)[1]
        # x is not free below this quantifier: nothing to replace
        (y == x || !(x in freevariables(body))) && return φ
        if y in variables(s)
            # Rename the bound variable to avoid capturing a variable of s
            ynew = _freshvariable(y, union(variables(s), variables(body), [x]))
            body = _substitute(body, y, ynew)
            tok = rebind(tok, ynew)
        end
        return SyntaxBranch(tok, (_substitute(body, x, s),))
    else
        return SyntaxBranch(tok, map(ch -> _substitute(ch, x, s), children(φ)))
    end
end

function _freshvariable(v::Variable, used)
    i = 1
    while Variable(Symbol(name(v), i)) in used
        i += 1
    end
    return Variable(Symbol(name(v), i))
end

############################################################################################
#### Macros ################################################################################
############################################################################################

"""
    @variables(xs...)

Instantiate a collection of [`Variable`](@ref)s, named after the given identifiers, and
return them as a vector.

!!! info
    Like `@atoms`, variables instantiated with this macro are defined in the global scope
    as constants.

# Examples
```julia-repl
julia> @variables x y
2-element Vector{Variable}:
 Variable: x
 Variable: y
```
"""
macro variables(xs...)
    quote
        $(map(x -> quote
                if !(@isdefined $x)
                    const $x = $(Variable(x))
                end
            end, xs,)...)
        [$(xs...)]
    end |> esc
end

"""
    @constants(cs...)

Instantiate a collection of [`Constant`](@ref)s, named after the given identifiers, and
return them as a vector.
Constants are defined in the global scope as Julia constants, as in [`@variables`](@ref).
"""
macro constants(cs...)
    quote
        $(map(c -> quote
                if !(@isdefined $c)
                    const $c = $(Constant(c))
                end
            end, cs,)...)
        [$(cs...)]
    end |> esc
end
