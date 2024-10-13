#=
    Syntactical Type Hierarchy

    Syntactical
    ├── Formula
    │   ├── SyntaxStructure
    │   │   ├── SyntaxTree
    │   │   │   ├── SyntaxLeaf
    │   │   │   │   ├── AbstractAtom
    │   │   │   │   │   └── Atom (e.g., p)
    │   │   │   │   └── Truth
    │   │   │   │       ├── BooleanTruth (⊤ and ⊥)
    │   │   │   │       └── ...
    │   │   │   └── AbstractSyntaxBranch
    │   │   │   │   └── SyntaxBranch (e.g., p ∧ q)
    │   │   ├── LinearForm
    │   │   │   └── LeftmostLinearForm (e.g., conjunctions, disjunctions, DNFs, CNFs)
    │   │   ├── Literal (e.g., p, ¬p)
    │   │   └── ...
    │   ├── TruthTable
    │   ├── AnchoredFormula
    │   └── ...
    └── Connective
        ├── NamedConnective (e.g., ∧, ∨, →, ¬, □, ◊)
        ├── AbstractRelationalConnective
        │   ├── DiamondRelationalConnective (e.g., ⟨G⟩)
        │   ├── BoxRelationalConnective (e.g., [G])
        │   └── ...
        └── ...
=#

############################################################################################
#### Atom ##################################################################################
############################################################################################

"""
    struct Atom{V} <: AbstractAtom
        value::V
    end

Simplest atom implementation, wrapping a `value`.

See also [`AbstractAtom`](@ref), [`value`](@ref), [`check`](@ref),
[`SyntaxToken`](@ref).
"""
struct Atom{V} <: AbstractAtom
    value::V

    function Atom{V}(value::V) where {V}
        @assert !(value isa Union{Formula, Connective}) "Illegal nesting. "*
        "Cannot instantiate Atom with value of type $(typeof(value))"
        new{V}(value)
    end
    function Atom(value::V) where {V}
        Atom{V}(value)
    end
    function Atom{V}(p::Atom) where {V}
        Atom{V}(value(p))
    end
    function Atom(p::Atom)
        p
    end
end

valuetype(::Atom{V}) where {V} = V
valuetype(::Type{Atom{V}}) where {V} = V

value(p::Atom) = p.value

dual(p::Atom) = Atom(dual(value(p)))
hasdual(p::Atom) = hasdual(value(p))
hasdual(value) = false
dual(value) = error("Please, provide method SoleLogics.dual(::$(typeof(value))).") # TODO explain why?

Base.convert(::Type{A}, p::Atom) where {A <: Atom} = A(p)
Base.convert(::Type{A}, a) where {A <: Atom} = A(a)

Base.isequal(a::Atom, b::Atom) = Base.isequal(value(a), value(b)) # Needed to avoid infinite recursion
Base.isequal(a::Atom, b) = Base.isequal(value(a), b)
Base.isequal(a, b::Atom) = Base.isequal(a, value(b))
Base.isequal(a::Atom, b::SyntaxTree) = (a == b) # Needed for resolving ambiguities
Base.isequal(a::SyntaxTree, b::Atom) = (a == b) # Needed for resolving ambiguities
Base.hash(a::Atom) = Base.hash(value(a))

syntaxstring(a::Atom; kwargs...)::String = syntaxstring(value(a); kwargs...)

syntaxstring(value; kwargs...) = string(value)

############################################################################################
#### SyntaxBranch ##########################################################################
############################################################################################

"""
    struct SyntaxBranch <: AbstractSyntaxBranch
        token::Connective
        children::NTuple{N,SyntaxTree} where {N}
    end

Simple implementation of a syntax branch. The
implementation is *arity-compliant*, in that, upon construction,
the arity of the token is checked against the number of children provided.

# Examples
```julia-repl
julia> p,q = Atom.([p, q])
2-element Vector{Atom{String}}:
 Atom{String}: p
 Atom{String}: q

julia> branch = SyntaxBranch(CONJUNCTION, p, q)
SyntaxBranch: p ∧ q

julia> token(branch)
∧

julia> syntaxstring.(children(branch))
(p, q)

julia> ntokens(a) == nconnectives(a) + nleaves(a)
true

julia> arity(a)
2

julia> height(a)
1
```

See also
[`token`](@ref), [`children`](@ref),
[`arity`](@ref),
[`Connective`](@ref),
[`height`](@ref),
[`atoms`](@ref), [`natoms`](@ref),
[`operators`](@ref), [`noperators`](@ref),
[`tokens`](@ref), [`ntokens`](@ref),
"""
struct SyntaxBranch <: AbstractSyntaxBranch

    # The syntax token at the current node
    token::Connective

    # The child nodes of the current node
    children::NTuple{N, SyntaxTree} where {N}

    function _aritycheck(N, token, children)
        @assert arity(token)==N "Cannot instantiate SyntaxBranch with token "*
        "$(token) of arity $(arity(token)) and $(N) children."
        return nothing
    end

    function SyntaxBranch(
            token::Connective,
            children::NTuple{N, SyntaxTree} = (),
    ) where {N}
        _aritycheck(N, token, children)
        return new(token, children)
    end

    # Helpers
    function SyntaxBranch(token::Connective, children...)
        return SyntaxBranch(token, children)
    end
end

children(φ::SyntaxBranch) = φ.children
token(φ::SyntaxBranch) = φ.token

################################################################################
################################################################################

"""
    collatetruth(c::Connective, ts::NTuple{N,T where T<:Truth})::Truth where {N}

Return the truth value for a composed formula `c(t1, ..., tN)`, given the `N`
with t1, ..., tN being `Truth` values.

See also [`simplify`](@ref), [`Connective`](@ref), [`Truth`](@ref).
"""
function collatetruth(
        c::Connective,
        ts::NTuple{N, T where T <: Truth},
)::Truth where {N}
    if arity(c) != length(ts)
        return error("Cannot collate $(length(ts)) truth values for " *
                     "connective $(typeof(c)) with arity $(arity(c))).")
    else
        return error("Please, provide method collatetruth(::$(typeof(c)), " *
                     "::NTuple{$(arity(c)),$(T)}).")
    end
end

# Helper (so that collatetruth work for all operators)
collatetruth(t::Truth, ::Tuple{}) = t

# With generic formulas, it composes formula
"""
    simplify(c::Connective, ts::NTuple{N,F where F<:Formula})::Truth where {N}

Return a formula with the same semantics of a composed formula `c(φ1, ..., φN)`,
given the `N`
immediate sub-formulas.

See also [`collatetruth`](@ref), [`Connective`](@ref), [`Formula`](@ref).
"""
function simplify(c::Connective, φs::NTuple{N, T where T <: Formula}) where {N}
    c(φs)
end

function simplify(c::Connective, φs::NTuple{N, T where T <: Truth}) where {N}
    collatetruth(c, φs)
end

############################################################################################
##################################### BASE CONNECTIVES #####################################
############################################################################################

"""
    struct NamedConnective{Symbol} <: Connective end

A singleton type for representing connectives defined by a name or a symbol.

# Examples
The AND connective (i.e., the logical conjunction) is defined as the subtype:

    const CONJUNCTION = NamedConnective{:∧}()
    const ∧ = CONJUNCTION
    arity(::typeof(∧)) = 2

See also [`NEGATION`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref),
[`IMPLICATION`](@ref), [`Connective`](@ref).
"""
struct NamedConnective{Symbol} <: Connective end

name(::NamedConnective{S}) where {S} = S

Base.show(io::IO, c::NamedConnective) = print(io, "$(syntaxstring(c))")

syntaxstring(c::NamedConnective; kwargs...) = string(name(c))

function precedence(c::NamedConnective)
    op = SoleLogics.name(c)
    # Using default Base.operator_precedence is risky. For example,
    # Base.isoperator(:(¬)) is true, but Base.operator_precedence(:(¬)) is 0.
    # See Base.operator_precedence documentation.
    if !Base.isoperator(op) || Base.operator_precedence(op) == 0
        error("Please, provide method SoleLogics.precedence(::$(typeof(c))).")
    else
        Base.operator_precedence(op)
    end
end

function associativity(c::NamedConnective)
    op = SoleLogics.name(c)
    # Base.isoperator(:(++)) is true, but Base.operator_precedence(:(++)) is :none
    if !Base.isoperator(op) || !(Base.operator_associativity(op) in [:left, :right])
        error("Please, provide method SoleLogics.associativity(::$(typeof(c))).")
    else
        Base.operator_associativity(op)
    end
end

doc_NEGATION = """
    const NEGATION = NamedConnective{:¬}()
    const ¬ = NEGATION
    arity(::typeof(¬)) = 1

Logical negation (also referred to as complement).
It can be typed by `\\neg<tab>`.

See also [`NamedConnective`](@ref), [`Connective`](@ref).
"""
"""$(doc_NEGATION)"""
const NEGATION = NamedConnective{:¬}()
"""$(doc_NEGATION)"""
const ¬ = NEGATION
arity(::typeof(¬)) = 1

# ¬ is a risky symbol, since by default it's precedence is defaulted to 0 by julia.
# Because of this, we override Base.operator_precedence.
precedence(::typeof(¬)) = Base.operator_precedence(:∧) + 1

doc_CONJUNCTION = """
    const CONJUNCTION = NamedConnective{:∧}()
    const ∧ = CONJUNCTION
    arity(::typeof(∧)) = 2

Logical conjunction.
It can be typed by `\\wedge<tab>`.

See also [`NamedConnective`](@ref), [`Connective`](@ref).
"""
"""$(doc_CONJUNCTION)"""
const CONJUNCTION = NamedConnective{:∧}()
"""$(doc_CONJUNCTION)"""
const ∧ = CONJUNCTION
arity(::typeof(∧)) = 2

doc_DISJUNCTION = """
    const DISJUNCTION = NamedConnective{:∨}()
    const ∨ = DISJUNCTION
    arity(::typeof(∨)) = 2

Logical disjunction.
It can be typed by `\\vee<tab>`.

See also [`NamedConnective`](@ref), [`Connective`](@ref).
"""
"""$(doc_DISJUNCTION)"""
const DISJUNCTION = NamedConnective{:∨}()
"""$(doc_DISJUNCTION)"""
const ∨ = DISJUNCTION
arity(::typeof(∨)) = 2

doc_IMPLICATION = """
    const IMPLICATION = NamedConnective{:→}()
    const → = IMPLICATION
    arity(::typeof(→)) = 2

Logical implication.
It can be typed by `\\to<tab>`.

See also [`NamedConnective`](@ref), [`Connective`](@ref).
"""
"""$(doc_IMPLICATION)"""
const IMPLICATION = NamedConnective{:→}()
"""$(doc_IMPLICATION)"""
const → = IMPLICATION
arity(::typeof(→)) = 2

iscommutative(::typeof(∧)) = true
iscommutative(::typeof(→)) = false
iscommutative(::typeof(∨)) = true

hasdual(::typeof(∧)) = true
dual(c::typeof(∧)) = typeof(∨)
hasdual(::typeof(∨)) = true
dual(c::typeof(∨)) = typeof(∧)

############################################################################################
###################################### BOOLEAN ALGEBRA #####################################
############################################################################################

"""
    struct BooleanTruth <: Truth
        flag::Bool
    end

Structure for representing the Boolean truth values ⊤ and ⊥.
It wraps a flag which takes value `true` for ⊤ ([`TOP`](@ref)),
and `false` for ⊥ ([`BOT`](@ref))

See also [`BooleanAlgebra`](@ref).
"""
struct BooleanTruth <: Truth
    flag::Bool
end

istop(t::BooleanTruth) = t.flag
isbot(t::BooleanTruth) = !istop(t)

syntaxstring(t::BooleanTruth; kwargs...) = istop(t) ? "⊤" : "⊥"

function Base.show(io::IO, φ::BooleanTruth)
    print(io, "$(syntaxstring(φ))")
end

doc_TOP = """
    const TOP = BooleanTruth(true)
    const ⊤ = TOP

Canonical truth operator representing the value `true`.
It can be typed by `\\top<tab>`.

See also [`BOT`](@ref), [`Truth`](@ref).
"""
"""$(doc_TOP)"""
const TOP = BooleanTruth(true)
"""$(doc_TOP)"""
const ⊤ = TOP

doc_BOTTOM = """
    const BOT = BooleanTruth(false)
    const ⊥ = BOT

Canonical truth operator representing the value `false`.
It can be typed by `\\bot<tab>`.

See also [`TOP`](@ref), [`Truth`](@ref).
"""
"""$(doc_BOTTOM)"""
const BOT = BooleanTruth(false)
"""$(doc_BOTTOM)"""
const ⊥ = BOT

# NOTE: it could be useful to provide a macro to easily create
# a new set of Truth types. In particular, a new subtree of types must be planted
# as children of Truth, and new promotion rules are to be defined like below.
Base.promote_rule(::Type{<:BooleanTruth}, ::Type{<:BooleanTruth}) = BooleanTruth

function Base.convert(::Type{BooleanTruth}, t::Bool)::BooleanTruth
    return (t ? TOP : BOT)
end
function Base.convert(::Type{BooleanTruth}, t::Integer)::BooleanTruth
    if isone(t)
        return TOP
    elseif iszero(t)
        return BOT
    else
        return error("Cannot interpret Integer value $t as BooleanTruth.")
    end
end

Base.convert(::Type{Truth}, t::Bool) = Base.convert(BooleanTruth, t)
Base.convert(::Type{Truth}, t::Integer) = Base.convert(BooleanTruth, t)

# NOTE: are these useful?
hasdual(::BooleanTruth) = true
dual(c::BooleanTruth) = BooleanTruth(!istop(c))

precedes(t1::BooleanTruth, t2::BooleanTruth) = istop(t1) < istop(t2)
truthmeet(t1::BooleanTruth, t2::BooleanTruth) = precedes(t1, t2) ? t1 : t2
truthjoin(t1::BooleanTruth, t2::BooleanTruth) = precedes(t1, t2) ? t2 : t1

"""
    struct BooleanAlgebra <: AbstractAlgebra{Bool} end

A [Boolean algebra](https://en.wikipedia.org/wiki/Boolean_algebra), defined on the values
TOP (representing *truth*) and BOT (for bottom, representing *falsehood*).
For this algebra, the basic operators negation,
conjunction and disjunction (stylized as ¬, ∧, ∨) can be defined as the complement, minimum
and maximum, of the integer cast of `true` and `false`, respectively.

See also [`Truth`](@ref).
"""
struct BooleanAlgebra <: AbstractAlgebra{BooleanTruth} end

domain(::BooleanAlgebra) = [TOP, BOT]

top(::BooleanAlgebra) = TOP
bot(::BooleanAlgebra) = BOT

############################################################################################

# Standard semantics for NOT, AND, OR, IMPLIES
collatetruth(::typeof(¬), (ts,)::Tuple{BooleanTruth}) = istop(ts) ? BOT : TOP
function collatetruth(::typeof(∧), (t1, t2)::NTuple{N, T where T <: BooleanTruth}) where {N}
    truthmeet(t1, t2)
end
function collatetruth(::typeof(∨), (t1, t2)::NTuple{N, T where T <: BooleanTruth}) where {N}
    truthjoin(t1, t2)
end

# Incomplete information
function simplify(::typeof(∧), (t1, t2)::Tuple{BooleanTruth, BooleanTruth})
    istop(t1) && istop(t2) ? TOP : BOT
end
simplify(::typeof(∧), (t1, t2)::Tuple{BooleanTruth, Formula}) = istop(t1) ? t2 : t1
simplify(::typeof(∧), (t1, t2)::Tuple{Formula, BooleanTruth}) = istop(t2) ? t1 : t2

function simplify(::typeof(∨), (t1, t2)::Tuple{BooleanTruth, BooleanTruth})
    isbot(t1) && isbot(t2) ? BOT : TOP
end
simplify(::typeof(∨), (t1, t2)::Tuple{BooleanTruth, Formula}) = isbot(t1) ? t2 : t1
simplify(::typeof(∨), (t1, t2)::Tuple{Formula, BooleanTruth}) = isbot(t2) ? t1 : t2

# The IMPLIES operator, →, falls back to using ¬ and ∨
function collatetruth(::typeof(→), (t1, t2)::NTuple{2, BooleanTruth})
    return collatetruth(∨, (collatetruth(¬, (t1,)), t2))
end

############################################################################################

# With dense, discrete algebras, floats can be used.
# These are sketches for a few ideas. Note that truth values should be wrapped into Truth substructures:
# istop(ts::AbstractFloat)::Bool = isone(ts)
# isbot(ts::AbstractFloat)::Bool = iszero(ts)

# # TODO idea: use full range for numbers!
# # istop(ts::AbstractFloat)::Bool = ts == typemax(typeof(ts))
# # isbot(ts::AbstractFloat)::Bool = ts == typemin(typeof(ts))
# istop(ts::Integer)::Bool = ts == typemax(typeof(ts))
# isbot(ts::Integer)::Bool = ts == typemin(typeof(ts))

# TODO:
# struct DiscreteChainAlgebra{T} <: AbstractAlgebra{T} domain::Vector{T} end
# struct DenseChainAlgebra{T<:AbstractFloat} <: AbstractAlgebra{T} end

# TODO:
# struct HeytingNode{T} end
# struct HeytingAlgebra{T} <: AbstractAlgebra{HeytingNode{T}} ... end

############################################################################################
########################################### LOGIC ##########################################
############################################################################################

"""
    struct BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra} <: AbstractLogic{G,A}
        grammar::G
        algebra::A
    end

A basic logic based on a grammar and an algebra, where both the grammar and the algebra
are instantiated.

See also [`grammar`](@ref), [`algebra`](@ref),
[`AbstractGrammar`](@ref), [`AbstractAlgebra`](@ref), [`AbstractLogic`](@ref).
"""
struct BaseLogic{G <: AbstractGrammar, A <: AbstractAlgebra} <: AbstractLogic{G, A}
    grammar::G
    algebra::A

    function BaseLogic{G, A}(
            grammar::G = BASE_GRAMMAR,
            algebra::A = BooleanAlgebra(),
    ) where {G <: AbstractGrammar, A <: AbstractAlgebra}
        # @assert all([goeswith(c, algebra) for c in operators(grammar)]) "Cannot instantiate BaseLogic{$(G),$(A)}: operators $(operators(grammar)[[goeswith(c, algebra) for c in operators(grammar)]]) cannot be interpreted on $(algebra)." # requires `goeswith` trait
        return new{G, A}(grammar, algebra)
    end

    function BaseLogic{G}(
            grammar::G = BASE_GRAMMAR,
            algebra::A = BooleanAlgebra(),
    ) where {G <: AbstractGrammar, A <: AbstractAlgebra}
        return BaseLogic{G, A}(grammar, algebra)
    end

    function BaseLogic(
            grammar::G = BASE_GRAMMAR,
            algebra::A = BooleanAlgebra(),
    ) where {G <: AbstractGrammar, A <: AbstractAlgebra}
        return BaseLogic{G, A}(grammar, algebra)
    end
end

grammar(l::BaseLogic) = l.grammar
algebra(l::BaseLogic) = l.algebra

function Base.isequal(a::BaseLogic, b::BaseLogic)
    return Base.isequal(grammar(a), grammar(b)) && Base.isequal(algebra(a), algebra(b))
end

Base.hash(a::BaseLogic) = Base.hash(algebra(a), Base.hash(grammar(a)))

function Base.show(
        io::IO, l::BaseLogic{G, A},) where {G <: AbstractGrammar, A <: AbstractAlgebra}
    if G <: CompleteFlatGrammar
        print(io,
            "BaseLogic with:\n\t- operators = [$(join(syntaxstring.(operators(l)), ", "))];\n\t- alphabet: $(alphabet(l));\n\t- algebra: $(algebra(l)).",)
    else
        print(io,
            "BaseLogic{$(G),$(A)}(\n\t- grammar: $(grammar(l));\n\t- algebra: $(algebra(l))\n)",)
    end
end




############################################################################################
#### ExplicitAlphabet ######################################################################
############################################################################################

"""
    struct ExplicitAlphabet{V} <: AbstractAlphabet{V}
        atoms::Vector{Atom{V}}
    end

An alphabet wrapping atoms in a (finite) `Vector`.

See also [`AbstractAlphabet`](@ref), [`atoms`](@ref).
"""
struct ExplicitAlphabet{V} <: AbstractAlphabet{V}
    atoms::Vector{Atom{V}}

    function ExplicitAlphabet{V}(atoms) where {V}
        return new{V}(collect(atoms))
    end

    function ExplicitAlphabet(atoms::AbstractVector{Atom{V}}) where {V}
        return ExplicitAlphabet{V}(collect(atoms))
    end

    function ExplicitAlphabet(atoms::AbstractVector{V}) where {V}
        return ExplicitAlphabet{V}(Atom.(collect(atoms)))
    end
end
atoms(a::ExplicitAlphabet) = a.atoms
natoms(a::ExplicitAlphabet) = length(atoms(a))

function Base.convert(::Type{AbstractAlphabet}, alphabet::Vector{<:Atom})
    ExplicitAlphabet(alphabet)
end

############################################################################################
#### AlphabetOfAny #########################################################################
############################################################################################

"""
    struct AlphabetOfAny{V} <: AbstractAlphabet{V} end

An implicit, infinite alphabet that includes all atoms with values of a subtype of V.

See also [`AbstractAlphabet`](@ref).
"""
struct AlphabetOfAny{V} <: AbstractAlphabet{V} end
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.in(::Atom{PV}, ::AlphabetOfAny{VV}) where {PV, VV} = (PV <: VV)

############################################################################################
#### UnionAlphabet #########################################################################
############################################################################################

# Finite alphabet of conditions induced from a set of metaconditions
"""
Alphabet given by the *union* of a number of (sub-)alphabets.

See also
[`UnboundedScalarAlphabet`](@ref),
[`ScalarCondition`](@ref),
[`ScalarMetaCondition`](@ref).
"""

struct UnionAlphabet{C, A <: AbstractAlphabet{C}} <: AbstractAlphabet{C}
    subalphabets::Vector{A}
end

subalphabets(a::UnionAlphabet) = a.subalphabets
nsubalphabets(a::UnionAlphabet) = length(subalphabets(a))

function Base.show(io::IO, a::UnionAlphabet)
    println(io, "$(typeof(a)):")
    for sa in subalphabets(a)
        Base.show(io, sa)
    end
end

function atoms(a::UnionAlphabet)
    return Iterators.flatten(Iterators.map(atoms, subalphabets(a)))
end

natoms(a::UnionAlphabet) = sum(natoms, subalphabets(a))

function Base.in(p::Atom, a::UnionAlphabet)
    return any(sa -> Base.in(p, sa), subalphabets(a))
end

############################################################################################
#### CompleteFlatGrammar ###################################################################
############################################################################################

"""
    struct CompleteFlatGrammar{V<:AbstractAlphabet,O<:Operator} <: AbstractGrammar{V,O}
        alphabet::V
        operators::Vector{<:O}
    end

V grammar of all well-formed formulas obtained by the arity-complying composition
of atoms of an alphabet of type `V`, and all operators in `operators`.
With n operators, this grammar has exactly n+1 production rules.
For example, with `operators = [∧,∨]`, the grammar (in Backus-Naur form) is:

    φ ::= p | φ ∧ φ | φ ∨ φ

with p ∈ alphabet. Note: it is *flat* in the sense that all rules substitute the same
(unique and starting) non-terminal symbol φ.

See also [`AbstractGrammar`](@ref), [`Operator`](@ref), [`alphabet`](@ref),
[`formulas`](@ref), [`connectives`](@ref), [`operators`](@ref), [`leaves`](@ref).
"""
struct CompleteFlatGrammar{V <: AbstractAlphabet, O <: Operator} <: AbstractGrammar{V, O}
    alphabet::V
    operators::Vector{<:O}

    function CompleteFlatGrammar{V, O}(
            alphabet::V,
            operators::Vector{<:O},
    ) where {V <: AbstractAlphabet, O <: Operator}
        return new{V, O}(alphabet, operators)
    end

    function CompleteFlatGrammar{V}(
            alphabet::V,
            operators::Vector{<:Operator},
    ) where {V <: AbstractAlphabet}
        return new{V, Union{typeof.(operators)...}}(
            alphabet,
            Vector{Union{typeof.(operators)...}}(operators),
        )
    end

    function CompleteFlatGrammar(
            alphabet::V,
            operators::Vector{<:Operator},
    ) where {V <: AbstractAlphabet}
        return new{V, Union{typeof.(operators)...}}(
            alphabet,
            Vector{Union{typeof.(operators)...}}(operators),
        )
    end
end

alphabet(g::CompleteFlatGrammar) = g.alphabet
operators(g::CompleteFlatGrammar) = g.operators

"""
    connectives(g::AbstractGrammar)

List all connectives appearing in a grammar.

See also [`Connective`](@ref), [`nconnectives`](@ref).
"""
function connectives(g::AbstractGrammar)::AbstractVector{Connective}
    return filter(!isnullary, operators(g))
end

"""
    leaves(g::AbstractGrammar)

List all leaves appearing in a grammar.

See also [`SyntaxLeaf`](@ref), [`nleaves`](@ref).
"""
function leaves(g::AbstractGrammar)
    return [atoms(alphabet(g))..., filter(isnullary, operators(g))...]
end

# V complete grammar includes any *safe* syntax tree that can be built with
#  the grammar token types.
function Base.in(φ::SyntaxTree, g::CompleteFlatGrammar)::Bool
    return if token(φ) isa Atom
        token(φ) in alphabet(g)
    elseif token(φ) isa Operator
        if operatorstype(φ) <: operatorstype(g)
            true
        else
            all([Base.in(c, g) for c in children(φ)])
        end
    else
        false
    end
end

"""
    formulas(
        g::CompleteFlatGrammar{V,O} where {V,O};
        maxdepth::Integer,
        nformulas::Union{Nothing,Integer} = nothing
    )::Vector{SyntaxBranch}

Generate all formulas whose `SyntaxBranch`s that are not taller than a given `maxdepth`.

See also [`AbstractGrammar`](@ref), [`SyntaxBranch`](@ref).
"""
function formulas(
        g::CompleteFlatGrammar{V, O} where {V, O};
        maxdepth::Integer,
        nformulas::Union{Nothing, Integer} = nothing,
)::Vector{SyntaxTree}
    @assert maxdepth >= 0
    @assert isnothing(nformulas) || nformulas > 0
    # With increasing `depth`, accumulate all formulas of length `depth` by combining all
    # formulas of `depth-1` using all non-terminal symbols.
    # Stop as soon as `maxdepth` is reached or `nformulas` have been generated.
    depth = 0
    cur_formulas = Vector{SyntaxTree}(leaves(g))
    all_formulas = SyntaxTree[cur_formulas...]
    while depth < maxdepth && (isnothing(nformulas) || length(all_formulas) < nformulas)
        _nformulas = length(all_formulas)
        cur_formulas = []
        for op in connectives(g)
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

# This dispatches are needed, since ambiguities might arise when choosing between
#   in(φ::SyntaxTree, g::SoleLogics.CompleteFlatGrammar) and
#   in(p::Atom, g::SoleLogics.AbstractGrammar)
Base.in(p::Atom, g::CompleteFlatGrammar) = Base.in(p, alphabet(g))
Base.in(op::Truth, g::CompleteFlatGrammar) = (op <: operatorstype(g))

############################################################################################
########################################### BASE ###########################################
############################################################################################

# This can be useful for standard phrasing of propositional formulas with string atoms.

"""
    const BASE_CONNECTIVES = [¬, ∧, ∨, →]

Basic logical operators.

See also [`NEGATION`](@ref),
[`CONJUNCTION`](@ref),
[`DISJUNCTION`](@ref),
[`IMPLICATION`](@ref),
[`Connective`](@ref).
"""
const BASE_CONNECTIVES = [¬, ∧, ∨, →]
const BaseConnectives = Union{typeof.(BASE_CONNECTIVES)...}

const BASE_ALPHABET = AlphabetOfAny{String}()

const BASE_GRAMMAR = CompleteFlatGrammar(BASE_ALPHABET, BASE_CONNECTIVES)
const BASE_ALGEBRA = BooleanAlgebra()

const BASE_LOGIC = BaseLogic(BASE_GRAMMAR, BASE_ALGEBRA)

function _baselogic(;
        alphabet::Union{Nothing, Vector, AbstractAlphabet} = nothing,
        operators::Union{Nothing, Vector{<:Operator}} = nothing,
        grammar::Union{Nothing, AbstractGrammar} = nothing,
        algebra::Union{Nothing, AbstractAlgebra} = nothing,
        default_operators::Vector{<:Operator},
        logictypename::String,
)
    if !(isnothing(grammar) || (isnothing(alphabet) && isnothing(operators)))
        error("Cannot instantiate $(logictypename) by specifing a grammar " *
              "together with argument(s): " *
              join(
                  [
                      (!isnothing(alphabet) ? ["alphabet"] : [])...,
                      (!isnothing(operators) ? ["operators"] : [])...,
                      (!isnothing(grammar) ? ["grammar"] : [])...,
                  ],
                  ", ",) * ".")
    end
    grammar = begin
        if isnothing(grammar)
            # @show alphabet
            # @show operators
            # @show BASE_GRAMMAR
            # if isnothing(alphabet) && isnothing(operators)
            # BASE_GRAMMAR
            # else
            alphabet = isnothing(alphabet) ? BASE_ALPHABET : alphabet
            operators = begin
                if isnothing(operators)
                    default_operators
                else
                    if length(setdiff(operators, default_operators)) > 0
                        @warn "Instantiating $(logictypename) with operators not in " *
                              "$(default_operators): " *
                              join(", ", setdiff(operators, default_operators)) * "."
                    end
                    operators
                end
            end
            if alphabet isa Vector
                alphabet = ExplicitAlphabet(map(Atom, alphabet))
            end
            CompleteFlatGrammar(alphabet, operators)
            # end
        else
            @assert isnothing(alphabet) && isnothing(operators)
            grammar
        end
    end

    algebra = isnothing(algebra) ? BASE_ALGEBRA : algebra

    return BaseLogic(grammar, algebra)
end
