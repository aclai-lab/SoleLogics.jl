"""
    collatetruth(c::Connective, ts::NTuple{N,T where T<:Truth})::Truth where {N}

Return the truth value for a composed formula `c(t1, ..., tN)`, given the `N`
with t1, ..., tN being `Truth` values.

See also [`simplify`](@ref), [`Connective`](@ref), [`Truth`](@ref).
"""
function collatetruth(
    c::Connective,
    ts::NTuple{N,T where T<:Truth}
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
function simplify(c::Connective, φs::NTuple{N,T where T<:Formula}) where {N}
    c(φs)
end

function simplify(c::Connective, φs::NTuple{N,T where T<:Truth}) where {N}
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
precedence(::typeof(¬)) = Base.operator_precedence(:∧)+1

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
iscommutative(::typeof(∨)) = true

hasdual(::typeof(∧)) = true
dual(c::typeof(∧))   = typeof(∨)
hasdual(::typeof(∨)) = true
dual(c::typeof(∨))   = typeof(∧)


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
dual(c::BooleanTruth)   = BooleanTruth(!istop(c))

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
collatetruth(::typeof(∧), (t1, t2)::NTuple{N,T where T<:BooleanTruth}) where {N} = truthmeet(t1, t2)
collatetruth(::typeof(∨), (t1, t2)::NTuple{N,T where T<:BooleanTruth}) where {N} = truthjoin(t1, t2)

# Incomplete information
function simplify(::typeof(∧), (t1, t2)::Tuple{BooleanTruth,BooleanTruth})
    istop(t1) && istop(t2) ? TOP : BOT
end
simplify(::typeof(∧), (t1, t2)::Tuple{BooleanTruth,Formula}) = istop(t1) ? t2 : t1
simplify(::typeof(∧), (t1, t2)::Tuple{Formula,BooleanTruth}) = istop(t2) ? t1 : t2


function simplify(::typeof(∨), (t1, t2)::Tuple{BooleanTruth,BooleanTruth})
    isbot(t1) && isbot(t2) ? BOT : TOP
end
simplify(::typeof(∨), (t1, t2)::Tuple{BooleanTruth,Formula}) = isbot(t1) ? t2 : t1
simplify(::typeof(∨), (t1, t2)::Tuple{Formula,BooleanTruth}) = isbot(t2) ? t1 : t2

# The IMPLIES operator, →, falls back to using ¬ and ∨
function collatetruth(::typeof(→), (t1, t2)::NTuple{2,BooleanTruth})
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
struct BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra} <: AbstractLogic{G,A}
    grammar::G
    algebra::A

    function BaseLogic{G,A}(
        grammar::G = BASE_GRAMMAR,
        algebra::A = BooleanAlgebra(),
    ) where {G<:AbstractGrammar,A<:AbstractAlgebra}
        # @assert all([goeswith(c, algebra) for c in operators(grammar)]) "Cannot instantiate BaseLogic{$(G),$(A)}: operators $(operators(grammar)[[goeswith(c, algebra) for c in operators(grammar)]]) cannot be interpreted on $(algebra)." # requires `goeswith` trait
        return new{G,A}(grammar, algebra)
    end

    function BaseLogic{G}(
        grammar::G = BASE_GRAMMAR,
        algebra::A = BooleanAlgebra(),
    ) where {G<:AbstractGrammar,A<:AbstractAlgebra}
        return BaseLogic{G,A}(grammar, algebra)
    end

    function BaseLogic(
        grammar::G = BASE_GRAMMAR,
        algebra::A = BooleanAlgebra(),
    ) where {G<:AbstractGrammar,A<:AbstractAlgebra}
        return BaseLogic{G,A}(grammar, algebra)
    end
end

grammar(l::BaseLogic) = l.grammar
algebra(l::BaseLogic) = l.algebra

function Base.isequal(a::BaseLogic, b::BaseLogic)
    return Base.isequal(grammar(a), grammar(b)) && Base.isequal(algebra(a), algebra(b))
end

Base.hash(a::BaseLogic) = Base.hash(algebra(a), Base.hash(grammar(a)))


function Base.show(io::IO, l::BaseLogic{G,A}) where {G<:AbstractGrammar,A<:AbstractAlgebra}
    if G <: CompleteFlatGrammar
        print(io, "BaseLogic with:\n\t- operators = [$(join(syntaxstring.(operators(l)), ", "))];\n\t- alphabet: $(alphabet(l));\n\t- algebra: $(algebra(l)).")
    else
        print(io, "BaseLogic{$(G),$(A)}(\n\t- grammar: $(grammar(l));\n\t- algebra: $(algebra(l))\n)")
    end
end

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
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:Operator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
    default_operators::Vector{<:Operator},
    logictypename::String,
)
    if !(isnothing(grammar) || (isnothing(alphabet) && isnothing(operators)))
        error("Cannot instantiate $(logictypename) by specifing a grammar " *
            "together with argument(s): " * join([
                (!isnothing(alphabet) ? ["alphabet"] : [])...,
                (!isnothing(operators) ? ["operators"] : [])...,
                (!isnothing(grammar) ? ["grammar"] : [])...,
                ], ", ") * ".")
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
