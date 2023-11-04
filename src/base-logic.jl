"""
    collatetruth(
        c::Connective,
        ts::NTuple{N,T},
    )::T where {N,T<:Truth}

Return the truth value of a composed formula c(φ1, ..., φN), given the `N`
truth values of its immediate sub-formulas.

See also [`AbstractAlgebra`](@ref) [`Connective`](@ref), [`Truth`](@ref).
"""
function collatetruth(
    c::Connective,
    ts::NTuple{N,T},
)::T where {N,T<:Truth}
    # if truthtype(a) != T
    #     return error("Cannot collate $(length(ts)) truth values of type $(T) " *
    #                  "with algebra $(typeof(a)) with truth type $(truthtype(a))).")
    # else
    if arity(c) != length(ts)
        return error("Cannot collate $(length(ts)) truth values for " *
                     "connective $(typeof(c)) with arity $(arity(c))).")
    else
        return error("Please, provide method collatetruth(::$(typeof(c)), " *
                     "::NTuple{$(arity(c)),$(T)}).")
    end
end

# collatetruth is defined for every operator, both Connectives and Truth values.

# `collatetruth` for any truth value returns itself.
collatetruth(t::Truth, ::Tuple{}) = t

function collatetruth(c::Connective, ts::NTuple{N,T}) where {N,T<:Formula}
    @assert arity(c) == N
        "Connective $(syntaxstring(c)) cannot be applied on its children $(ts)"
    c(ts...)
end
############################################################################################
##################################### BASE CONNECTIVES #####################################
############################################################################################

"""
    struct NamedConnective{Symbol} <: Connective end

A singleton type for representing connectives defined by a name or a symbol.

# Examples
The AND connective (logical conjuction) is defined as the subtype:

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
    Base.operator_precedence(SoleLogics.name(c))
end

function associativity(c::NamedConnective)
    Base.operator_associativity(SoleLogics.name(c))
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
precedence(::typeof(¬)) = 15 # Inspired by Base.operator_precedence(:(^)) which is 15

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
precedence(::typeof(∧)) = 12 # Base.operator_precedence(:∧) is 12, this is for completeness

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
precedence(::typeof(∨)) = precedence(CONJUNCTION) - 1;

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
precedence(::typeof(→)) = 4 # As in Base.operator_precedence, this is for completeness

# Helpers that allow the conjuction/disjuction/implication of more than two tokens/formulas.
function CONJUNCTION(
    c1::Formula,
    c2::Formula,
    c3::Formula,
    cs::Formula...
)
    @assert associativity(CONJUNCTION) == :left
    cs2 = [c3, cs...]
    children = (CONJUNCTION(c1, c2, cs2[1:end-1]...), cs2[end])
    T = Base.promote_type((typeof.(children))...)
    T <: SyntaxTree || (children = Base.promote(children...))
    return joinformulas(CONJUNCTION, children)
end
function DISJUNCTION(
    c1::Formula,
    c2::Formula,
    c3::Formula,
    cs::Formula...
)
    @assert associativity(DISJUNCTION) == :left
    cs2 = [c3, cs...]
    children = (DISJUNCTION(c1, c2, cs2[1:end-1]...), cs2[end])
    T = Base.promote_type((typeof.(children))...)
    T <: SyntaxTree || (children = Base.promote(children...))
    return joinformulas(DISJUNCTION, children)
end
function IMPLICATION(
    c1::Formula,
    c2::Formula,
    c3::Formula,
    cs::Formula...
)
    @assert associativity(IMPLICATION) == :right
    children = (c1, IMPLICATION(c2, c3, cs...))
    T = Base.promote_type((typeof.(children))...)
    T <: SyntaxTree || (children = Base.promote(children...))
    return joinformulas(IMPLICATION, children)
end

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
    abstract type BooleanTruth <: Truth end

Supertype of `Top` and `Bot`, the two truth values of `BooleanAlgebra`

See also [`Bot`](@ref), [`Top`](@ref), [`BooleanAlgebra`](@ref).
"""
abstract type BooleanTruth <: Truth end

doc_TOP = """
    struct Top <: Truth end
    const TOP = Top()
    const ⊤ = TOP

Canonical truth operator representing the value `true`.
It can be typed by `\\top<tab>`.

See also [`BOT`](@ref), [`Truth`](@ref).
"""
"""$(doc_TOP)"""
struct Top <: BooleanTruth end
"""$(doc_TOP)"""
const TOP = Top()
"""$(doc_TOP)"""
const ⊤ = TOP

syntaxstring(o::Top; kwargs...) = "⊤"

istop(t::Top) = true

doc_BOTTOM = """
    struct Bot <: Truth end
    const BOT = Bot()
    const ⊥ = BOT

Canonical truth operator representing the value `false`.
It can be typed by `\\bot<tab>`.

See also [`TOP`](@ref), [`Truth`](@ref).
"""
"""$(doc_BOTTOM)"""
struct Bot <: BooleanTruth end
"""$(doc_BOTTOM)"""
const BOT = Bot()
"""$(doc_BOTTOM)"""
const ⊥ = BOT

syntaxstring(o::Bot; kwargs...) = "⊥"

isbot(t::Bot) = true

# NOTE: it could be useful to provide a macro to easily create
# a new set of Truth types. In particular, a new subtree of types must be planted
# as children of Truth, and new promotion rules are to be defined like below.
Base.promote_rule(::Type{<:BooleanTruth}, ::Type{<:BooleanTruth}) = BooleanTruth

# Helpers
Base.convert(::Type{Bool}, ::Top) = true
Base.convert(::Type{Bool}, ::Bot) = false

function Base.convert(::Union{Type{Truth},Type{BooleanTruth}}, t::Bool)
    return (t ? Top : Bot)
end
function Base.convert(::Union{Type{Truth},Type{BooleanTruth}}, t::Integer)
    if isone(t)
        return Top
    elseif iszero(t)
        return Bot
    else
        return error("Cannot interpret Integer value $t as Truth.")
    end
end

# NOTE: are these useful?
hasdual(::typeof(⊤)) = true
dual(c::typeof(⊤))   = typeof(⊥)
hasdual(::typeof(⊥)) = true
dual(c::typeof(⊥))   = typeof(⊤)


"""
    struct BooleanAlgebra <: AbstractAlgebra{Bool} end

A [boolean algebra](https://en.wikipedia.org/wiki/Boolean_algebra), defined on the values
Top (representing `true`) and Bot (for bottom, representing `false`).
For this algebra, the basic operators negation,
conjunction and disjunction (stylized as ¬, ∧, ∨) can be defined as the complement, minimum
and maximum, of the integer cast of `true` and `false`, respectively.

See also [`Truth`](@ref).
"""
struct BooleanAlgebra <: AbstractAlgebra{BooleanTruth} end

domain(::BooleanAlgebra) = [TOP, BOT]

top(::BooleanAlgebra) = TOP
bot(::BooleanAlgebra) = BOT


function collatetruth(
    c::Connective,
    ch::NTuple{N,T}
)::BooleanTruth where {N,T<:BooleanTruth}
    _collatetruth(c, convert.(Bool, ch)) == true ? TOP : BOT
end

# Standard semantics for NOT, AND, OR, IMPLIES
_collatetruth(::typeof(¬), (ts,)::NTuple{1,Bool}) = (!ts)
_collatetruth(::typeof(∧), (t1, t2)::NTuple{2,Bool}) = min(t1, t2)
_collatetruth(::typeof(∨), (t1, t2)::NTuple{2,Bool}) = max(t1, t2)

# The IMPLIES operator, →, falls back to ¬
function _collatetruth(::typeof(→), (t1, t2)::NTuple{2,Bool})
    return _collatetruth(∨, (_collatetruth(¬, (t1,)), t2))
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
    const BASE_OPERATORS = [¬, ∧, ∨, →]

Basic logical operators.

See also [`NEGATION`](@ref),
[`CONJUCTION`](@ref),
[`DISJUNCTION`](@ref),
[`IMPLICATION`](@ref),
[`Connective`](@ref).
"""
const BASE_OPERATORS = [¬, ∧, ∨, →]
const BaseOperators = Union{typeof.(BASE_OPERATORS)...}

const BASE_ALPHABET = AlphabetOfAny{String}()

const BASE_GRAMMAR = CompleteFlatGrammar(BASE_ALPHABET, BASE_OPERATORS)
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
