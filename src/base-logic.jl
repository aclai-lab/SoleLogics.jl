export NamedOperator

export ∧, ¬, ∨, →
export CONJUNCTION, NEGATION, DISJUNCTION, IMPLICATION

export BooleanAlgebra, BaseLogic

############################################################################################
####################################### BASE OPERATORS #####################################
############################################################################################

"""
    struct NamedOperator{Symbol} <: AbstractOperator end

A singleton type for representing operators defined by a name or a symbol.

# Examples
The AND operator (logical conjuction) can be defined as the subtype:

    const CONJUNCTION = NamedOperator{:∧}()
    const ∧ = CONJUNCTION
    arity(::Type{typeof(∧)}) = 2

See also [`NEGATION`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref),
[`IMPLICATION`](@ref), [`AbstractOperator`](@ref).
"""
struct NamedOperator{Symbol} <: AbstractOperator end

name(::NamedOperator{S}) where {S} = S

# Base.show(io::IO, op::NamedOperator) = print(io, "$(syntaxstring(op))")
syntaxstring(op::NamedOperator; kwargs...) = string(name(op))

doc_NEGATION = """
    const NEGATION = NamedOperator{:¬}()
    const ¬ = NEGATION
    arity(::Type{typeof(¬)}) = 1

Logical negation.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_NEGATION)
"""
const NEGATION = NamedOperator{:¬}()
"""
$(doc_NEGATION)
"""
const ¬ = NEGATION
arity(::Type{typeof(¬)}) = 1

doc_CONJUNCTION = """
    const CONJUNCTION = NamedOperator{:∧}()
    const ∧ = CONJUNCTION
    arity(::Type{typeof(∧)}) = 2

Logical conjunction.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_CONJUNCTION)
"""
const CONJUNCTION = NamedOperator{:∧}()
"""
$(doc_CONJUNCTION)
"""
const ∧ = CONJUNCTION
arity(::Type{typeof(∧)}) = 2

doc_DISJUNCTION = """
    const DISJUNCTION = NamedOperator{:∨}()
    const ∨ = DISJUNCTION
    arity(::Type{typeof(∨)}) = 2

Logical disjunction.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_DISJUNCTION)
"""
const DISJUNCTION = NamedOperator{:∨}()
"""
$(doc_DISJUNCTION)
"""
const ∨ = DISJUNCTION
arity(::Type{typeof(∨)}) = 2

doc_IMPLICATION = """
    const IMPLICATION = NamedOperator{:→}()
    const → = IMPLICATION
    arity(::Type{typeof(→)}) = 2

Logical implication.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_IMPLICATION)
"""
const IMPLICATION = NamedOperator{:→}()
"""
$(doc_IMPLICATION)
"""
const → = IMPLICATION
arity(::Type{typeof(→)}) = 2

# Helpers
# TODO2: I am not a great fan of this.. it is really the best way to do it?
# TODO I could find a way that is more elegant that would not introduce ambiguity in dispatching.
# Note that these are only helpers, so it's not really problem if they are hard to understand.
function CONJUNCTION(
    c1::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
    c2::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
    c3::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
    cs::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula}...
)
    return CONJUNCTION(c1, CONJUNCTION(c2, c3, cs...))
end
function CONJUNCTION(
    c1::Union{AbstractSyntaxToken,SyntaxTree},
    c2::Union{AbstractSyntaxToken,SyntaxTree},
    c3::Union{AbstractSyntaxToken,SyntaxTree},
    cs::Union{AbstractSyntaxToken,SyntaxTree}...
)
    return CONJUNCTION(c1, CONJUNCTION(c2, c3, cs...))
end
function DISJUNCTION(
    c1::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
    c2::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
    c3::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula},
    cs::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula}...
)
    return DISJUNCTION(c1, DISJUNCTION(c2, c3, cs...))
end
function DISJUNCTION(
    c1::Union{AbstractSyntaxToken,SyntaxTree},
    c2::Union{AbstractSyntaxToken,SyntaxTree},
    c3::Union{AbstractSyntaxToken,SyntaxTree},
    cs::Union{AbstractSyntaxToken,SyntaxTree}...
)
    return DISJUNCTION(c1, DISJUNCTION(c2, c3, cs...))
end

############################################################################################
########################################## ALGEBRA #########################################
############################################################################################

"""
    struct BooleanAlgebra <: AbstractAlgebra{Bool} end

[Boolean algebra](https://en.m.wikipedia.org/wiki/Boolean_algebra) is defined on the values
`true` (top) and `false` (bottom). For this algebra, the basic operators negation,
conjunction and disjunction (stylized as ¬, ∧, ∨) can be defined as the complement, minimum
and maximum, respectively.

See also [`TruthValue`](@ref).
"""
struct BooleanAlgebra <: AbstractAlgebra{Bool} end

domain(::BooleanAlgebra) = [true, false]
top(a::BooleanAlgebra) = true
bottom(a::BooleanAlgebra) = false

# Standard semantics for NOT, AND, OR, IMPLIES
collate_truth(::BooleanAlgebra, ::typeof(¬), (t,)::NTuple{1}) = (!t)
collate_truth(::BooleanAlgebra, ::typeof(∧), (t1, t2)::NTuple{2}) = min(t1, t2)
collate_truth(::BooleanAlgebra, ::typeof(∨), (t1, t2)::NTuple{2}) = max(t1, t2)

# The IMPLIES operator, →, falls back to ¬
function collate_truth(a::BooleanAlgebra, ::typeof(→), (t1, t2)::NTuple{2})
    return collate_truth(a, ∨, (collate_truth(a, ¬, t1), t2))
end

default_algebra(::Type{Bool}) = BooleanAlgebra{Bool}()


# TODO:
# struct DiscreteChainAlgebra{T} <: AbstractAlgebra{T} domain::Vector{T} end
# struct DenseChainAlgebra{T<:AbstractFloat} <: AbstractAlgebra{T} end
# default_algebra(::Type{T}) where {T<:AbstractFloat} = DenseChainAlgebra{T}()

# TODO:
# struct HeytingNode{T} end
# struct HeytingAlgebra{T} <: AbstractAlgebra{HeytingNode{T}} ... end
# default_algebra(::Type{<:HeytingNode{T}}) = error("...")

############################################################################################
########################################### LOGIC ##########################################
############################################################################################

"""
    struct BaseLogic{G<:AbstractGrammar, A<:AbstractAlgebra} <: AbstractLogic{G, A}
        grammar::G
        algebra::A
    end

Basic logic type based on a grammar and an algebra, where both the grammar and the algebra
are instantiated.

See also [`AbstractGrammar`](@ref), [`AbstractAlgebra`](@ref), [`AbstractLogic`](@ref).
"""
struct BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra} <: AbstractLogic{G,A}
    grammar::G
    algebra::A

    function BaseLogic{G,A}(
        grammar::G = BASE_GRAMMAR,
        algebra::A = BooleanAlgebra(),
    ) where {G<:AbstractGrammar,A<:AbstractAlgebra}
        # @assert all([goeswith(op, algebra) for op in operators(grammar)]) "Cannot instantiate BaseLogic{$(G),$(A)}: operators $(operators(grammar)[[goeswith(op, algebra) for op in operators(grammar)]]) cannot be interpreted on $(algebra)." # requires `goeswith` trait
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

"""
A base logic can be used to instantiate `Formula`s out of syntax trees.
"""
(l::BaseLogic)(t::SyntaxTree, args...) = Formula(Base.RefValue(l), t; args...)

############################################################################################
########################################### BASE ###########################################
############################################################################################


# This can be useful for standard phrasing of propositional formulas with string propositions.

"""
    const BASE_OPERATORS = [⊤, ⊥, ¬, ∧, ∨, →]

Basic logical operators.

See also [`TOP`](@ref), [`BOTTOM`](@ref), [`NEGATION`](@ref),
[`CONJUCTION`](@ref), [`AbstractOperator`](@ref).
"""
const BASE_OPERATORS = [⊤, ⊥, ¬, ∧, ∨, →]
const BaseOperators = Union{typeof.(BASE_OPERATORS)...}

const BASE_ALPHABET = AlphabetOfAny{String}()

const BASE_GRAMMAR = CompleteFlatGrammar(BASE_ALPHABET, BASE_OPERATORS)
const BASE_ALGEBRA = BooleanAlgebra()

const BASE_LOGIC = BaseLogic(BASE_GRAMMAR, BASE_ALGEBRA)

function _base_logic(;
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
    default_operators::Vector{<:AbstractOperator},
    logictypename::String,
)
    @assert isnothing(grammar) || (isnothing(alphabet) && isnothing(operators)) ||
            "Cannot instantiate $(logictypename) by specifing a grammar together with parameter(s):
            $(join([
                (!isnothing(alphabet) ? ["alphabet"] : [])...,
                (!isnothing(operators) ? ["operators"] : [])...,
                (!isnothing(grammar) ? ["grammar"] : [])...,
                ], ", "))."

    grammar = begin
        if isnothing(grammar)
            if isnothing(alphabet) && isnothing(operators)
                BASE_GRAMMAR
            else
                alphabet = isnothing(alphabet) ? BASE_ALPHABET : alphabet
                operators = begin
                    if isnothing(operators)
                        default_operators
                    else
                        if length(setdiff(operators, default_operators)) > 0
                            @warn "Instantiating $(logictypename) with operators not in" *
                                " $(default_operators): " *
                                join(", ", setdiff(operators, default_operators)) * "."
                        end
                        operators
                    end
                end
                if alphabet isa Vector
                    alphabet = ExplicitAlphabet(map(Proposition, alphabet))
                end
                CompleteFlatGrammar(alphabet, operators)
            end
        else
            @assert isnothing(alphabet) && isnothing(operators)
            grammar
        end
    end

    algebra = isnothing(algebra) ? BASE_ALGEBRA : algebra

    return BaseLogic(grammar, algebra)
end
