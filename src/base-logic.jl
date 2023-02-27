"""
    collatetruth(
        a::AbstractAlgebra,
        op::AbstractOperator,
        t::NTuple{N,T},
    )::T where {N,T<:TruthValue}

Returns the truth value of a composed formula op(φ1, ..., φN), given the `N`
truth values of its immediate sub-formulas.
An algebra must provide a `collatetruth` method for each operator that can be
interpreted on it.

See also [`AbstractAlgebra`](@ref) [`AbstractOperator`](@ref), [`TruthValue`](@ref).
"""
function collatetruth(
    a::AbstractAlgebra{T},
    op::AbstractOperator,
    t::NTuple{N,T},
)::T where {N,T<:TruthValue}
    if truthtype(a) != T
        return error("Cannot collate $(length(t)) truth values of type $(T)" *
                     " with algebra $(typeof(a)) with truth type $(truthtype(a))).")
    elseif arity(op) != length(t)
        return error("Cannot collate $(length(t)) truth values for" *
                     " operator $(typeof(op)) with arity $(arity(op))).")
    else
        return error("Please, provide method collatetruth(::$(typeof(a)), ::$(typeof(op))," *
                     " ::NTuple{$(arity(op)),$(truthtype(a))}.")
    end
end

# Note: `collatetruth` for TOP and BOTTOM relies on the `top` and `bottom` methods.
collatetruth(a::AbstractAlgebra{T}, ::typeof(⊤), t::NTuple{0,T}) where {T<:TruthValue} = top(a)
collatetruth(a::AbstractAlgebra{T}, ::typeof(⊥), t::NTuple{0,T}) where {T<:TruthValue} = bottom(a)

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
"""$(doc_NEGATION)"""
const NEGATION = NamedOperator{:¬}()
"""$(doc_NEGATION)"""
const ¬ = NEGATION
arity(::Type{typeof(¬)}) = 1

doc_CONJUNCTION = """
    const CONJUNCTION = NamedOperator{:∧}()
    const ∧ = CONJUNCTION
    arity(::Type{typeof(∧)}) = 2

Logical conjunction.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""$(doc_CONJUNCTION)"""
const CONJUNCTION = NamedOperator{:∧}()
"""$(doc_CONJUNCTION)"""
const ∧ = CONJUNCTION
arity(::Type{typeof(∧)}) = 2

doc_DISJUNCTION = """
    const DISJUNCTION = NamedOperator{:∨}()
    const ∨ = DISJUNCTION
    arity(::Type{typeof(∨)}) = 2

Logical disjunction.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""$(doc_DISJUNCTION)"""
const DISJUNCTION = NamedOperator{:∨}()
"""$(doc_DISJUNCTION)"""
const ∨ = DISJUNCTION
arity(::Type{typeof(∨)}) = 2

doc_IMPLICATION = """
    const IMPLICATION = NamedOperator{:→}()
    const → = IMPLICATION
    arity(::Type{typeof(→)}) = 2

Logical implication.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""$(doc_IMPLICATION)"""
const IMPLICATION = NamedOperator{:→}()
"""$(doc_IMPLICATION)"""
const → = IMPLICATION
arity(::Type{typeof(→)}) = 2

# Helpers that allow the conjuction/disjuction of more than two tokens/formulas.
function CONJUNCTION(
    c1::Union{AbstractSyntaxToken,AbstractFormula},
    c2::Union{AbstractSyntaxToken,AbstractFormula},
    c3::Union{AbstractSyntaxToken,AbstractFormula},
    cs::Union{AbstractSyntaxToken,AbstractFormula}...
)
    return CONJUNCTION(c1, CONJUNCTION(c2, c3, cs...))
end
function CONJUNCTION(
    c1::Union{AbstractSyntaxToken,AbstractSyntaxStructure},
    c2::Union{AbstractSyntaxToken,AbstractSyntaxStructure},
    c3::Union{AbstractSyntaxToken,AbstractSyntaxStructure},
    cs::Union{AbstractSyntaxToken,AbstractSyntaxStructure}...
)
    return CONJUNCTION(c1, CONJUNCTION(c2, c3, cs...))
end
function DISJUNCTION(
    c1::Union{AbstractSyntaxToken,AbstractSyntaxStructure},
    c2::Union{AbstractSyntaxToken,AbstractSyntaxStructure},
    c3::Union{AbstractSyntaxToken,AbstractSyntaxStructure},
    cs::Union{AbstractSyntaxToken,AbstractSyntaxStructure}...
)
    return DISJUNCTION(c1, DISJUNCTION(c2, c3, cs...))
end
function DISJUNCTION(
    c1::Union{AbstractSyntaxToken,AbstractFormula},
    c2::Union{AbstractSyntaxToken,AbstractFormula},
    c3::Union{AbstractSyntaxToken,AbstractFormula},
    cs::Union{AbstractSyntaxToken,AbstractFormula}...
)
    return DISJUNCTION(c1, DISJUNCTION(c2, c3, cs...))
end

# TODO example _iscommutative(::Type{typeof(xor)}) = true
_iscommutative(::Type{typeof(∧)}) = true
_iscommutative(::Type{typeof(∨)}) = true

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
collatetruth(::BooleanAlgebra, ::typeof(¬), (t,)::NTuple{1,Bool}) = (!t)
collatetruth(::BooleanAlgebra, ::typeof(∧), (t1, t2)::NTuple{2,Bool}) = min(t1, t2)
collatetruth(::BooleanAlgebra, ::typeof(∨), (t1, t2)::NTuple{2,Bool}) = max(t1, t2)

# The IMPLIES operator, →, falls back to ¬
function collatetruth(a::BooleanAlgebra, ::typeof(→), (t1, t2)::NTuple{2,Bool})
    return collatetruth(a, ∨, (collatetruth(a, ¬, t1), t2))
end


# Bool values -> Boolean algebra
tops(t::Bool)::Bool = (t == true)
bottoms(t::Bool)::Bool = (t == false)
default_algebra(::Type{Bool}) = BooleanAlgebra{Bool}()

# # With dense, discrete algebras, floats can be used.
# tops(t::AbstractFloat)::Bool = isone(t)
# bottoms(t::AbstractFloat)::Bool = iszero(t)

# # TODO idea: use full range for numbers!
# # tops(t::AbstractFloat)::Bool = t == typemax(typeof(t))
# # bottoms(t::AbstractFloat)::Bool = t == typemin(typeof(t))
# tops(t::Integer)::Bool = t == typemax(typeof(t))
# bottoms(t::Integer)::Bool = t == typemin(typeof(t))

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
    struct BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra} <: AbstractLogic{G,A}
        grammar::G
        algebra::A
    end

Basic logic type based on a grammar and an algebra, where both the grammar and the algebra
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

function _baselogic(;
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
            # end
        else
            @assert isnothing(alphabet) && isnothing(operators)
            grammar
        end
    end

    algebra = isnothing(algebra) ? BASE_ALGEBRA : algebra

    return BaseLogic(grammar, algebra)
end

"""
    function baseformula(
        tokf::Union{AbstractSyntaxToken,AbstractFormula};
        operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
        kwargs...,
    )

Attempts at instantiating a `Formula` from a syntax token/formula,
by inferring the logic it belongs to.

# Examples
```julia-repl
julia> t = parseformulatree("◊((p∧q)→r)");

julia> operators(logic(SoleLogics.baseformula(t)))
3-element Vector{Union{SoleLogics.NamedOperator{:→}, SoleLogics.NamedOperator{:◊}, SoleLogics.NamedOperator{:∧}}}:
 ∧
 ◊
 →

julia> operators(logic(SoleLogics.baseformula(t; operators = SoleLogics.BASE_MODAL_OPERATORS)))
8-element Vector{Union{SoleLogics.BottomOperator, SoleLogics.NamedOperator{:¬}, SoleLogics.NamedOperator{:∧}, SoleLogics.NamedOperator{:∨}, SoleLogics.NamedOperator{:→}, SoleLogics.NamedOperator{:◊}, SoleLogics.NamedOperator{:□}, SoleLogics.TopOperator}}:
 ⊤
 ⊥
 ¬
 ∧
 ∨
 →
 ◊
 □
```
"""
function baseformula(
    tokf::Union{AbstractSyntaxToken,AbstractFormula};
    operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    # additional_operators::Vector{<:AbstractOperator} = AbstractOperator[],
    kwargs...,
)
    t = convert(SyntaxTree, tokf)
    ops = isnothing(operators) ? SoleLogics.operators(t) : operators
    # operators = unique([additional_operators..., ops...])
    # props = propositions(t)

    logic = begin
        if issubset(ops, BASE_PROPOSITIONAL_OPERATORS)
            propositionallogic(;
                operators = ops,
                kwargs...,
            )
        elseif issubset(ops, BASE_MODAL_OPERATORS)
            modallogic(;
                operators = ops,
                kwargs...,
            )
        else
            error("Could not infer logic from object of type $(typeof(tokf)): $(t). operators = $(ops).")
        end
    end
    Formula(logic, t)
end
