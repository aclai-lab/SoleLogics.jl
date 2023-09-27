"""
    collatetruth(
        a::AbstractAlgebra,
        op::Operator,
        t::NTuple{N,T},
    )::T where {N,T<:Truth}

Return the truth value of a composed formula op(φ1, ..., φN), given the `N`
truth values of its immediate sub-formulas.
An algebra must provide a `collatetruth` method for each operator that can be
interpreted on it.

See also [`AbstractAlgebra`](@ref) [`Operator`](@ref), [`Truth`](@ref).
"""
function collatetruth(
    a::AbstractAlgebra{T},
    op::Connective,
    t::NTuple{N,T},
)::T where {N,T<:Truth}
    if truthtype(a) != T
        return error("Cannot collate $(length(t)) truth values of type $(T) " *
                     "with algebra $(typeof(a)) with truth type $(truthtype(a))).")
    elseif arity(op) != length(t)
        return error("Cannot collate $(length(t)) truth values for " *
                     "operator $(typeof(op)) with arity $(arity(op))).")
    else
        return error("Please, provide method collatetruth(::$(typeof(a)), ::$(typeof(op)), " *
                     "::NTuple{$(arity(op)),$(truthtype(a))}).")
    end
end

# Note: `collatetruth` for TOP and BOTTOM relies on the `top` and `bottom` methods.
collatetruth(a::AbstractAlgebra{T}, ::typeof(⊤), t::NTuple{0,T}) where {T<:Truth} = top(a)
collatetruth(a::AbstractAlgebra{T}, ::typeof(⊥), t::NTuple{0,T}) where {T<:Truth} = bottom(a)

############################################################################################
####################################### BASE OPERATORS #####################################
############################################################################################

"""
TODO: @typeHierarchyUpdate

    struct NamedConnective{Symbol} <: Connective end

A singleton type for representing connectives defined by a name or a symbol.

# Examples
The AND operator (logical conjuction) is defined as the subtype:

    const CONJUNCTION = NamedConnective{:∧}()
    const ∧ = CONJUNCTION
    arity(::Type{typeof(∧)}) = 2

See also [`NEGATION`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref),
[`IMPLICATION`](@ref), [`Operator`](@ref).
"""
struct NamedConnective{Symbol} <: Connective end

name(::NamedConnective{S}) where {S} = S

Base.show(io::IO, op::NamedConnective) = print(io, "$(syntaxstring(op))")

syntaxstring(op::NamedConnective; kwargs...) = string(name(op))

doc_NEGATION = """
    const NEGATION = NamedConnective{:¬}()
    const ¬ = NEGATION
    arity(::Type{typeof(¬)}) = 1

Logical negation (also referred to as complement).
It can be typed by `\\neg<tab>`.

See also [`NamedConnective`](@ref), [`Operator`](@ref).
"""
"""$(doc_NEGATION)"""
const NEGATION = NamedConnective{:¬}()
"""$(doc_NEGATION)"""
const ¬ = NEGATION
arity(::Type{typeof(¬)}) = 1

doc_CONJUNCTION = """
    const CONJUNCTION = NamedConnective{:∧}()
    const ∧ = CONJUNCTION
    arity(::Type{typeof(∧)}) = 2

Logical conjunction.
It can be typed by `\\wedge<tab>`.

See also [`NamedConnective`](@ref), [`Operator`](@ref).
"""
"""$(doc_CONJUNCTION)"""
const CONJUNCTION = NamedConnective{:∧}()
"""$(doc_CONJUNCTION)"""
const ∧ = CONJUNCTION
arity(::Type{typeof(∧)}) = 2

doc_DISJUNCTION = """
    const DISJUNCTION = NamedConnective{:∨}()
    const ∨ = DISJUNCTION
    arity(::Type{typeof(∨)}) = 2

Logical disjunction.
It can be typed by `\\vee<tab>`.

See also [`NamedConnective`](@ref), [`Operator`](@ref).
"""
"""$(doc_DISJUNCTION)"""
const DISJUNCTION = NamedConnective{:∨}()
"""$(doc_DISJUNCTION)"""
const ∨ = DISJUNCTION
arity(::Type{typeof(∨)}) = 2

doc_IMPLICATION = """
    const IMPLICATION = NamedConnective{:→}()
    const → = IMPLICATION
    arity(::Type{typeof(→)}) = 2

Logical implication.
It can be typed by `\\to<tab>`.

See also [`NamedConnective`](@ref), [`Operator`](@ref).
"""
"""$(doc_IMPLICATION)"""
const IMPLICATION = NamedConnective{:→}()
"""$(doc_IMPLICATION)"""
const → = IMPLICATION
arity(::Type{typeof(→)}) = 2

Base.operator_precedence(::typeof(IMPLICATION)) = LOW_PRECEDENCE

# Helpers that allow the conjuction/disjuction of more than two tokens/formulas.
function CONJUNCTION(
    c1::Union{SyntaxToken,AbstractSyntaxStructure},
    c2::Union{SyntaxToken,AbstractSyntaxStructure},
    c3::Union{SyntaxToken,AbstractSyntaxStructure},
    cs::Union{SyntaxToken,AbstractSyntaxStructure}...
)
    return CONJUNCTION(c1, CONJUNCTION(c2, c3, cs...))
end
function DISJUNCTION(
    c1::Union{SyntaxToken,AbstractSyntaxStructure},
    c2::Union{SyntaxToken,AbstractSyntaxStructure},
    c3::Union{SyntaxToken,AbstractSyntaxStructure},
    cs::Union{SyntaxToken,AbstractSyntaxStructure}...
)
    return DISJUNCTION(c1, DISJUNCTION(c2, c3, cs...))
end

iscommutative(::Type{typeof(∧)}) = true
iscommutative(::Type{typeof(∨)}) = true

hasdual(::typeof(∧)) = true
dual(op::typeof(∧)) = typeof(∨)
hasdual(::typeof(∨)) = true
dual(op::typeof(∨))     = typeof(∧)


############################################################################################
########################################## ALGEBRA #########################################
############################################################################################

"""
    struct BooleanAlgebra <: AbstractAlgebra{Bool} end

A [boolean algebra](https://en.wikipedia.org/wiki/Boolean_algebra), defined on the values
`true` (for top) and `false` (for bottom). For this algebra, the basic operators negation,
conjunction and disjunction (stylized as ¬, ∧, ∨) can be defined as the complement, minimum
and maximum, respectively.

See also [`Truth`](@ref).
"""
struct BooleanAlgebra <: AbstractAlgebra{BooleanTruth} end

domain(::BooleanAlgebra) = [true, false]
top(a::BooleanAlgebra) = true
bottom(a::BooleanAlgebra) = false

# TODO: @typeHierarchyUpdate
# Base.convert(::Type{Bool}, tok::Top) = true
# Base.convert(::Type{Bool}, tok::Bottom) = false

toval(::Top) = true
toval(::Bottom) = false

function collatetruth(
    a::BooleanAlgebra,
    o::Connective,
    ch::NTuple{N,BooleanTruth}
) where {N}
    _collatetruth(a, o, toval.(ch))
end

# Standard semantics for NOT, AND, OR, IMPLIES
_collatetruth(::BooleanAlgebra, ::typeof(¬), (t,)::NTuple{1,Bool}) = (!t)
_collatetruth(::BooleanAlgebra, ::typeof(∧), (t1, t2)::NTuple{2,Bool}) = min(t1, t2)
_collatetruth(::BooleanAlgebra, ::typeof(∨), (t1, t2)::NTuple{2,Bool}) = max(t1, t2)

# The IMPLIES operator, →, falls back to ¬
function collatetruth(a::BooleanAlgebra, ::typeof(→), (t1, t2)::NTuple{2,Bool})
    return collatetruth(a, ∨, (collatetruth(a, ¬, (t1,)), t2))
end


# Bool values -> Boolean algebra
istop(t::Top)::Bool = (t == true)
isbottom(t::Bottom)::Bool = (t == false)
default_algebra(::Type{<:BooleanTruth}) = BooleanAlgebra()

# # With dense, discrete algebras, floats can be used.
# istop(t::AbstractFloat)::Bool = isone(t)
# isbottom(t::AbstractFloat)::Bool = iszero(t)

# # TODO idea: use full range for numbers!
# # istop(t::AbstractFloat)::Bool = t == typemax(typeof(t))
# # isbottom(t::AbstractFloat)::Bool = t == typemin(typeof(t))
# istop(t::Integer)::Bool = t == typemax(typeof(t))
# isbottom(t::Integer)::Bool = t == typemin(typeof(t))

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


# This can be useful for standard phrasing of propositional formulas with string atoms.

"""
    const BASE_OPERATORS = [⊤, ⊥, ¬, ∧, ∨, →]

Basic logical operators.

See also [`TOP`](@ref), [`BOTTOM`](@ref), [`NEGATION`](@ref),
[`CONJUCTION`](@ref), [`Operator`](@ref).
"""
const BASE_OPERATORS = Operator[⊤, ⊥, ¬, ∧, ∨, →]
# const BaseOperators = Union{typeof.(BASE_OPERATORS)...}
#
# const BASE_ALPHABET = AlphabetOfAny{String}()
#
# const BASE_GRAMMAR = CompleteFlatGrammar(BASE_ALPHABET, BASE_OPERATORS)
# const BASE_ALGEBRA = BooleanAlgebra()
#
# const BASE_LOGIC = BaseLogic(BASE_GRAMMAR, BASE_ALGEBRA)
#
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

"""
TODO: @typeHierarchyUpdate
    function baseformula(
        tokf::Union{AbstractSyntaxToken,AbstractFormula};
        infer_logic = true,
        additional_operators::Union{Nothing,Vector{<:Operator}} = nothing,
        kwargs...,
    )

Attempt at instantiating a `Formula` from a syntax token/formula,
by inferring the logic it belongs to. If `infer_logic` is true, then
a canonical logic (e.g., propositional logic with all the `BASE_PROPOSITIONAL_OPERATORS`) is
inferred; if it's false, then a logic with exactly the operators appearing in the syntax tree,
plus the `additional_operators` is instantiated.

# Examples
```julia-repl
julia> t = parseformula("◊((p∧q)→r)");

julia> operators(logic(SoleLogics.baseformula(t)))
3-element Vector{Union{SoleLogics.NamedConnective{:→}, SoleLogics.NamedConnective{:◊}, SoleLogics.NamedConnective{:∧}}}:
 ∧
 ◊
 →

julia> operators(logic(SoleLogics.baseformula(t; additional_operators = SoleLogics.BASE_MODAL_OPERATORS)))
8-element Vector{Union{SoleLogics.BottomOperator, SoleLogics.NamedConnective{:¬}, SoleLogics.NamedConnective{:∧}, SoleLogics.NamedConnective{:∨}, SoleLogics.NamedConnective{:→}, SoleLogics.NamedConnective{:◊}, SoleLogics.NamedConnective{:□}, SoleLogics.TopOperator}}:
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
    tokf::Union{AbstractSyntaxStructure,Connective};
    infer_logic = true,
    additional_operators::Union{Nothing,Vector{<:Operator}} = nothing,
    kwargs...,
)
    t = convert(SyntaxTree, tokf)

    ops = isnothing(additional_operators) ? SoleLogics.operators(t) : additional_operators
    # operators = unique([additional_operators..., ops...])
    # props = atoms(t)

    logic = begin
        if issubset(ops, BASE_PROPOSITIONAL_OPERATORS)
            propositionallogic(;
                operators = (infer_logic ? BASE_PROPOSITIONAL_OPERATORS : ops),
                kwargs...,
            )
        elseif issubset(ops, BASE_MODAL_OPERATORS)
            modallogic(;
                operators = (infer_logic ? BASE_MODAL_OPERATORS : ops),
                default_operators = BASE_MODAL_OPERATORS,
                kwargs...,
            )
        elseif issubset(ops, BASE_MULTIMODAL_OPERATORS)
            modallogic(;
                operators = (infer_logic ? BASE_MULTIMODAL_OPERATORS : ops),
                default_operators = BASE_MULTIMODAL_OPERATORS,
                kwargs...,
            )
        else
            unknown_ops = setdiff(ops, BASE_PROPOSITIONAL_OPERATORS, BASE_MODAL_OPERATORS, BASE_MULTIMODAL_OPERATORS)
            error("Could not infer logic from object of type $(typeof(tokf)): $(t). Unknown operators: $(unknown_ops).")
        end
    end
    Formula(logic, t)
end
