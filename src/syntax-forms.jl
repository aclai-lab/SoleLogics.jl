doc_lmlf = """
    struct LeftmostLinearForm{O<:AbstractOperator, SS<:AbstractSyntaxStructure} <: AbstractSyntaxStructure
        children::Vector{<:SS}
    end

A syntax structure representing the `foldl` of a set of other syntax structure of type `SS`
by means of an operator `O`. This structure enables a structured instantiation of
formulas in conjuctive/disjunctive forms, and
conjuctive normal form (CNF) or disjunctive normal form (DNF), defined as:

    const LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}
    const LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

    const CNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),LeftmostLinearForm{typeof(∨),SS}}
    const DNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),LeftmostLinearForm{typeof(∧),SS}}

# Examples
TODO four examples of syntaxstring of a LeftmostConjunctiveForm, LeftmostDisjunctiveForm, CNF, DNF
"""

"""$(doc_lmlf)

See also [`AbstractSyntaxStructure`](@ref), [`SyntaxTree`](@ref),
[`LeftmostConjunctiveForm`](@ref), [`LeftmostDisjunctiveForm`](@ref),
[`Literal`](@ref).
"""
struct LeftmostLinearForm{O<:AbstractOperator, SS<:AbstractSyntaxStructure} <: AbstractSyntaxStructure
    children::Vector{<:SS}

    function LeftmostLinearForm{O,SS}(
        children::Vector,
    ) where {O<:AbstractOperator,SS<:AbstractSyntaxStructure}
        new{O,SS}(children)
    end

    function LeftmostLinearForm{O}(
        children::Vector,
    ) where {O <: AbstractOperator}
        SS = SoleBase._typejoin(typeof.(children)...)
        LeftmostLinearForm{O,SS}(children)
    end

    function LeftmostLinearForm(
        O::Type{<:SoleLogics.AbstractOperator},
        children::Vector,
    )
        LeftmostLinearForm{O}(children)
    end

    function LeftmostLinearForm(
        op::AbstractOperator,
        children::Vector,
    )
        LeftmostLinearForm(typeof(op),children)
    end
end

children(lf::LeftmostLinearForm) = lf.children
op(::LeftmostLinearForm{O}) where {O} = O()

operatortype(::LeftmostLinearForm{O}) where {O} = O
childrentype(::LeftmostLinearForm{O,SS}) where {O,SS} = SS

convert(::Type{SyntaxTree}, lf::LeftmostLinearForm) = op(lf)(children(lf)...)

############################################################################################

"""
    struct Literal{T<:AbstractSyntaxToken} <: AbstractSyntaxStructure
        ispos::Bool
        prop::T
    end

A proposition or its negation.

See also [`CNF`](@ref), [`DNF`](@ref).
"""
struct Literal{T<:AbstractSyntaxToken} <: AbstractSyntaxStructure
    ispos::Bool
    prop::T

    function Literal{T}(
        ispos::Bool,
        prop::T,
    ) where {T<:AbstractSyntaxToken}
        new{T}(ispos, prop)
    end

    function Literal(
        ispos::Bool,
        prop::T,
    ) where {T<:AbstractSyntaxToken}
        Literal{T}(ispos, prop)
    end
end

ispos(l::Literal) = l.ispos
prop(l::Literal) = l.prop

convert(::Type{SyntaxTree}, l::Literal) = ispos ? SyntaxTree(l.prop) : ¬(SyntaxTree(l.prop))

complement(l::Literal) = Literal(!ispos(l), prop(l))

############################################################################################

"""$(doc_lmlf)"""
const LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}
"""$(doc_lmlf)"""
const LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

"""$(doc_lmlf)"""
const CNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),LeftmostLinearForm{typeof(∨),SS}}
"""$(doc_lmlf)"""
const DNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),LeftmostLinearForm{typeof(∧),SS}}
