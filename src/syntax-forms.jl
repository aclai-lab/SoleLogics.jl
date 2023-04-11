import Base: show, promote_rule
using SoleBase

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
```julia-repl
julia> lfcf = LeftmostConjunctiveForm{Literal}([l1,l2_neg,l1_float])
LeftmostLinearForm{SoleLogics.NamedOperator{:∧},Literal}
(1) ∧ (¬(2)) ∧ (1.0)

julia> lfdf = LeftmostDisjunctiveForm{Literal}([l1_number_float,l_string_neg])
LeftmostLinearForm{SoleLogics.NamedOperator{:∨},Literal}
(1.4) ∨ (¬(1))
```
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
        a = arity(O)
        n_children = length(children)

        if a == 0
            n_children == 0 ||
                error("Mismatching number of children and operator's arity.")
        elseif a == 1
            n_children == 1 ||
                error("Mismatching number of children and operator's arity.")
        else
            h = (n_children-1)/(a-1)
            (isinteger(h) && h >= 1) ||
                error("Mismatching number of children and operator's arity.")
        end

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

function syntaxstring(
    lf::LeftmostLinearForm;
    # function_notation = false,
    kwargs...,
)
    ch = children(lf)
    if length(ch) == 0
        # syntaxstring(op(lf); function_notation = function_notation, kwargs...)
        syntaxstring(op(lf); kwargs...)
    else
        children_ss = map(
            # c->syntaxstring(c; function_notation = function_notation, kwargs...),
            c->syntaxstring(c; kwargs...),
            ch
        )
        # if function_notation
            # "$(syntaxstring(op(lf)))(" * join(children_ss, ", ") * ")"
        # else
        "(" * join(children_ss, ") $(syntaxstring(op(lf); kwargs...)) (") * ")"
    end
end

function tree(lf::LeftmostLinearForm)
    operator = op(lf)
    a = arity(operator)
    function _tree(children::Vector{<:AbstractSyntaxStructure})
        return length(children) == a ?
            SyntaxTree(operator, children...) :
            SyntaxTree(operator, children[1:(a-1)]..., _tree(children[a:end]))
    end
    _tree(tree.(children(lf)))
end

function Base.show(io::IO, lf::LeftmostLinearForm{O,SS}) where {O,SS}
    println(io, "LeftmostLinearForm{$(O),$(SS)}")
    println(io, "\t$(syntaxstring(lf))")
end

Base.promote_rule(::Type{<:LeftmostLinearForm}, ::Type{<:LeftmostLinearForm}) where {O} = SyntaxTree
Base.promote_rule(::Type{SS}, ::Type{LF}) where {SS<:AbstractSyntaxStructure, LF<:LeftmostLinearForm} = SyntaxTree
Base.promote_rule(::Type{LF}, ::Type{SS}) where {LF<:LeftmostLinearForm, SS<:AbstractSyntaxStructure} = SyntaxTree

############################################################################################

"""
    struct Literal{T<:AbstractSyntaxToken} <: AbstractSyntaxStructure
        ispos::Bool
        prop::T
    end

A proposition or its negation.

See also [`CNF`](@ref), [`DNF`](@ref), [`AbstractSyntaxStructure`](@ref).
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

propositionstype(::Literal{T}) where {T} = T

tree(l::Literal) = ispos(l) ? SyntaxTree(l.prop) : ¬(SyntaxTree(l.prop))

complement(l::Literal) = Literal(!ispos(l), prop(l))

function Base.show(io::IO, l::Literal)
    println(io,
        "Literal{$(propositionstype(l))}: " * (ispos(l) ? "" : "¬") * syntaxstring(prop(l))
    )
end

############################################################################################

"""$(doc_lmlf)"""
const LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}
"""$(doc_lmlf)"""
const LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

"""$(doc_lmlf)"""
const CNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),LeftmostLinearForm{typeof(∨),SS}}
"""$(doc_lmlf)"""
const DNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),LeftmostLinearForm{typeof(∧),SS}}
