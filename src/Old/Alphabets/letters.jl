abstract type AbstractPropositionalLetter end
is_proposition(::Any) = false
is_proposition(::AbstractPropositionalLetter) = true

# This represents a variable that could be a letter (in the future) or already is one.
const MetaLetter = Union{<:AbstractPropositionalLetter, String}

############################################################################################
# Utilities
############################################################################################

# Default Letter type, used when nothing is specified (e.g. using an external constructor).
const _DEFAULT_LETTER_TYPE = Number

# Check whether is it legal to confront the specified attribute with threshold's type.
function _is_comparable(a::Any, b::Any)
    if !(typeof(a) <: typeof(b))
        error("$a and $b must be comparable.")
    end
end

# Check wheter the specified operator is binary.
function _is_valid_operator(operator::AbstractOperator)
    if SoleLogics.ariety(operator) != 2
        error("Operator $operator must be binary.")
    end
end

# NOTE: currently, Letter is immutable but looking ahead, we defined the setters anyway.
struct Letter{T} <: AbstractPropositionalLetter
    relation::Union{Relation, Nothing}         # Relation of a certain custom-made modal logic formalism.
    feature::Union{Function, Nothing}          # Function manipulating the attribute.
    attribute::Any                             # Attribute.
    operator::Union{AbstractOperator, Nothing} # Binary operator.
    threshold::Union{T, Nothing}               # Comparison value of a specific type.

    name::Union{String, Nothing}               # Compact readable string associated with the letter.

    ########################################################################################
    # Constructors
    ########################################################################################

    # Empty Letter, with only an identification name.
    function Letter{T}(name::Union{String, Symbol}) where {T}
        new{T}(nothing, nothing, nothing, nothing, nothing, string(name))
    end

    # Letter where both Relation and Feature are the identity itself.
    function Letter{T}(
        attribute::Any,
        operator::AbstractOperator,
        threshold::T;
        name::Union{String, Symbol, Nothing} = nothing
    ) where {T}
        _is_comparable(attribute, threshold)
        _is_valid_operator(operator)

        new{T}(nothing, nothing, attribute, operator, threshold, isnothing(name) ? name : string(name))
    end

    # Letter where a specific Relation is specified.
    function Letter{T}(
        relation::Relation,
        attribute::Any,
        operator::AbstractOperator,
        threshold::T;
        name::Union{String, Symbol, Nothing} = nothing
    ) where {T}
        _is_comparable(attribute, threshold)
        _is_valid_operator(operator)

        new{T}(relation, nothing, attribute, operator, threshold, isnothing(name) ? name : string(name))
    end

    # Letter where a specific Feature is specified.
    function Letter{T}(
        feature::Function,
        attribute::Any,
        operator::AbstractOperator,
        threshold::T;
        name::Union{String, Symbol, Nothing} = nothing
    ) where {T}
        _is_comparable(attribute, threshold)
        _is_valid_operator(operator)

        new{T}(nothing, feature, attribute, operator, threshold, isnothing(name) ? name : string(name))
    end

    # A complete Letter, where both Relation and Feature are specified.
    function Letter{T}(
        relation::Relation,
        feature::Function,
        attribute::Any,
        operator::AbstractOperator,
        threshold::T;
        name::Union{String, Symbol, Nothing} = nothing
    ) where {T}
        _is_comparable(attribute, threshold)
        _is_valid_operator(operator)

        new{T}(relation, feature, attribute, operator, threshold, isnothing(name) ? name : string(name))
    end
end

Letter(name::String) = Letter{_DEFAULT_LETTER_TYPE}(name)

#=
How to implement propositional letters with shape "a1 ⋈ R f(A) ⋈ a2" is to be decided
togheter. The following is just a toy example / idea using delegation pattern.

struct LeftBoundedProposition{T} <: AbstractPropositionalLetter
    left_threshold::T
    left_operator::AbstractOperator
    letter::Letter{T}
end
@forward LeftBoundedProposition.letter, relation, feature, attribute...
=#

""" TODO: add documentation. """
relation(letter::Letter{T}) where {T} = letter.relation
relation!(letter::Letter{T}, relation::Relation) where {T} = letter.relation = relation

""" TODO: add documentation. """
feature(letter::Letter{T}) where {T} = letter.feature
feature!(letter::Letter{T}, feature::Function) where {T} = begin
    letter.feature = feature
end

""" TODO: add documentation. """
attribute(letter::Letter{T}) where {T} = letter.attribute
attribute!(letter::Letter{T}, attribute::Any) where {T} = begin
    _is_comparable(attribute, letter.threshold)
    letter.attribute = attribute
end

""" TODO: add documentation. """
operator(letter::Letter{T}) where {T} = letter.operator
operator!(letter::Letter{T}, operator::AbstractOperator) where {T} = begin
    _is_valid_operator(operator)
    letter.operator = operator
end

""" TODO: add documentation. """
threshold(letter::Letter{T}) where {T} = letter.threshold
threshold!(letter::Letter{T}, threshold::T) where {T} = begin
    _is_comparable(letter.attribute, threshold)
    letter.threshold = threshold
end

""" TODO: add documentation. """
name(letter::Letter{T}) where {T} = letter.name
name!(letter::Letter{T}, name::String) where {T} = letter.name = name

""" TODO: add documentation. """
Base.string(letter::Letter{T}) where {T} = begin
    # Attempt to return `letter` verbose string representation.
    if !isnothing(letter.name)
        return letter.name
    elseif !isnothing(letter.attribute) &&
        !isnothing(letter.operator) &&
        !isnothing(letter.threshold)
        return (isnothing(letter.relation) ? "" : "$(letter.relation) ") * (isnothing(letter.feature) ? "id($(letter.attribute)) " : "$(letter.feature)($(letter.attribute)) ") * "$(letter.operator) $(letter.threshold)"
    # Attempt to print `letter` .
    else
        @warn "Unable to print $letter"
        return ""
    end
end

Base.show(io::IO, letter::Letter) = print(io, string(letter))

#=
using SoleLogics

a = SoleLogics.Letter{Int64}(5, CONJUNCTION, 6, name="p");

---------------------------------

gen_formula driver

generator = ["a", "b", "c", "d", "e"]
letters = Vector{SoleLogics.Letter{Int64}}()
for s in generator
    push!(letters, SoleLogics.Letter{Int64}(s) )
end
gen_formula(4, letters)
=#
