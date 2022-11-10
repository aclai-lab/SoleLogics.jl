abstract type AbstractPropositionalLetter end

struct Letter{T} <: AbstractPropositionalLetter
    relation::Union{Relation, Nothing}         # Relation of a certain custom-made modal logic formalism.
    feature::Union{Function, Nothing}          # Function manipulating the attribute.
    attribute::Any                             # Attribute.
    operator::Union{AbstractOperator, Nothing} # Binary operator.
    threshold::Union{T, Nothing}               # Comparison value of a specific type.

    name::Union{String, Nothing}               # Compact readable string associated with the letter.

    ########################################################################################
    # Utilities
    ########################################################################################

    # Check whether is it legal to confront the specified attribute with threshold's type.
    function is_comparable(a::Any, b::Any)
        if !(typeof(a) <: typeof(b))
            error("$a and $b must be comparable.")
        end
    end

    # Check wheter the specified operator is binary.
    function valid_operator(operator::AbstractOperator)
        if SoleLogics.ariety(operator) != 2
            error("Operator $operator must be binary.")
        end
    end

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
        name::Union{String, Symbol, Nothing}=nothing
    ) where {T}
        is_comparable(attribute, threshold)
        valid_operator(operator)

        new{T}(nothing, nothing, attribute, operator, threshold, isnothing(name) ? name : string(name))
    end

    # Letter where a specific Relation is specified.
    function Letter{T}(
        relation::Relation,
        attribute::Any,
        operator::AbstractOperator,
        threshold::T;
        name::Union{String, Symbol, Nothing}=nothing
    ) where {T}
        is_comparable(attribute, threshold)
        valid_operator(operator)

        new{T}(relation, nothing, attribute, operator, threshold, isnothing(name) ? name : string(name))
    end

    # Letter where a specific Feature is specified.
    function Letter{T}(
        feature::Function,
        attribute::Any,
        operator::AbstractOperator,
        threshold::T;
        name::Union{String, Symbol, Nothing}=nothing
    ) where {T}
        is_comparable(attribute, threshold)
        valid_operator(operator)

        new{T}(nothing, feature, attribute, operator, threshold, isnothing(name) ? name : string(name))
    end

    # A complete Letter, where both Relation and Feature are specified.
    function Letter{T}(
        relation::Relation,
        feature::Function,
        attribute::Any,
        operator::AbstractOperator,
        threshold::T;
        name::Union{String, Symbol, Nothing}=nothing
    ) where {T}
        is_comparable(attribute, threshold)
        valid_operator(operator)

        new{T}(relation, feature, attribute, operator, threshold, isnothing(name) ? name : string(name))
    end
end

# Toy example of letter print.
# TODO: separate verbose case (R f(A) ⋈ a) and non-verbose case ("p")
function Base.show(io::IO, letter::Letter{T}) where {T}
    if !(isnothing(letter.name))
        print(letter.name)
    else
        output_string =
            (isnothing(letter.relation) ? "" : "$(letter.relation) ") *
            (isnothing(letter.feature)
                ? "id($(letter.attribute)) "
                : "$(letter.feature)($(letter.attribute)) ") *
            "$(letter.operator) $(letter.threshold)"
        print(output_string)
    end
end
