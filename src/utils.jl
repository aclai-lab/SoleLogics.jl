function base_formula(
    ttf::Union{AbstractSyntaxToken,SyntaxTree,AbstractFormula};
    operators::Vector{<:AbstractOperator} = AbstractOperator[],
    # additional_operators::Vector{<:AbstractOperator} = AbstractOperator[],
    args...,
)
    tree = convert(SyntaxTree, ttf)
    # ops = operators(tree)
    # operators = unique([additional_operators..., ops...])
    # props = propositions(tree)

    logic = begin
        if issubset(operators, BASE_PROPOSITIONAL_OPERATORS)
            propositional_logic(;
                operators = operators,
                args...,
            )
        elseif issubset(operators, BASE_MODAL_OPERATORS)
            modal_logic(;
                operators = operators,
                args...,
            )
        else
            error("Could not infer logic from SyntaxTree object: $(tree). Operators = $(operators).")
        end
    end
    Formula(logic, tree)
end


iscommutative(::Type{typeof(¬)}) = true
iscommutative(::Type{typeof(∧)}) = true
iscommutative(::Type{typeof(∨)}) = true
iscommutative(::Type{typeof(◊)}) = true
iscommutative(::Type{typeof(□)}) = true

function intersects(u, v)
    for x in u
        if x in v
            return true
        end
    end
    false
end
