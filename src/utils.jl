function base_formula(
    ttf::Union{SyntaxToken,SyntaxTree,AbstractFormula};
    operators::Vector{<:AbstractOperator} = AbstractOperator[],
    # additional_operators::Vector{<:AbstractOperator} = AbstractOperator[],
    args...,
)
    tree = convert(SyntaxTree, ttf)
    # ops = operators(tree)
    # operators = unique([additional_operators..., ops...])
    # props = propositions(tree)

    logic = begin
        if issubset(operators, base_propositional_operators)
            propositional_logic(;
                operators = operators,
                args...,
            )
        elseif issubset(operators, base_modal_operators)
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
