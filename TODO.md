□ refactor this file

□ establish the trait is_right_associative (defaulted to false, but true for implication)
    □ look at operator associativity table (in C)
    □ fix parsing accordingly
    □ update AbstractOperator implementation dosctring, in general.jl

□ move Base.operator_precedence docstring and definitions from parse.jl to AbstractOperator interface, in general.jl

□ in syntaxstring, parameter remove_redundant_parentheses (with examples)
