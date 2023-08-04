(A) Make an mauro/outreach branch {cm:2023-08-04}

(A) Rename general.jl in core.jl {cm:2023-08-04}

(C) Establish the trait is_right_associative (defaulted to false, but true for implication)
    Fix parsing algorithm accordingly 
        # When child are popped out of the token stack, if associativity is left, then the AST grows to the left.
        # For example, a ~ b ~ c becomes (a ~ b) ~ c instead of (right associativity case) a ~ (b ~ c)
    Update AbstractOperator implementation dosctring, in general.jl.
        # The user must know everything about how to create a custom operator. 
        Show how to create a custom operator - maybe a Xor, using ⊻ or ⊕ - starting from scratch.

(C) Move Base.operator_precedence docstring and definitions from parse.jl to AbstractOperator interface, in general.jl

(C) Enrich syntaxstring examples 
    Each parameter has to be considered atleast once (atm, remove_redundant_parentheses has to be added)

(A) refactor this file {cm:2023-08-04}