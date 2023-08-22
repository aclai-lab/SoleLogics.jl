In README.md
    Check examples (see NOTE or pluto-demo.jl)
    List of similar packages, highlightning SoleLogics use cases

Rename things to increase readability, using @deprecate in a deprecate.jl file.

See PAndQ
    Write the algorithm that produces truth tables of propositional formulae; also, recognize if a formula is satisfiable, is a contingency, a tautology or a contraddiction.
    Macro to easily create atoms

Fix SatsBase.sample errors

Write check's docstring

Write random models generation (see pluto-demo.jl)

Normalization
    Add custom normalization rules to normalize
    Simplify code when possible, using dual/hasdual traits

In the documentation, show how to create a custom operator (maybe a Xor/⊻/⊕) starting from scratch.

(A) Make an mauro/outreach branch {cm:2023-08-04}

(A) Rename general.jl in core.jl {cm:2023-08-04}

(C) Establish the trait is_right_associative (defaulted to false, but true for implication) {cm:2023-08-06}
    Fix parsing algorithm accordingly {cm:2023-08-06}
        # When child are popped out of the token stack, if associativity is left, then the AST grows to the left.
        # For example, a ~ b ~ c becomes (a ~ b) ~ c instead of (right associativity case) a ~ (b ~ c)
    Update AbstractOperator implementation dosctring, in general.jl. {cm:2023-08-06}

(C) Move Base.operator_precedence docstring and definitions from parse.jl to AbstractOperator interface, in core.jl {cm:2023-08-06}

(C) Enrich syntaxstring examples. Each parameter has to be considered atleast once (atm, remove_redundant_parentheses has to be added) {cm:2023-08-06}

(A) refactor this file {cm:2023-08-04}