export parseformula

export tokenizer

using ReadableRegex

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Precedence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Atleast 3 degrees of priority can be distinguished:
#
# HIGH_PRIORITY = 15 (this value is Base.operator_precedence(:^))
# BASE_PRIORITY = 12 (this value is Base.operator_precedence(:*))
# LOW_PRIORITY  = 11 (this value is Base.operator_precedence(:+))
#
# Consider the following pairs (operator, priority):
#
# (!, HIGH_PRIORITY), (∧, BASE_PRIORITY), (=>, LOW_PRIORITY),
#
# then the expression "!a => b ∧ c" is evaluated as "(!a) => (b ∧ c)"

doc_priority = """
    Standard integer representing a precedence.
    High numbers take precedence over low numbers.
    This is needed to establish unambiguous implementations of parsing-related algorithms.
"""

"""$(doc_priority)"""
HIGH_PRIORITY = Base.operator_precedence(:^)

"""$(doc_priority)"""
BASE_PRIORITY = Base.operator_precedence(:*)

"""$(doc_priority)"""
LOW_PRIORITY  = Base.operator_precedence(:+)

Base.operator_precedence(::AbstractOperator) = BASE_PRIORITY
Base.operator_precedence(::typeof(NEGATION)) = HIGH_PRIORITY

# "a∧b → c∧d" is parsed "(a∧b) → (c∧d)" instead of "a ∧ (b→c) ∧ d"
Base.operator_precedence(::typeof(IMPLICATION)) = LOW_PRIORITY

# TODO: place DIAMOND and BOX precedences here from modal-logic.jl

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Input and construction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A simple lexer capable of distinguish operators in a string.
# Returns a Vector{SoleLogics.SyntaxTree}
function tokenizer(expression::String, operators::Vector{<:NamedOperator})
    # Symbolic represention of given OPERATORS
    expression = filter(x -> !isspace(x), expression)
    symops = Symbol.(operators)

    # Collection responsible for split `expression` in the correct points.
    splitter = vcat(["(", ")"], String.(symops))

    # NOTE: at the moment this code only works with single-char long variables.
    # For example "my_long_name1 ∧ my_long_name2" is not parsed correctly;
    # This happens because the following split behaves like a split(expression, "")
    split(expression, Regex(
            either(
                [look_for("", before=sep) for sep in splitter]...,
                [look_for("", after=sep) for sep in splitter]...,

                look_for("", before = "("),
                look_for("", after = "("),

                look_for("", before = ")"),
                look_for("", after = ")")
            )
        )
    )

    # Vector{SoleLogics.SyntaxToken}
    return [Symbol(st) in symops ?
        NamedOperator{Symbol(st)}() :
        Proposition(st)
        for st in expression
    ]
end

# Build a formula starting from a Vector{SyntaxToken} representing its postfix notation
function buildformula(postfix::Vector{SyntaxToken})
    stack = SyntaxTree[]

    # Each tok might be a Proposition or a NamedOperator
    for tok in postfix
        # Stack collapses, composing a new part of the syntax tree
        if tok isa NamedOperator
            children = [pop!(stack) for _ in 1:arity(tok)]
            push!(stack, SyntaxTree(tok, Tuple(children)))
        else
            push!(stack, SyntaxTree(tok))
        end
    end
    println(stack)
    return stack[1]
end

"""
    parseformula(expression::String, operators::Vector{<:NamedOperator})

Return a `SyntaxTree` starting from `expression`.
TODO: `operators` could be defaulted to a certain, exhaustive, set of `NamedOperators`'s.

See also [`SyntaxTree`](@ref)
"""
function parseformula(expression::String, operators::Vector{<:NamedOperator})
    tokens = tokenizer(expression, operators) # Still a Vector{SoleLogics.SyntaxToken}

    # Stack containing operators. Needed to transform the expression in postfix notation;
    # opstack may contain Proposition("("), Proposition(")") and NamedOperators
    opstack = Vector{SoleLogics.SyntaxToken}([])
    postfix = Vector{SoleLogics.SyntaxToken}([])

    shunting_yard(tokens, opstack, postfix)

    # Consume the leftovers in the opstack
    while !isempty(opstack)
        op = pop!(opstack)

        # Starting expression is not well formatted, or a "(" is found
        if !(op isa NamedOperator)
            throw(error("Mismatching brackets"))
        end
        push!(postfix, op)
    end

    return buildformula(postfix)
end

function shunting_yard(
    tokens::Vector{SoleLogics.SyntaxToken},
    opstack::Vector{SoleLogics.SyntaxToken},
    postfix::Vector{SoleLogics.SyntaxToken}
)
    for tok in tokens

        # tok is an operator, something must be done until another operator
        # is placed at the top of the stack.
        if tok isa NamedOperator
            while !isempty(opstack) &&
                (opstack[end] isa NamedOperator &&
                Base.operator_precedence(opstack[end]) > Base.operator_precedence(tok))

                push!(postfix, pop!(opstack))
            end
            # Now push the current operator onto the opstack
            push!(opstack, tok)

        # Start a new "context" in the expression
        elseif atom(tok) === '('
            push!(opstack, tok)

        # opstack shrinkens and postfix vector is filled
        elseif atom(tok) === ')'
            while !isempty(opstack)
                op = pop!(opstack)
                if op isa NamedOperator || atom(op) != '('
                    push!(postfix, op)
                end
            end

        # tok is certainly a Proposition
        else
            push!(postfix, tok)
        end
    end
end

#= Repl fast test
ops = [NEGATION, CONJUNCTION]
expr = "¬a∧b∧(¬c∧¬d)"
parseformula(expr,ops)
=#
