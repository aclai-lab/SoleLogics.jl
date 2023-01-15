export parseformula, parseformulatree

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
const HIGH_PRIORITY = Base.operator_precedence(:^)

"""$(doc_priority)"""
const BASE_PRIORITY = Base.operator_precedence(:*)

"""$(doc_priority)"""
const LOW_PRIORITY  = Base.operator_precedence(:+)

function Base.operator_precedence(op::AbstractOperator)
    if isunary(op)
        HIGH_PRIORITY
    else
        BASE_PRIORITY
    end
end

# "a∧b → c∧d" is parsed "(a∧b) → (c∧d)" instead of "a ∧ (b→c) ∧ d"
Base.operator_precedence(::typeof(IMPLICATION)) = LOW_PRIORITY

const BASE_PARSABLE_OPERATORS = [BASE_MODAL_OPERATORS...]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Input and construction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A simple lexer capable of distinguish operators in a string,
# returning a Vector{SoleLogics.SyntaxTree}.
function tokenizer(expression::String, operators::Vector{<:AbstractOperator})
    # Symbolic represention of given OPERATORS
    expression = filter(x -> !isspace(x), expression)
    string_to_op = Dict([string(op) => op for op in operators])

    # Collection responsible for split `expression` in the correct points.
    splitter = ["(", ")", keys(string_to_op)...]

    # NOTE: at the moment, this code only works with single-char long variables.
    # For example "my_long_name1 ∧ my_long_name2" is not parsed correctly;
    # this happens because the following split behaves like a split(expression, "").
    # A macro should be created to dynamically generate a look_for("", before="some_string")
    # for each element in spliter.
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

    # Trick: wrap chars like '(' and 'p' into Proposition{String}'s. shunting_yard will
    #  take care of this.
    return SoleLogics.AbstractSyntaxToken[string(st) in keys(string_to_op) ?
        string_to_op[string(st)] :
        Proposition{String}(string(st))
        for st in expression
    ]
end

# Rearrange a serie of token, from infix to postfix notation.
# Tokens are consumed from `tokens` in order to fill `postfix` and `opstack`.
function shunting_yard!(
    tokens::Vector{SoleLogics.AbstractSyntaxToken},
    opstack::Vector{SoleLogics.AbstractSyntaxToken},
    postfix::Vector{SoleLogics.AbstractSyntaxToken}
)
    for tok in tokens

        # tok is an operator, something must be done until another operator
        # is placed at the top of the stack.
        if tok isa AbstractOperator
            while !isempty(opstack) &&
                (opstack[end] isa AbstractOperator &&
                Base.operator_precedence(opstack[end]) > Base.operator_precedence(tok))
                push!(postfix, pop!(opstack))
            end
            # Now push the current operator onto the opstack
            push!(opstack, tok)

        # Start a new "context" in the expression
        elseif atom(tok) === "("
            push!(opstack, tok)

        # opstack shrinkens and postfix vector is filled
        elseif atom(tok) === ")"
            while !isempty(opstack)
                op = pop!(opstack)
                if op isa AbstractOperator || atom(op) != "("
                    push!(postfix, op)
                end
            end

        # tok is certainly a Proposition
        else
            push!(postfix, tok)
        end
    end
end

# Build a formula starting from a Vector{AbstractSyntaxToken} representing its postfix notation
function buildformulatree(postfix::Vector{AbstractSyntaxToken})
    stack = SyntaxTree[]

    # Each tok might be a Proposition or a AbstractOperator
    for tok in postfix
        # Stack collapses, composing a new part of the syntax tree
        if tok isa AbstractOperator
            children = [pop!(stack) for _ in 1:arity(tok)]
            push!(stack, SyntaxTree(tok, Tuple(reverse(children))))
        else
            push!(stack, SyntaxTree(tok))
        end
    end

    if length(stack) != 1
        throw(error("Malformed input"))
    end

    return stack[1]
end

"""
    parseformulatree(expression::String, operators::Vector{<:AbstractOperator})

Returns a `SyntaxTree` which is the result from parsing `expression`.
At the moment, the propositional letters in `expression` must be represented with
 a single character (e.g., "p", "q", etc...).

# Examples
```julia-repl
julia> parseformulatree("¬p∧q∧(¬s∧¬z)")
∧(∧(∧(¬(z), ¬(s)), q), ¬(p))
```

See also [`SyntaxTree`](@ref)
"""
function parseformulatree(
    expression::String,
    operators::Vector{<:AbstractOperator} = AbstractOperator[],
)
    operators = unique(AbstractOperator[BASE_PARSABLE_OPERATORS..., operators...])
    tokens = tokenizer(expression, operators) # Still a Vector{SoleLogics.AbstractSyntaxToken}

    # Stack containing operators. Needed to transform the expression in postfix notation;
    # opstack may contain Proposition("("), Proposition(")") and operators
    opstack = Vector{SoleLogics.AbstractSyntaxToken}([])
    postfix = Vector{SoleLogics.AbstractSyntaxToken}([])

    shunting_yard!(tokens, opstack, postfix)

    # Consume the leftovers in the opstack
    while !isempty(opstack)
        op = pop!(opstack)

        # Starting expression is not well formatted, or a "(" is found
        if !(op isa AbstractOperator)
            throw(error("Mismatching brackets"))
        end
        push!(postfix, op)
    end

    return buildformulatree(postfix)
end

function parseformula(
    expression::String;
    # TODO add alphabet parameter add custom parser for propositions
    # alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
)
    operators = (isnothing(operators) ? AbstractOperator[] : operators)
    t = parseformulatree(expression, operators)
    base_formula(t;
        operators = unique(AbstractOperator[operators..., SoleLogics.operators(t)...]),
        # alphabet = alphabet,
        alphabet = AlphabetOfAny{String}(),
        grammar = grammar,
        algebra = algebra,
    )
end

function parseformula(
    expression::String,
    logic::AbstractLogic,
)
    Formula(parseformulatree(expression, operatorstype(logic)), logic)
end

function parseformula(
    expression::String,
    operators::Union{Nothing,Vector{<:AbstractOperator}};
    args...,
)
    parseformula(expression; operators = operators, args...)
end
