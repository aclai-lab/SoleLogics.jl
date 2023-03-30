export parseformula, parseformulatree

export tokenizer

using ReadableRegex

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Table of contents ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#    TODO: Studying this code (which has to be refactored) is not so friendly.
#    A little overview about all the private methods and the workflow involved
#    in this page could be helpful to future developers.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Precedence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

doc_priority = """
    Standard integer representing a precedence.
    High numbers take precedence over low numbers.
    This is needed to establish unambiguous implementations of parsing-related algorithms.

    
    Consider the following pairs (operator, priority):
    
    (¬, HIGH_PRIORITY), (∧, BASE_PRIORITY), (⟹, LOW_PRIORITY),
    
    then the expression "¬a ⟹ b ∧ c" is interpreted as "¬(a) ⟹ (b ∧ c)"

    "a∧b → c∧d" is parsed "(a∧b) → (c∧d)" instead of "a ∧ (b→c) ∧ d"

"""

# At least 3 degrees of priority can be distinguished:

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
Base.operator_precedence(::typeof(IMPLICATION)) = LOW_PRIORITY

const BASE_PARSABLE_OPERATORS = [BASE_MODAL_OPERATORS...,
    DiamondRelationalOperator(globalrel),
    BoxRelationalOperator(globalrel),
    DiamondRelationalOperator(identityrel),
    BoxRelationalOperator(identityrel),
]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Input and construction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Characters with special meaning in expressions.
# '(' and ')' are needed to wrap a new scope
# ',' is ignored but might be useful to deal with more readable inputs
_parsing_special_strings = ["(", ")"]
_parsing_ignored_strings = [",", ""]
_default_proposition_parser = Proposition{String}

# Check if a specific unary operator is in a valid position, during tokens recognition
function _check_unary_validity(tokens::Vector{<:AbstractSyntaxToken}, op::AbstractOperator)
    # A unary operator is always preceeded by some other operator or a '('
    if (arity(op) == 1 &&
        !isempty(tokens) &&
        (syntaxstring(tokens[end]) != "(" && !(tokens[end] isa AbstractOperator))
    )
        error("Malformed input: " * syntaxstring(op) * " is following " * syntaxstring(tokens[end]))
    end
end

# Raw tokens are cutted out from the initial expression
function _recognize_tokens(expression::String, splitters::Vector{String})
    potential_token = ""
    raw_tokens = String[]

    sort!(splitters, by=length, rev=true)

    i = 1
    while i <= sizeof(expression)
        splitter_found = false

        for splitter in splitters
            # The lenghtiest, correct splitter starting at index 'i' is found (if possible)
            splitrange = findnext(splitter, expression, i)

            if (!isnothing(splitrange) && first(splitrange) == i)
                # Iterator is teleported to the next (possibly UNICODE) char
                i = nextind(expression, last(splitrange))

                # Currently loaded token is pushed: a new potential token has to be collect
                push!(raw_tokens, potential_token)
                potential_token = ""

                # Splitter is pushed: since a correct splitter is found
                # set a flag to avoid repeating the iterator increment later
                push!(raw_tokens, splitter)
                splitter_found = true
                break
            end
        end

        # If no splitter has been found in this cycle,
        # then simply continue collecting a new potential splitter
        if (!splitter_found)
            potential_token = potential_token * expression[i];
            i = nextind(expression, i)
        end
    end

    # Last potential token has to be pushed
    if (!isempty(potential_token))
        push!(raw_tokens, potential_token)
    end

    # Particular special characters are ignored (see _parsing_ignore_strings);
    # strip is added to avoid accepting concatenations of  " ":
    # it is possible to have meaningful spaces in an expression (e.g [My operator]
    # different from [MyOperator]) but here we are working on isolated presences of
    # operators we want to ignore.
    return filter(x -> !(strip(x) in _parsing_ignored_strings), raw_tokens);
end

# Raw tokens are interpreted and, thus, made processable by a parser
function _interpret_tokens(
    raw_tokens::Vector{String},
    string_to_op::Dict{String, AbstractOperator},
    proposition_parser::Base.Callable
)
    tokens = SoleLogics.AbstractSyntaxToken[]

    i = 1
    while i <= length(raw_tokens)
        st = syntaxstring(raw_tokens[i])

        # token is an operator
        if (st in keys(string_to_op))
            op = string_to_op[st]
            _check_unary_validity(tokens, op)
            push!(tokens, op)

        # token is a special string
        elseif st in _parsing_special_strings
            # NOTE: this is a trick to distinguish parentheses (special parsing characters).
            # TODO: change this to work with Symbol types and modify shunting yard accordingly.
            push!(tokens, _default_proposition_parser(st))

        # token is something else
        else
            proposition = proposition_parser(st)
            @assert proposition isa Proposition string(proposition) *
                " is not a proposition. Please, provide a valid proposition_parser."
            push!(tokens, proposition)
        end

        i += 1
    end

    return tokens;
end

# A simple lexer capable of distinguish operators in a string,
# returning a Vector{SoleLogics.SyntaxTree}.
function tokenizer(
    expression::String,
    operators::Vector{<:AbstractOperator},
    proposition_parser::Base.Callable
)
    # Symbolic represention of given OPERATORS
    # expression = filter(x -> !isspace(x), expression)
    expression = String(strip(expression))
    string_to_op = Dict([syntaxstring(op) => op for op in operators])

    # operators whose syntaxstring is padded with spaces are invalid
    # since they might cause an ambiguous parse.
    invalidops = filter(o -> syntaxstring(o) != strip(syntaxstring(o)), operators)
    @assert length(invalidops) == 0 "Certain operators are identified by a name starting" *
        " or ending with a space. Please, fix typos in the following operators: " *
        join(invalidops, ", ")

    splitters = vcat(_parsing_special_strings, keys(string_to_op)...)
    raw_tokens = String.(strip.(_recognize_tokens(expression, splitters)))
    return _interpret_tokens(raw_tokens, string_to_op, proposition_parser);
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

# TODO: in the following docstring, in `additional_operators` description,
#   we have to write about what BASE_PARSABLE_OPERATORS are set by default.
# TODO: in the following docstring, Examples section should be expanded to consider
#   different use cases of `proposition_parser`
# TODO: `proposition_parser` name (who was previously called `proposition_parser`) is ugly
"""
    parseformulatree(
        expression::String,
        additional_operators::Vector{<:AbstractOperator} = AbstractOperator[];
        proposition_parser::Base.Callable = Proposition{String} # Proposition
    )

Returns a `SyntaxTree` which is the result from parsing `expression`.

!!! warning
    Each additional_operators' `syntaxstring` must not contain spaces padding.
    For example, if ⨁ is an AbstractOperator syntaxstring(⨁) = "  ⨁ " is invalid.

# Arguments
- `expression::String`: expression to be parsed
- `additional_operators::Vector{<:AbstractOperator}` additional non-standard operators
    needed to correctly parse expression.
- `proposition_parser::Base.Callable = Proposition{String}`: constructor needed to
    interpret propositions recognized in expression.

# Examples
```julia-repl
julia> syntaxstring(parseformulatree("¬p∧q∧(¬s∧¬z)"))
"(¬(p)) ∧ (q ∧ ((¬(s)) ∧ (¬(z))))"
```

See also [`SyntaxTree`](@ref), [`syntaxstring`](@ref).
"""
function parseformulatree(
    expression::String,
    additional_operators::Vector{<:AbstractOperator} = AbstractOperator[];
    proposition_parser::Base.Callable = Proposition{String} # Proposition
)
    # Build a formula starting from a Vector{AbstractSyntaxToken} representing its postfix notation
    function _buildformulatree(postfix::Vector{AbstractSyntaxToken})
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
            error("Malformed input: $(expression) (postfix: $(postfix))")
        end

        return stack[1]
    end

    operators = unique(AbstractOperator[BASE_PARSABLE_OPERATORS..., additional_operators...])
    tokens = tokenizer(expression, operators, proposition_parser) # Still a Vector{SoleLogics.AbstractSyntaxToken}

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
            error("Mismatching brackets")
        end
        push!(postfix, op)
    end

    return _buildformulatree(postfix)
end

# TODOs:
# - Parametro function_notation = false,
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
    baseformula(t;
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

# Done TODOs
# Space paddings
#   ☑ @assert about operators whose syntaxstring contains space padding
#   ☑ parseformulatree docstring revised;
#   ☑ warning section in syntaxstring docstring, to advise against
#       syntaxstrings containing space padding
#
# Logic
#   ☑ `_relation_delimeters` variable removed: is no longer needed
#   ☑ `_extract_token_in_context` removed: is no longer needed
#   ☑ `_recognize_tokens` is now smart enough to interpret this snippet correctly:
#
#       struct MyCustomRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
#       (MyCustomRelationalOperator)(r::AbstractRelation) = MyCustomRelationalOperator{typeof(r)}()
#       SoleLogics.syntaxstring(op::MyCustomRelationalOperator; kwargs...) = "LEFT CUSTOM BRACKET $(syntaxstring(relationtype(op);  kwargs...)) RIGHT CUSTOM BRACKET"
#
#       f = parseformulatree("LEFT CUSTOM BRACKET G RIGHT CUSTOM BRACKET p ∧ ¬ LEFT CUSTOM BRACKET G RIGHT CUSTOM BRACKET q", [MyCustomRelationalOperator(globalrel)])
#
# ☑ Tests
#
# Minor changes
#   ☑ proposition_parser changed to proposition_parser (mhhh)
#   ☑ @assert about invalid `proposition_parser` selections, now contains a string
