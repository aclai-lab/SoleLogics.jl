export parseformula, parseformulatree

export tokenizer

using ReadableRegex

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Table of contents ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#    TODO: Studying this code (which has to be refactored) is not so friendly.
#    A little overview about all the private methods and the workflow involved
#    in this page could be helpful to future developers.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Precedence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# At least 3 levels of operator precedence can be distinguished:
#
# HIGH_PRECEDENCE = 15 (this value is Base.operator_precedence(:^))
# BASE_PRECEDENCE = 12 (this value is Base.operator_precedence(:*))
# LOW_PRECEDENCE  = 11 (this value is Base.operator_precedence(:+))

doc_precedence = """
    const HIGH_PRECEDENCE = Base.operator_precedence(:^)
    const BASE_PRECEDENCE = Base.operator_precedence(:*)
    const LOW_PRECEDENCE  = Base.operator_precedence(:+)

Standard integers representing operator precedence;
operators with high values take precedence over operators with lower values.
This is needed to establish unambiguous implementations of parsing-related algorithms.

By default, all operators are assigned a `BASE_PRECEDENCE`, except for:
- unary operators (e.g., ¬, ◊), that are assigned a `HIGH_PRECEDENCE`;
- the implication (⟹), that are assigned a `LOW_PRECEDENCE`.

In case of tie, operators are evaluated in the left-to-right order.

# Examples
```julia-repl
julia> julia> syntaxstring(parseformulatree("a ∧ b ∨ c"))
"a ∧ (b ∨ c)"

# TODO this is wrong.
julia> syntaxstring(parseformulatree("¬a ⟹ b ∧ c"))
"(¬(a ⟹ b)) ∧ c"

julia> syntaxstring(parseformulatree("a∧b → c∧d"))
"(a ∧ b) → (c ∧ d)"
```

See also [`parseformulatree`](@ref).
"""

"""$(doc_precedence)"""
const HIGH_PRECEDENCE = Base.operator_precedence(:^)
"""$(doc_precedence)"""
const BASE_PRECEDENCE = Base.operator_precedence(:*)
"""$(doc_precedence)"""
const LOW_PRECEDENCE  = Base.operator_precedence(:+)

function Base.operator_precedence(op::AbstractOperator)
    if isunary(op)
        HIGH_PRECEDENCE
    else
        BASE_PRECEDENCE
    end
end

Base.operator_precedence(::typeof(IMPLICATION)) = LOW_PRECEDENCE

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Input and construction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Characters with special meaning in expressions.
# '(' and ')' are needed to wrap a new scope
# ',' is ignored but might be useful to deal with more readable inputs

# TODO make these a parameter of the parsing algorithm.
const OPENING_BRACKET = ("(")
const CLOSING_BRACKET = (")")

_parsing_special_strings = [OPENING_BRACKET, CLOSING_BRACKET]
_parsing_ignored_strings = [",", ""] # TODO make this a parsing argument

const BASE_PARSABLE_OPERATORS = [
    BASE_MODAL_OPERATORS...,
    DiamondRelationalOperator(globalrel),
    BoxRelationalOperator(globalrel),
    DiamondRelationalOperator(identityrel),
    BoxRelationalOperator(identityrel),
]


# Check if a specific unary operator is in a valid position, during token recognition
function _check_unary_validity(
    tokens::Vector{<:AbstractSyntaxToken},
    op::AbstractOperator,
)
    # A unary operator is always preceeded by some other operator or a OPENING_BRACKET
    if (arity(op) == 1 &&
        !isempty(tokens) &&
        (syntaxstring(tokens[end]) != OPENING_BRACKET && !(tokens[end] isa AbstractOperator))
    )
        error("Malformed input! Operator `" * syntaxstring(op) *
            "` encountered following `" * syntaxstring(tokens[end]) * "`.")
    end
end

# Raw tokens are cutted out from the initial expression
function _recognize_tokens(
    expression::String,
    splitters::Vector{String},
)
    potential_token = ""
    raw_tokens = String[]

    # Important: this allows to have splitters that are prefixes of other splitters.
    sort!(splitters, by=length, rev=true)

    i = 1
    while i <= sizeof(expression)
        splitter_found = false

        for splitter in splitters
            # The longest correct splitter starting at index `i` is found (if possible)
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

    # Special characters are ignored (see _parsing_ignore_strings);
    # strip is added to avoid accepting concatenations of whitespaces:
    # it is possible to have meaningful spaces in an expression (e.g.,
    # two different operators "[My operator]" and "[MyOperator]")
    # but here we are working on isolated presences of
    # operators we want to ignore.
    return filter(x -> !(strip(x) in _parsing_ignored_strings), raw_tokens);
end

# Raw tokens are interpreted and, thus, made processable by a parser
function _interpret_tokens(
    raw_tokens::Vector{String},
    string_to_op::Dict{String,AbstractOperator},
    proposition_parser::Base.Callable,
)
    tokens = AbstractSyntaxToken[]

    i = 1
    while i <= length(raw_tokens)
        st = syntaxstring(raw_tokens[i])

        tok = begin
            if (st in keys(string_to_op))
                # If the token is an operator -> perform check and push it as is
                op = string_to_op[st]
                _check_unary_validity(tokens, op)
                op
            elseif st in _parsing_special_strings
                # If the token is a special string -> push it
                # NOTE: wrap special strings into Proposition{String}`s.
                #  shunting_yard will take care of this.
                # TODO: use Symbol(st) instead, and modify shunting yard accordingly.
                Proposition{String}(st)
            else
                # If the token is something else -> parse as Proposition and push it
                proposition = proposition_parser(st)
                @assert proposition isa Proposition string(proposition) *
                    " is not a proposition. Please, provide a valid proposition_parser."
                proposition
            end
        end

        push!(tokens, tok)

        i += 1
    end

    return tokens;
end

# A simple lexer capable of distinguish operators in a string.
function tokenizer(
    expression::String,
    operators::Vector{<:AbstractOperator},
    proposition_parser::Base.Callable,
)
    # Strip whitespaces
    expression = String(strip(expression))
    # Get the string representions of the given `operators`
    string_to_op = Dict([syntaxstring(op) => op for op in operators])

    # Note: operators whose syntaxstring is padded with spaces are invalid
    # since they might cause ambiguities.
    invalidops = filter(o -> syntaxstring(o) != strip(syntaxstring(o)), operators)
    @assert length(invalidops) == 0 "Cannot safely parse operators that are" *
        " prefixed/suffixed by whitespaces: " * join(invalidops, ", ")

    # Note: everything that is not a special string or an operator
    #  will be parsed as a Proposition.
    splitters = vcat(_parsing_special_strings, collect(keys(string_to_op)))
    raw_tokens = String.(strip.(_recognize_tokens(expression, splitters)))
    return _interpret_tokens(raw_tokens, string_to_op, proposition_parser);
end

# Rearrange a serie of token, from infix to postfix notation.
# Elements from `tokens` are consumed in order to fill `postfix` and `opstack`.
function shunting_yard!(
    tokens::Vector{<:AbstractSyntaxToken},
    opstack::Vector{<:AbstractSyntaxToken},
    postfix::Vector{<:AbstractSyntaxToken},
)
    for tok in tokens
        if tok isa AbstractOperator
            # If tok is an operator, something must be done until another operator
            # is placed at the top of the stack.
            while !isempty(opstack) &&
                (opstack[end] isa AbstractOperator &&
                Base.operator_precedence(opstack[end]) > Base.operator_precedence(tok))
                push!(postfix, pop!(opstack))
            end
            # Now push the current operator onto the opstack
            push!(opstack, tok)

        elseif atom(tok) === OPENING_BRACKET
            # Start a new "context" in the expression
            push!(opstack, tok)

        elseif atom(tok) === CLOSING_BRACKET
            # `opstack` shrinks and postfix vector is filled
            while !isempty(opstack)
                op = pop!(opstack)
                if op isa AbstractOperator || atom(op) != OPENING_BRACKET
                    push!(postfix, op)
                end
            end

        elseif tok isa Proposition
            push!(postfix, tok)
        else
            error("Parsing error! Unexpected token type encountered: $(typeof(tok)).")
        end
    end
end

# TODO: in the following docstring, in `additional_operators` description,
#   we have to write about what BASE_PARSABLE_OPERATORS are set by default.
# TODO: in the following docstring, Examples section should be expanded to consider
#   different use cases of `proposition_parser`
# TODO do and explain overwrite
# TODO special characters should not appear in the tokens.
# TODO: Parameter function_notation = false,
# TODO function_notation only affects the syntaxstring of binary operators. Ternary operators always rely on ","
"""
    parseformulatree(
        expression::String,
        additional_operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing;
        proposition_parser::Base.Callable = Proposition{String} # Proposition
    )

Returns a `SyntaxTree` which is the result of parsing `expression`.
By default, this function is only able to parse operators in
`SoleLogics.BASE_PARSABLE_OPERATORS`; additional operators may be provided as
parameter `additional_operators`. In case of clashing syntaxstring's, the provided
additional operators will override the base parsable ones.

!!! warning
    Operators' `syntaxstring`'s must not be prefixed/suffixed by whitespaces. Essentially,
    for any operator ⨁, it must hold that `syntaxstring(⨁) == strip(syntaxstring(⨁))`.

# Arguments
- `expression::String`: expression to be parsed;
- `additional_operators::Vector{<:AbstractOperator}` additional non-standard operators
    needed to correctly parse expression;

# Keyword Arguments
- `proposition_parser::Base.Callable = Proposition{String}`: a callable function to use in
    order to build propositions from strings recognized in the expression.

# Examples
```julia-repl
julia> syntaxstring(parseformulatree("¬p∧q∧(¬s∧¬z)"))
"(¬(p)) ∧ (q ∧ ((¬(s)) ∧ (¬(z))))"
```

See also [`SyntaxTree`](@ref), [`syntaxstring`](@ref).
"""
function parseformulatree(
    expression::String,
    additional_operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing;
    proposition_parser::Base.Callable = Proposition{String},
)
    additional_operators = (isnothing(additional_operators) ? AbstractOperator[] : additional_operators)

    # Build a formula starting from its postfix notation
    function _buildformulatree(postfix::Vector{<:AbstractSyntaxToken})
        stack = SyntaxTree[]

        for tok in postfix
            # Stack collapses, composing a new part of the syntax tree
            if tok isa AbstractOperator
                children = [pop!(stack) for _ in 1:arity(tok)]
                push!(stack, SyntaxTree(tok, Tuple(reverse(children))))
            elseif tok isa Proposition
                push!(stack, SyntaxTree(tok))
            else
                error("Parsing error! Unexpected token type encountered: $(typeof(tok)).")
            end
        end

        if length(stack) != 1
            error("Malformed input! Expression: `$(expression)`. (postfix: `$(postfix)`).")
        end

        return stack[1]
    end

    operators = unique(AbstractOperator[BASE_PARSABLE_OPERATORS..., additional_operators...])
    tokens = tokenizer(expression, operators, proposition_parser)

    # Stack containing operators. Needed to transform the expression in postfix notation;
    # opstack may contain Proposition(OPENING_BRACKET), Proposition(CLOSING_BRACKET) and operators
    opstack = AbstractSyntaxToken[]
    postfix = AbstractSyntaxToken[]

    shunting_yard!(tokens, opstack, postfix)

    # Consume the leftovers in the opstack
    while !isempty(opstack)
        op = pop!(opstack)

        # Starting expression is not well formatted, or a OPENING_BRACKET is found
        if !(op isa AbstractOperator)
            error("Parsing error! Mismatching brackets detected.")
        end
        push!(postfix, op)
    end

    return _buildformulatree(postfix)
end

function parseformulatree(
    expression::String,
    logic::AbstractLogic,
)
    parseformulatree(expression, operators(logic))
end

function parseformula(
    expression::String;
    # TODO add alphabet parameter add custom parser for propositions
    # alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    additional_operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
)
    additional_operators = (isnothing(additional_operators) ? AbstractOperator[] : additional_operators)
    t = parseformulatree(expression, additional_operators)
    baseformula(t;
        # additional_operators = unique(AbstractOperator[operators..., SoleLogics.operators(t)...]),
        additional_operators = length(additional_operators) == 0 ? nothing : unique(AbstractOperator[additional_operators..., SoleLogics.operators(t)...]),
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
    Formula(logic, parseformulatree(expression, operators(logic)))
end

function parseformula(
    expression::String,
    operators::Union{Nothing,Vector{<:AbstractOperator}};
    args...,
)
    parseformula(expression; additional_operators = operators, args...)
end
