export parseformula, parseformulatree

export tokenizer

using ReadableRegex

#= ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Table of contents ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    TODO: Studying this code (which has to be refactored) is not so friendly.
    A little overview about all the private methods and the workflow involved
    in this page could be helpful to future developers.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ =#


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

const BASE_PARSABLE_OPERATORS = [BASE_MODAL_OPERATORS...,
    DiamondRelationalOperator(globalrel),
    BoxRelationalOperator(globalrel),
    DiamondRelationalOperator(identityrel),
    BoxRelationalOperator(identityrel),
]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Input and construction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Characters with special meaning in expressions.
# '(' and ')' are needed to wrap a new scope
# '⟨', '⟩', '[' and ']' wraps modal operators
# ',' is ignored but might be useful to deal with more readable inputs
_parsing_special_strings = ["(", ")", "⟨", "⟩", "[", "]"]
_relation_delimeters = Dict("⟨" => "⟩", "[" => "]") #TODO: change name
_parsing_ignore_strings = [",", "", " "]

# Raw tokens are cutted out from the initial expression
function _recognize_tokens(expression::String, splitters::Vector{String})
    piece = ""
    raw_tokens = String[]

    # TODO: Come gestisco i casi in cui gli splitter sono più lunghi di un carattere?
    # Algoritmo di Gio (assert sui prefissi)
    for c in expression
        if string(c) in splitters
            push!(raw_tokens, piece)
            push!(raw_tokens, string(c))
            piece = "";
        else
            piece = piece * c;
        end
    end

    if (!isempty(piece))
        push!(raw_tokens, piece)
    end

    return filter(x->!(x in _parsing_ignore_strings), raw_tokens);
end

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

# Get a sequence of tokens until a "closing" string is found,
# return the position at which the new token has been found or throw an error;
# also, return the entire collected token: the method invoking this has
# the responsability to push the token into a specific data structure.
function _extract_token_in_context(
    opening_idx::Int,
    closing::String,
    raw_tokens::Vector{String},
    tokens::Vector{<:AbstractSyntaxToken},
    string_to_op::Dict{String, AbstractOperator}
)
    closing_idx = opening_idx
    try
        while (raw_tokens[closing_idx] != closing)
            closing_idx += 1
        end
    catch
        error("Mismatching delimeters: " * raw_tokens[opening_idx] *
            " at position " * syntaxstring(opening_idx) * " is never closed with a " * closing)
    end

    op = string_to_op[string.(raw_tokens[opening_idx:closing_idx]...)] # TODO: Maybe is syntaxstring here
    _check_unary_validity(tokens, op)

    return [closing_idx, op]
end

# Raw tokens are interpreted and, thus, processable by a parser

function _interpret_tokens(raw_tokens::Vector{String}, string_to_op::Dict{String, AbstractOperator}, proposition_parser::Base.Callable)
    tokens = SoleLogics.AbstractSyntaxToken[]

    i = 1
    while i <= length(raw_tokens)
        st = syntaxstring(raw_tokens[i])

        # token is an operator
        if (st in keys(string_to_op))
            op = string_to_op[st]
            _check_unary_validity(tokens, op)
            push!(tokens, op)

        # token is a relational operator, thus, starts with a special opening character:
        # find the position of its corresponding closing character
        # and continue the loop from here onwards.
        elseif (st in keys(_relation_delimeters))
            i, op = _extract_token_in_context(i, _relation_delimeters[st], raw_tokens,
                tokens, string_to_op)
            push!(tokens, op)

        # token is something else
        else
            proposition = proposition_parser(st)
            @assert proposition isa Proposition
            push!(tokens, proposition) # TODO: find a better name to proposition_constructor
        end

        i += 1
    end

    return tokens;
end

# A simple lexer capable of distinguish operators in a string,
# returning a Vector{SoleLogics.SyntaxTree}.
function tokenizer(expression::String, operators::Vector{<:AbstractOperator}, proposition_parser::Base.Callable)
    # Symbolic represention of given OPERATORS
    # expression = filter(x -> !isspace(x), expression)
    expression = String(strip(expression))
    string_to_op = Dict([syntaxstring(op) => op for op in operators])

    # TODO1: assert che tutti gli operators non abbiano spazi in testa o in coda

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

# TODO1: fai notare all'utente che ogni syntax token non può contenere spazi
# NOTE: la syntax string di ogni token, non deve avere spazi in testa o in coda
# questa doc deve riferirsi a syntaxstring e viceversa
# in order to be parsable, un token non deve iniziare o terminare con degli spazi

"""
    parseformulatree(expression::String, operators::Vector{<:AbstractOperator})

Returns a `SyntaxTree` which is the result from parsing `expression`.
At the moment, the propositional letters in `expression` must be represented with
 a single character (e.g., "p", "q", etc...).

# Examples
```julia-repl
julia> syntaxstring(parseformulatree("¬p∧q∧(¬s∧¬z)"))
"(¬(p)) ∧ (q ∧ ((¬(s)) ∧ (¬(z))))"
```

See also [`SyntaxTree`](@ref)
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
