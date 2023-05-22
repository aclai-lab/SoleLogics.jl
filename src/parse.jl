
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Table of contents ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#    TODO: This code is not so friendly.
#    A little overview about all the private methods and the workflow involved
#    in this page could be helpful to future developers.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function strip_whitespaces(
    expression::String;
    additional_whitespaces::Vector{Char} = Char[]
)
    return strip(x -> isspace(x) || x in additional_whitespaces, expression)
end

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
- the implication (→), that are assigned a `LOW_PRECEDENCE`.

In case of tie, operators are evaluated in the left-to-right order.

# Examples
```julia-repl
julia> syntaxstring(parseformulatree("¬a ∧ b ∧ c"))
"(¬(a)) ∧ (b ∧ c)"

julia> syntaxstring(parseformulatree("¬a → b ∧ c"))
"(¬(a)) → (b ∧ c)"

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

const STACK_TOKEN_TYPE = Union{<:AbstractSyntaxToken,Symbol}

const _DEFAULT_OPENING_BRACKET = "("
const _DEFAULT_CLOSING_BRACKET = ")"
const _DEFAULT_ARG_DELIM       = ","


# Special symbols: syntax tokens cannot contain these:
const DEFAULT_OPENING_BRACKET = Symbol(_DEFAULT_OPENING_BRACKET)
const DEFAULT_CLOSING_BRACKET = Symbol(_DEFAULT_CLOSING_BRACKET)
const DEFAULT_ARG_DELIM       = Symbol(_DEFAULT_ARG_DELIM)

"""
See [`parseformulatree`](@ref).
"""
const BASE_PARSABLE_OPERATORS = [
    BASE_PROPOSITIONAL_OPERATORS...,
    BASE_MODAL_OPERATORS...,
    BASE_MULTIMODAL_OPERATORS...,
] |> unique

# Check if a specific unary operator is in a valid position, during token recognition
function _check_unary_validity(
    tokens::Vector{STACK_TOKEN_TYPE},
    op::AbstractOperator,
    opening_bracket::Symbol,
    arg_separator::Symbol
)
    # A unary operator is always preceeded by some other operator or a opening_bracket
    if (arity(op) == 1 && !isempty(tokens) &&
        (tokens[end] !== opening_bracket && tokens[end] !== arg_separator &&
        !(tokens[end] isa AbstractOperator))
    )
        error("Malformed input: operator `" * syntaxstring(op) *
              "` encountered following `" *
              (tokens[end] isa Symbol ? string(tokens[end]) : syntaxstring(tokens[end])) *
              "`.")
    end
end

# Raw tokens are cutted out from the initial expression
function _recognize_tokens(
    expression::String,
    splitters::Vector{String},
    additional_whitespaces::Vector{Char}
)::Vector{String}
    potential_token = ""
    raw_tokens = String[]

    # Important: this allows to have splitters that are prefixes of other splitters.
    sort!(splitters, by=length, rev=true)

    i = 1
    while i <= sizeof(expression)
        splitter_found = false

        for splitter in splitters
            # Here, splitter might be a special sequence (a Symbol, see above)
            #  but we need to operate over string types here.
            splitter = string(splitter)
            # The longest correct splitter starting at index `i` is found (if possible)
            splitrange = findnext(splitter, expression, i)

            if (!isnothing(splitrange) && first(splitrange) == i)
                # Iterator is teleported to the next (possibly UNICODE) char
                i = nextind(expression, last(splitrange))

                # Currently loaded token is pushed: a new potential token has to be collect
                push!(raw_tokens, potential_token)
                potential_token = ""

                # Splitter is pushed: since a correct splitter is found
                #  set a flag to avoid repeating the iterator increment later.
                push!(raw_tokens, splitter)
                splitter_found = true
                break
            end
        end

        # If no splitter has been found in this cycle,
        #  then simply continue collecting a new potential splitter.
        if (!splitter_found)
            potential_token = potential_token * expression[i]
            i = nextind(expression, i)
        end
    end

    # Last potential token has to be pushed
    if (!isempty(potential_token))
        push!(raw_tokens, potential_token)
    end

    # Strings in "additional_whitespaces" are stripped out;
    #  for example, if '@' is an additional whitespace then "@p @" becomes just "p".
    raw_tokens =
        map(x -> strip_whitespaces(x, additional_whitespaces = additional_whitespaces),
        raw_tokens)
    return filter(!isempty, raw_tokens)
end

# Raw tokens are interpreted and, thus, made processable by a parser
function _interpret_tokens(
    raw_tokens::Vector{String},
    string_to_op::Dict{String,AbstractOperator},
    proposition_parser::Base.Callable;
    opening_bracket::Symbol,
    closing_bracket::Symbol,
    arg_separator::Symbol
)
    tokens = STACK_TOKEN_TYPE[]

    i = 1
    while i <= length(raw_tokens)
        tok = begin
            if (Symbol(raw_tokens[i]) in [opening_bracket, closing_bracket, arg_separator])
                # If the token is a special symbol -> push it as is
                Symbol(raw_tokens[i])
            else
                st = syntaxstring(raw_tokens[i])
                if (st in keys(string_to_op))
                    # If the token is an operator -> perform check and push it as is
                    op = string_to_op[st]
                    _check_unary_validity(tokens, op, opening_bracket, arg_separator)
                    op
                else
                    # If the token is something else -> parse as Proposition and push it
                    proposition = Proposition(proposition_parser(st))
                    # @assert proposition isa Proposition string(proposition) *
                    #     " is not a proposition. Please, provide a valid proposition_parser."
                    proposition
                end
            end
        end

        push!(tokens, tok)
        i += 1
    end

    return tokens
end

# A simple lexer capable of distinguish operators in a string.
function tokenizer(
    expression::String,
    operators::Vector{<:AbstractOperator},
    proposition_parser::Base.Callable,
    additional_whitespaces::Vector{Char},
    opening_bracket::Symbol = DEFAULT_OPENING_BRACKET,
    closing_bracket::Symbol = DEFAULT_CLOSING_BRACKET,
    arg_separator::Symbol = DEFAULT_ARG_DELIM,
)
    # Strip input's whitespaces
    expression = String(
        strip_whitespaces(expression, additional_whitespaces = additional_whitespaces))

    # Get the string representions of the given `operators`
    string_to_op = Dict([syntaxstring(op) => op for op in operators])

    # Operators whose syntaxstring is padded with spaces might cause ambiguities
    invalidops = filter(o -> syntaxstring(o) !=
        strip_whitespaces(syntaxstring(o), additional_whitespaces = additional_whitespaces),
        operators)
    @assert length(invalidops) == 0 "Cannot safely parse operators that are" *
        " prefixed/suffixed by whitespaces: " * join(invalidops, ", ")

    # Each parsing method has to know which symbols represent opening/closing a context;
    #  additionaly, parsing in function notation needs to know how arguments are separated.
    special_delimiters = vcat(opening_bracket, closing_bracket)
    if !(isnothing(arg_separator))
        push!(special_delimiters, arg_separator)
    end

    splitters = Vector{String}(vcat(string.(special_delimiters),
        collect(keys(string_to_op))))

    # Determine which tokens are separated for sure
    raw_tokens = _recognize_tokens(expression, splitters, additional_whitespaces)

    # Interpret each raw token
    return _interpret_tokens(raw_tokens, string_to_op, proposition_parser;
        opening_bracket = opening_bracket, closing_bracket = closing_bracket,
        arg_separator = arg_separator)
end

# Rearrange a serie of token, from infix to postfix notation
function shunting_yard!(
    tokens::Vector{STACK_TOKEN_TYPE};
    opening_bracket::Symbol = DEFAULT_OPENING_BRACKET,
    closing_bracket::Symbol = DEFAULT_CLOSING_BRACKET)
    tokstack = STACK_TOKEN_TYPE[] # support structure
    postfix = AbstractSyntaxToken[] # returned structure: tokens rearranged in postfix

    for tok in tokens
        if tok isa Symbol
            # If tok is a Symbol, then it might be a special parsing symbol
            if tok === opening_bracket
                # Start a new "context" in the expression
                push!(tokstack, tok)
            elseif tok === closing_bracket
                # `tokstack` shrinks and postfix vector is filled
                while !isempty(tokstack)
                    popped = pop!(tokstack)
                    if popped !== opening_bracket
                        push!(postfix, popped)
                    else
                        break
                    end
                end
            else
                error("Unexpected special symbol encountered: `$(tok)`.")
            end

        elseif tok isa AbstractOperator
            # If tok is an operator, something must be done until another operator
            #  is placed at the top of the stack.
            while !isempty(tokstack) &&
                (tokstack[end] isa AbstractOperator &&
                 Base.operator_precedence(tokstack[end]) > Base.operator_precedence(tok))
                 push!(postfix, pop!(tokstack))
            end
            # Now push the current operator onto the tokstack
            push!(tokstack, tok)

        elseif tok isa Proposition
            push!(postfix, tok)
        else
            error("Parsing error! Unexpected token type encountered: `$(typeof(tok))`.")
        end
    end

    # Consume the leftovers in the tokstack
    while !isempty(tokstack)
        popped = pop!(tokstack)

        # Starting expression is not well formatted, or a opening_bracket is found
        if !(popped isa AbstractOperator)
            error("Parsing error! Mismatching brackets detected.")
        end
        push!(postfix, popped)
    end

    return postfix
end

"""
    parseformulatree(
        expression::String,
        additional_operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing;
        function_notation::Bool = false,
        proposition_parser::Base.Callable = Proposition{String},
        additional_whitespaces::Vector{Char} = Char[],
        opening_bracket::Union{String,Symbol} = $(repr(DEFAULT_OPENING_BRACKET)),
        closing_bracket::Union{String,Symbol} = $(repr(DEFAULT_CLOSING_BRACKET)),
        arg_separator::Union{String,Symbol} = $(repr(DEFAULT_ARG_DELIM))
    )

Return a `SyntaxTree` which is the result of parsing `expression`
 via [Shunting yard](https://en.wikipedia.org/wiki/Shunting_yard_algorithm).
By default, this function is only able to parse operators in
`SoleLogics.BASE_PARSABLE_OPERATORS`; additional operators may be provided as
a second argument.

# Arguments
- `expression::String`: expression to be parsed;
- `additional_operators::Vector{<:AbstractOperator}`: additional, non-standard operators
    needed to correctly parse the expression.
    When left unset, only the following operators are
    correctly parsed: $(repr(BASE_PARSABLE_OPERATORS));
    note that, in case of clashing `syntaxstring`'s,
    the provided additional operators will override these.

# Keyword Arguments
- `function_notation::Bool = false`: if set to `true`, the expression is considered
    in function notation (e.g, `"⨁(arg1, arg2)"`);
    otherwise, it is considered in
    [infix notation](https://en.wikipedia.org/wiki/Infix_notation) (e.g, `"arg1 ⨁ arg2"`);
- `proposition_parser::Base.Callable = Proposition{String}`: a callable to be used for
    parsing propositions, once they are recognized in the expression. It must return
    the atom, or the `Proposition` itself;
- `additional_whitespaces`::Vector{Char} = Char[]: characters to be stripped out from each
    syntax token.
    For example, if `'@' in additional_whitespaces`, "¬@p@" is parsed just as "¬p".
- `opening_bracket`::Union{String,Symbol} = $(repr(DEFAULT_OPENING_BRACKET)):
    the string signaling the opening of an expression block;
- `closing_bracket`::Union{String,Symbol} = $(repr(DEFAULT_CLOSING_BRACKET)):
    the string signaling the closing of an expression block;
- `arg_separator`::Union{String,Symbol} = $(repr(DEFAULT_ARG_DELIM)):
    when `function_notation = true`,
    the string that separates the different arguments of a function call.

!!! warning
    For a proper functioning,
    the `syntaxstring` of any syntax token cannot be prefixed/suffixed by whitespaces.
    For example, for any operator `⨁`,
    it should hold that `syntaxstring(⨁) == strip(syntaxstring(⨁))`.
    Also, `syntaxstring`s cannot contain special symbols
    (`opening_bracket`, `closing_bracket`, and `arg_separator`) as
    substrings.

# Examples
```julia-repl
julia> syntaxstring(parseformulatree("¬p∧q∧(¬s∧¬z)"))
"(¬(p)) ∧ (q ∧ ((¬(s)) ∧ (¬(z))))"

julia> syntaxstring(parseformulatree("∧(¬p,∧(q,∧(¬s,¬z)))", function_notation=true))
"(¬(p)) ∧ (q ∧ ((¬(s)) ∧ (¬(z))))"

julia> syntaxstring(parseformulatree("¬1→0";
    proposition_parser = (x -> Proposition{Float64}(parse(Float64, x)))))
"(¬(1.0)) → 0.0"
```

See also [`SyntaxTree`](@ref), [`syntaxstring`](@ref).
"""
function parseformulatree(
    expression::String,
    additional_operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing;
    function_notation::Bool = false,
    proposition_parser::Base.Callable = Proposition{String},
    additional_whitespaces::Vector{Char} = Char[],
    opening_bracket::Union{String,Symbol} = DEFAULT_OPENING_BRACKET,
    closing_bracket::Union{String,Symbol} = DEFAULT_CLOSING_BRACKET,
    arg_separator::Union{String,Symbol} = DEFAULT_ARG_DELIM,
)
    additional_operators = (
        isnothing(additional_operators) ? AbstractOperator[] : additional_operators)
    operators = unique(
        AbstractOperator[BASE_PARSABLE_OPERATORS..., additional_operators...])

    # TODO: expand special sequences to special *sequences* (strings of characters)
    # TODO: check that no special sequence is a substring of another one.
    @assert function_notation ||
            (allunique(string.([opening_bracket, arg_separator])) ||
             allunique(string.([closing_bracket, arg_separator]))) "" *
        "Invalid " *
        "special sequences provided: please, check that both the `opening_bracket`" *
        " and the `closing_bracket` are not equal to the `arg_separator`."

    opening_bracket = Symbol(opening_bracket)
    closing_bracket = Symbol(closing_bracket)
    arg_separator   = Symbol(arg_separator)

    # parseformulatree workflow:
    # 1) function_notation = false; _infixbuild -> _postfixbuild
    # 2) function_notation = true;  _fxbuild    -> _prefixbuild

    # Build a formula starting from its postfix notation, preprocessed with shunting yard.
    #  In other words, all special symbols (e.g. opening_bracket) are already filtered
    #  out and only AbstractSyntaxToken are considered.
    function _postfixbuild(postfix::Vector{<:AbstractSyntaxToken})

        stack = SyntaxTree[]

        for tok in postfix
            # Stack collapses, composing a new part of the syntax tree
            if tok isa AbstractOperator
                children = [pop!(stack) for _ in 1:arity(tok)]
                push!(stack, SyntaxTree(tok, Tuple(reverse(children))))
            elseif tok isa Proposition
                push!(stack, SyntaxTree(tok))
            else
                error("Parsing error! Unexpected token type encountered: `$(typeof(tok))`.")
            end
        end

        if length(stack) != 1
            error("Malformed input when parsing expression: " *
                "`$(expression)`. (postfix: `$(postfix)`).")
        end

        return stack[1]
    end

    # Build a formula starting from its infix notation;
    # actually this is a preprocessing who fallbacks into `_postfixbuild`
    function _infixbuild()
        tokens = tokenizer(expression, operators, proposition_parser,
            additional_whitespaces, opening_bracket, closing_bracket)
        return _postfixbuild(shunting_yard!(tokens,
            opening_bracket = opening_bracket, closing_bracket = closing_bracket))
    end

    # Build a formula starting from its function notation;
    function _prefixbuild(prefix::Vector{STACK_TOKEN_TYPE})
        stack = Vector{Union{SyntaxTree, STACK_TOKEN_TYPE}}()

        for tok in reverse(prefix)
            if tok isa Symbol || tok isa Proposition
                push!(stack, tok)
            elseif tok isa AbstractOperator
                if (arity(tok) == 1 && stack[end] isa
                    Union{AbstractSyntaxToken, AbstractSyntaxStructure})
                    # If operator arity is 1, then what follows could be a single AST
                    newtok = SyntaxTree(tok, stack[end])
                    pop!(stack)
                    push!(stack, newtok)
                elseif (length(stack) >= (1 + 2*arity(tok)))
                    # Else, follow this general procedure;
                    # consider 1 opening bracket, `arity` AbstractSyntaxToken,
                    # `arity`-1 separators and 1 closing bracket for a total of
                    # 1 + (arity) + (arity-1) + 1 = (1 + 2*arity) tokens to read.
                    #
                    #   (           T      ,     T   ,   ...     ,     T        )
                    # stack[end]  end-1  end-2                           end-(1 + 2*arity)

                    # Extract needed tokens from `stack`
                    # (in other words, execute pop! (1+2*arity) times)
                    popped = reverse(stack[(length(stack) - 2*arity(tok)) : end])
                    stack = stack[1:(length(stack) - 1 - 2*arity(tok))]

                    # The following conditions must hold
                    # stack[1] == OPENING BRACKET
                    # stack[end] == CLOSING BRACKET
                    # stack[even indexes] <: AbstractSyntaxTree
                    # stack[odd indexes after 1 and before length(stack)] == SEP
                    # else an error has to be thrown

                    children =
                        [popped[s] for s in 2:length(popped) if popped[s] isa
                            Union{AbstractSyntaxToken, AbstractSyntaxStructure}]
                    separators =
                        [s for s in 3:(length(popped)-2) if popped[s] == arg_separator]

                    if (popped[1] == opening_bracket &&
                        popped[end] == closing_bracket &&
                        length(children) == arity(tok) &&
                        length(separators) == arity(tok) - 1)
                        push!(stack, SyntaxTree(tok, Tuple(children)))
                    else
                        error("Malformed expression `$(syntaxstring(tok))` followed by `" *
                        "$(popped)`.")
                    end
                end
            else
                error("Unexpected unparsable token: `$(tok)`.")
            end
        end

        if (isempty(stack))
            error("Malformed expression: parsing stack is " *
                "empty when parsing `$(expression)`.")
        end

        if (length(stack) > 1)
            error("Malformed expression: parsing stack could not interpret " *
                "`$(stack)` when parsing `$(expression)`.")
        end

        return stack[1]
    end

    # Build a formula starting from its prefix notation;
    # actually this is a preprocessing who fallbacks into `_prefixbuild`
    function _fxbuild()
        tokens = tokenizer(expression, operators, proposition_parser,
            additional_whitespaces, opening_bracket, closing_bracket, arg_separator)
        return _prefixbuild(tokens)
    end

    return (function_notation ? _fxbuild() : _infixbuild())
end

function parseformulatree(
    expression::String,
    logic::AbstractLogic;
    kwargs...
)
    parseformulatree(expression, operators(logic); kwargs...)
end

"""
    function parseformula(
        expression::String,
        additional_operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing;
        operators::Union{Nothing,Vector{<:AbstractOperator}},
        grammar::Union{Nothing,AbstractGrammar} = nothing,
        algebra::Union{Nothing,AbstractAlgebra} = nothing,
        kwargs...
    )::Formula

Return a `Formula` which is the result of parsing `expression`
 via [Shunting yard](https://en.wikipedia.org/wiki/Shunting_yard_algorithm).
By default, this function is only able to parse operators in
`SoleLogics.BASE_PARSABLE_OPERATORS`; additional operators may be provided as
a second argument.

The `grammar` and `algebra` of the associated logic is inferred using
the `baseformula` function from the operators encountered
in the expression, and those in `additional_operators`.

See [`parseformulatree`](@ref), [`baseformula`](@ref).
"""
function parseformula(
    expression::String,
    additional_operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing;
    # TODO add alphabet parameter add custom parser for propositions
    # alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
    kwargs...
)
    additional_operators =
        (isnothing(additional_operators) ? AbstractOperator[] : additional_operators)

    t = parseformulatree(expression, additional_operators; kwargs...)
    baseformula(t;
        # additional_operators = unique(AbstractOperator[operators..., SoleLogics.operators(t)...]),
        additional_operators = length(additional_operators) == 0 ? nothing :
            unique(AbstractOperator[additional_operators..., SoleLogics.operators(t)...]),
        # alphabet = alphabet,
        alphabet = AlphabetOfAny{String}(),
        grammar = grammar,
        algebra = algebra
    )
end

function parseformula(
    expression::String,
    logic::AbstractLogic;
    kwargs...,
)
    Formula(logic, parseformulatree(expression, operators(logic); kwargs...))
end

# function parseformula(
#     expression::String;
#     operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
#     kwargs...,
# )
#     parseformula(expression; additional_operators = operators, kwargs...)
# end
