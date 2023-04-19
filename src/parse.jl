export parseformula, parseformulatree

export tokenizer

using ReadableRegex

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Table of contents ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#    TODO: Studying this code (which has to be refactored) is not so friendly.
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

const STACK_TOKEN_TYPE = Union{<:AbstractSyntaxToken, Symbol}

const _OPENING_BRACKET = "("
const _CLOSING_BRACKET = ")"
const _ARG_DELIM       = ","


# Special symbols: syntax tokens cannot contain these:
const OPENING_BRACKET = Symbol(_OPENING_BRACKET)
const CLOSING_BRACKET = Symbol(_CLOSING_BRACKET)
const ARG_DELIM       = Symbol(_ARG_DELIM)

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
    arg_separator::Union{Symbol,Nothing}
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
            # Here, splitter might be a special character (a Symbol, see above)
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
    arg_separator::Union{Symbol,Nothing}
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
                    proposition = proposition_parser(st)
                    @assert proposition isa Proposition string(proposition) *
                        " is not a proposition. Please, provide a valid proposition_parser."
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
    opening_bracket::Symbol = OPENING_BRACKET,
    closing_bracket::Symbol = CLOSING_BRACKET,
    arg_separator::Union{Symbol,Nothing} = nothing
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
function shunting_yard!(tokens::Vector{STACK_TOKEN_TYPE};
    opening_bracket::Symbol = OPENING_BRACKET,
    closing_bracket::Symbol = CLOSING_BRACKET)
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
                error("Unexpected special symbol encountered: $(tok).")
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
            error("Parsing error! Unexpected token type encountered: $(typeof(tok)).")
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
        opening_bracket::Symbol = $(OPENING_BRACKET),
        closing_bracket::Symbol = $(CLOSING_BRACKET),
        arg_separator::Union{Symbol,Nothing} = $(ARG_DELIM)
    )

Returns a `SyntaxTree` which is the result of parsing `expression`
 via [Shunting yard](https://en.wikipedia.org/wiki/Shunting_yard_algorithm).
By default, this function is only able to parse operators in
`SoleLogics.BASE_PARSABLE_OPERATORS`; additional operators may be provided as
parameter `additional_operators`.

# Arguments
- `expression::String`: expression to be parsed;
- `additional_operators::Vector{<:AbstractOperator}:` additional, non-standard operators
    needed to correctly parse the expression; in case of clashing `syntaxstring`'s,
    the provided additional operators will override the base parsable ones.

# Keyword Arguments
- `function_notation::Bool = false`: if set to `true`, the expression is considered
    in function notation (e.g, ⨁(arg1, arg2));
    otherwise, it is considered in
    [infix notation](https://en.wikipedia.org/wiki/Infix_notation) (e.g, arg1 ⨁ arg2);
- `proposition_parser::Base.Callable = Proposition{String}`: a callable to be used when
    parsing `Proposition`s, once they are recognized in the expression;
- `additional_whitespaces`::Vector{Char} = Char[]: characters to be stripped out from each
    syntax token. For example, if '@' is added, "¬@p@" is parsed just as "¬p".
- `opening_bracket`::Symbol = $(OPENING_BRACKET):
    the string signaling the opening of anexpression block;
- `closing_bracket`::Symbol = $(CLOSING_BRACKET):
    the string signaling the closing of anexpression block;
- `arg_separator`::Union{Symbol,Nothing} = $(ARG_DELIM): when `function_notation = true`,
    the string that separates the different arguments of a function call.

!!! warning
    For a proper functioning,
    operators' `syntaxstring`'s must not be prefixed/suffixed by whitespaces. Essentially,
    for any operator ⨁, it must hold that `syntaxstring(⨁) == strip(syntaxstring(⨁))`.
    Also, special symbols (`opening_bracket`, `closing_bracket` and `arg_separator`) must
    be all different from each other and CAN NOT compare in tokens (e.g, "pro(position" will
    never be interpreted as an atomic proposition).

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
    opening_bracket::Symbol = OPENING_BRACKET,
    closing_bracket::Symbol = CLOSING_BRACKET,
    arg_separator::Union{Symbol,Nothing} = ARG_DELIM
)
    additional_operators = (
        isnothing(additional_operators) ? AbstractOperator[] : additional_operators)
    operators = unique(
        AbstractOperator[BASE_PARSABLE_OPERATORS..., additional_operators...])

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
                error("Parsing error! Unexpected token type encountered: $(typeof(tok)).")
            end
        end

        if length(stack) != 1
            error("Malformed input: expression: `$(expression)`. (postfix: `$(postfix)`).")
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
                if (arity(tok) == 1 && typeof(stack[end]) <:
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
                        [popped[s] for s in 2:length(popped) if typeof(popped[s]) <:
                            Union{AbstractSyntaxToken, AbstractSyntaxStructure}]
                    separators =
                        [s for s in 3:(length(popped)-2) if popped[s] == arg_separator]

                    if (popped[1] == opening_bracket &&
                        popped[end] == closing_bracket &&
                        length(children) == arity(tok) &&
                        length(separators) == arity(tok) - 1)
                        push!(stack, SyntaxTree(tok, Tuple(children)))
                    else
                        error("Malformed expression $(syntaxstring(tok)) followed by " *
                        "$(popped).")
                    end
                end
            else
                error("Unexpected unparsable token: $(tok).")
            end
        end

        if (isempty(stack))
            error("Malformed expression: parsing stack is empty.")
        end

        if (length(stack) > 1)
            error("Malformed expression: parsing stack could not interpret $(stack).")
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
    function_notation::Bool = false,
    proposition_parser::Base.Callable = Proposition{String},
    additional_whitespaces::Vector{Char} = Char[],
    opening_bracket::Symbol = OPENING_BRACKET,
    closing_bracket::Symbol = CLOSING_BRACKET,
    arg_separator::Union{Symbol,Nothing} = nothing
)
    parseformulatree(
        expression,
        operators(logic),
        function_notation = function_notation,
        proposition_parser = proposition_parser,
        additional_whitespaces = additional_whitespaces,
        opening_bracket = opening_bracket,
        closing_bracket = closing_bracket,
        arg_separator = arg_separator,
    )
end

```
TODO
```
function parseformula(
    expression::String;
    # TODO add alphabet parameter add custom parser for propositions
    # alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    additional_operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
    function_notation::Bool = false,
    proposition_parser::Base.Callable = Proposition{String},
    additional_whitespaces::Vector{Char} = Char[],
    opening_bracket::Union{String, Symbol} = OPENING_BRACKET,
    closing_bracket::Union{String, Symbol} = CLOSING_BRACKET,
    arg_separator::Union{String, Symbol,Nothing} = nothing
)
    additional_operators =
        (isnothing(additional_operators) ? AbstractOperator[] : additional_operators)

    opening_bracket = Symbol(opening_bracket)
    closing_bracket = Symbol(closing_bracket)
    arg_separator   = Symbol(arg_separator)

    t = parseformulatree(expression, additional_operators;
        function_notation = function_notation,
        proposition_parser = proposition_parser,
        additional_whitespaces = additional_whitespaces,
        opening_bracket = opening_bracket,
        closing_bracket = closing_bracket,
        arg_separator = arg_separator)
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

function parseformula(
    expression::String,
    operators::Union{Nothing,Vector{<:AbstractOperator}};
    kwargs...,
)
    parseformula(expression; additional_operators = operators, kwargs...)
end

# Working on...
# ☑ make some new "strip_whitespaces" function
# ☑ function notation parsing it's working
# ☑ OPENING_BRACKET, CLOSING_BRACKET, SEPARATOR as arguments
# ☑ comments refactoring
# ☑ parseformulatree docstring updated
#   ☑ written about limitations in warning, but maybe there's more to safely
#   ☑ info about left-right operator precedence in case of tie (precedence docstrings)
#   □ in official documentation, user has to be informed about BASE_PARSABLE_OPERATORS

# 82ad8c2570d8e69162edad822d591ebff5edb5a4:
# ☑ allow the user to specify a string instead of a symbol for a special character.
#   Symbols are only used internally.
#   See _OPENING_BRACKET, _CLOSING_BRACKET, _ARG_DELIM above
#   NOTE: user can now choose to use string type OR symbol.
#   Should Symbols be completely removed from parseformulatree interface instead?
# ☑ shunting_yard critical bug solve
# ☑ more tests added
# ☑ make tests silent
#   □ everything works, but actually the cryptic error thrown by _randformulatree
#   now is also thrown by a specific test (see test/parse.jl) where parseformulatree
#   is called with a long expression string as first argument.
#   I'm investigating... see (@)

#= (@)
f = parseformulatree("⟨G⟩(((¬(⟨G⟩((q ∧ p) → (¬(q))))) ∧ (((¬(q → q)) → ((q → p) → (¬(q))))"*
           "∧ (((¬(p)) ∧ (⟨G⟩(p))) → (¬(⟨G⟩(q)))))) ∧ ((¬(([G](p ∧ q)) → (¬(p → q)))) →" *
           "([G](([G](q∧ q)) ∧ ([G](q → p))))))",
           [BoxRelationalOperator(globalrel), DiamondRelationalOperator(globalrel)])
Internal error: encountered unexpected error in runtime:
BoundsError(a=Type{Union{SoleLogics.NamedOperator{Symbol("¬")}, SoleLogics.NamedOperator{:→}, SoleLogics.Proposition{String}}}, i=1)
ijl_bounds_error at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/rtutils.c:146
get_fieldtype at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/builtins.c:1106
tmerge at ./compiler/typelimits.jl:468
abstract_call_gf_by_type at ./compiler/abstractinterpretation.jl:169
abstract_call_known at ./compiler/abstractinterpretation.jl:1716
abstract_call at ./compiler/abstractinterpretation.jl:1786
abstract_apply at ./compiler/abstractinterpretation.jl:1357
abstract_call_known at ./compiler/abstractinterpretation.jl:1620
abstract_call at ./compiler/abstractinterpretation.jl:1786
abstract_call at ./compiler/abstractinterpretation.jl:1753
abstract_eval_statement at ./compiler/abstractinterpretation.jl:1910
typeinf_local at ./compiler/abstractinterpretation.jl:2386
typeinf_nocycle at ./compiler/abstractinterpretation.jl:2482
_typeinf at ./compiler/typeinfer.jl:230
typeinf at ./compiler/typeinfer.jl:213
typeinf_edge at ./compiler/typeinfer.jl:877
abstract_call_method at ./compiler/abstractinterpretation.jl:647
abstract_call_gf_by_type at ./compiler/abstractinterpretation.jl:139
abstract_call_known at ./compiler/abstractinterpretation.jl:1716
abstract_call at ./compiler/abstractinterpretation.jl:1786
abstract_call at ./compiler/abstractinterpretation.jl:1753
abstract_eval_statement at ./compiler/abstractinterpretation.jl:1910
typeinf_local at ./compiler/abstractinterpretation.jl:2360
typeinf_nocycle at ./compiler/abstractinterpretation.jl:2482
_typeinf at ./compiler/typeinfer.jl:230
typeinf at ./compiler/typeinfer.jl:213
typeinf_edge at ./compiler/typeinfer.jl:877
abstract_call_method at ./compiler/abstractinterpretation.jl:647
abstract_call_gf_by_type at ./compiler/abstractinterpretation.jl:139
abstract_call at ./compiler/abstractinterpretation.jl:1784
abstract_call at ./compiler/abstractinterpretation.jl:1753
abstract_eval_statement at ./compiler/abstractinterpretation.jl:1910
typeinf_local at ./compiler/abstractinterpretation.jl:2386
typeinf_nocycle at ./compiler/abstractinterpretation.jl:2482
_typeinf at ./compiler/typeinfer.jl:230
typeinf at ./compiler/typeinfer.jl:213
typeinf_edge at ./compiler/typeinfer.jl:877
abstract_call_method at ./compiler/abstractinterpretation.jl:647
abstract_call_gf_by_type at ./compiler/abstractinterpretation.jl:139
abstract_call_known at ./compiler/abstractinterpretation.jl:1716
abstract_call at ./compiler/abstractinterpretation.jl:1786
abstract_call at ./compiler/abstractinterpretation.jl:1753
abstract_eval_statement at ./compiler/abstractinterpretation.jl:1910
typeinf_local at ./compiler/abstractinterpretation.jl:2386
typeinf_nocycle at ./compiler/abstractinterpretation.jl:2482
_typeinf at ./compiler/typeinfer.jl:230
typeinf at ./compiler/typeinfer.jl:213
typeinf_edge at ./compiler/typeinfer.jl:877
abstract_call_method at ./compiler/abstractinterpretation.jl:647
abstract_call_gf_by_type at ./compiler/abstractinterpretation.jl:139
abstract_call_known at ./compiler/abstractinterpretation.jl:1716
abstract_call at ./compiler/abstractinterpretation.jl:1786
abstract_call at ./compiler/abstractinterpretation.jl:1753
abstract_eval_statement at ./compiler/abstractinterpretation.jl:1910
typeinf_local at ./compiler/abstractinterpretation.jl:2386
typeinf_nocycle at ./compiler/abstractinterpretation.jl:2482
_typeinf at ./compiler/typeinfer.jl:230
typeinf at ./compiler/typeinfer.jl:213
typeinf_edge at ./compiler/typeinfer.jl:877
abstract_call_method at ./compiler/abstractinterpretation.jl:647
abstract_call_gf_by_type at ./compiler/abstractinterpretation.jl:139
abstract_call_known at ./compiler/abstractinterpretation.jl:1716
abstract_call at ./compiler/abstractinterpretation.jl:1786
abstract_call at ./compiler/abstractinterpretation.jl:1753
abstract_eval_statement at ./compiler/abstractinterpretation.jl:1910
typeinf_local at ./compiler/abstractinterpretation.jl:2386
typeinf_nocycle at ./compiler/abstractinterpretation.jl:2482
_typeinf at ./compiler/typeinfer.jl:230
typeinf at ./compiler/typeinfer.jl:213
typeinf_ext at ./compiler/typeinfer.jl:967
typeinf_ext_toplevel at ./compiler/typeinfer.jl:1000
typeinf_ext_toplevel at ./compiler/typeinfer.jl:996
jfptr_typeinf_ext_toplevel_17539.clone_1 at /home/mauro/.julia/julia_exec/julia-1.8.5/lib/julia/sys.so (unknown line)
_jl_invoke at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2377 [inlined]
ijl_apply_generic at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2559
jl_apply at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/julia.h:1843 [inlined]
jl_type_infer at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:315
jl_generate_fptr_impl at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/jitlayers.cpp:319
jl_compile_method_internal at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2091 [inlined]
jl_compile_method_internal at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2035
_jl_invoke at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2369 [inlined]
ijl_apply_generic at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2559
SyntaxTree at /home/mauro/Desktop/UNI/Laboratorio/Sole/SoleLogics.jl/src/general.jl:506
_jl_invoke at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2377 [inlined]
ijl_apply_generic at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2559
_postfixbuild at /home/mauro/Desktop/UNI/Laboratorio/Sole/SoleLogics.jl/src/parse.jl:412
_infixbuild at /home/mauro/Desktop/UNI/Laboratorio/Sole/SoleLogics.jl/src/parse.jl:432 [inlined]
#parseformulatree#155 at /home/mauro/Desktop/UNI/Laboratorio/Sole/SoleLogics.jl/src/parse.jl:511
parseformulatree at /home/mauro/Desktop/UNI/Laboratorio/Sole/SoleLogics.jl/src/parse.jl:382
unknown function (ip: 0x7fa310f3ea86)
_jl_invoke at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2377 [inlined]
ijl_apply_generic at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2559
jl_apply at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/julia.h:1843 [inlined]
do_call at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/interpreter.c:126
eval_value at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/interpreter.c:215
eval_stmt_value at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/interpreter.c:166 [inlined]
eval_body at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/interpreter.c:612
jl_interpret_toplevel_thunk at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/interpreter.c:750
top-level scope at REPL[6]:1
jl_toplevel_eval_flex at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/toplevel.c:906
jl_toplevel_eval_flex at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/toplevel.c:850
jl_toplevel_eval_flex at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/toplevel.c:850
ijl_toplevel_eval_in at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/toplevel.c:965
eval at ./boot.jl:368 [inlined]
eval_user_input at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/usr/share/julia/stdlib/v1.8/REPL/src/REPL.jl:151
repl_backend_loop at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/usr/share/julia/stdlib/v1.8/REPL/src/REPL.jl:247
start_repl_backend at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/usr/share/julia/stdlib/v1.8/REPL/src/REPL.jl:232
#run_repl#47 at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/usr/share/julia/stdlib/v1.8/REPL/src/REPL.jl:369
run_repl at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/usr/share/julia/stdlib/v1.8/REPL/src/REPL.jl:355
jfptr_run_repl_65104.clone_1 at /home/mauro/.julia/julia_exec/julia-1.8.5/lib/julia/sys.so (unknown line)
_jl_invoke at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2377 [inlined]
ijl_apply_generic at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2559
#967 at ./client.jl:419
jfptr_YY.967_33139.clone_1 at /home/mauro/.julia/julia_exec/julia-1.8.5/lib/julia/sys.so (unknown line)
_jl_invoke at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2377 [inlined]
ijl_apply_generic at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2559
jl_apply at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/julia.h:1843 [inlined]
jl_f__call_latest at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/builtins.c:774
#invokelatest#2 at ./essentials.jl:729 [inlined]
invokelatest at ./essentials.jl:726 [inlined]
run_main_repl at ./client.jl:404
exec_options at ./client.jl:318
_start at ./client.jl:522
jfptr__start_38041.clone_1 at /home/mauro/.julia/julia_exec/julia-1.8.5/lib/julia/sys.so (unknown line)
_jl_invoke at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2377 [inlined]
ijl_apply_generic at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/gf.c:2559
jl_apply at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/julia.h:1843 [inlined]
true_main at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/jlapi.c:575
jl_repl_entrypoint at /cache/build/default-amdci4-2/julialang/julia-release-1-dot-8/src/jlapi.c:719
main at julia (unknown line)
unknown function (ip: 0x7fa327029d8f)
__libc_start_main at /lib/x86_64-linux-gnu/libc.so.6 (unknown line)
unknown function (ip: 0x401098)
SyntaxTree: ⟨G⟩(((¬(⟨G⟩((q ∧ p) → (¬(q))))) ∧ (((¬(q → q)) → ((q → p) → (¬(q)))) ∧ (((¬(p)) ∧ (⟨G⟩(p))) → (¬(⟨G⟩(q)))))) ∧ ((¬(([G](p ∧ q)) → (¬(p → q)))) → ([G](([G](q ∧ q)) ∧ ([G](q → p))))))
Allowed token types: Union{BoxRelationalOperator{GlobalRel}, DiamondRelationalOperator{GlobalRel}, SoleLogics.NamedOperator{:∧}, SoleLogics.NamedOperator{:¬}, SoleLogics.NamedOperator{:→}, Proposition{String}}
=#
