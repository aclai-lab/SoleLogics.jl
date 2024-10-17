
const STACK_TOKEN_TYPE = Union{SyntaxToken,Symbol}

# Special symbols: syntax tokens cannot contain these:
const DEFAULT_OPENING_PARENTHESIS = "(" # TODO use these in `syntaxstring` as well, and add the same arguments (with same name).
const DEFAULT_CLOSING_PARENTHESIS = ")" # TODO use these in `syntaxstring` as well, and add the same arguments (with same name).
const DEFAULT_ARG_DELIM           = "," # TODO use these in `syntaxstring` as well, and add the same arguments (with same name).

"""
    const BASE_PARSABLE_CONNECTIVES = $(repr(BASE_PARSABLE_CONNECTIVES))

Vector of (standard) operators that are automatically taken care of when parsing.
These are $(join(SoleLogics.BASE_PARSABLE_CONNECTIVES, ", ", " and ")).

See also [`parseformula`](@ref).
"""
const BASE_PARSABLE_CONNECTIVES = [
    BASE_PROPOSITIONAL_CONNECTIVES...,
    BASE_MODAL_CONNECTIVES...,
    BASE_MULTIMODAL_CONNECTIVES...,
    ⊤,
    ⊥
] |> unique

doc_parseformula = """
    parseformula(expr::String, additional_operators = nothing; kwargs...)

    parseformula(
        F::Type{<:SyntaxTree},
        expr::String,
        additional_operators::Union{Nothing,AbstractVector} = nothing;
        function_notation::Bool = false,
        atom_parser::Base.Callable = Atom{String},
        additional_whitespaces::Vector{Char} = Char[],
        opening_parenthesis::String = $(repr(DEFAULT_OPENING_PARENTHESIS)),
        closing_parenthesis::String = $(repr(DEFAULT_CLOSING_PARENTHESIS)),
        arg_delim::String = $(repr(DEFAULT_ARG_DELIM))
    )::F

    parseformula(F::Type{<:Formula}, expr::String, additional_operators = nothing; kwargs...)
    parseformula(F::Type{<:SyntaxTree}, expr::String, logic::AbstractLogic; kwargs...)

Parse a formula of type `F` from a string expression (its [`syntaxstring`](@ref)).
When `F` is not specified, it defaults to `SyntaxTree`.

By default, this function is only able to parse operators in
[`SoleLogics.BASE_PARSABLE_CONNECTIVES`](@ref) (e.g.,
$(join(repr.(
    BASE_PARSABLE_CONNECTIVES[1:min(4, length(BASE_PARSABLE_CONNECTIVES))]), ", ", " and ")));
additional, non-standard operators may be provided as a vector `additional_operators`,
and their `syntaxstring`s will be used for parsing them.
Note that, in case of clashing `syntaxstring`s,
the provided additional operators will override the standard ones.

When parsing `SyntaxTree`s,
the [Shunting yard](https://en.wikipedia.org/wiki/Shunting_yard_algorithm)
algorithm is used, and the method allows the following keywords arguments.

# Keyword Arguments
- `function_notation::Bool = false`: if set to `true`, the expression is considered
    in function notation (e.g., `"⨁(arg1, arg2)"`);
    otherwise, it is considered in
    [infix notation](https://en.wikipedia.org/wiki/Infix_notation) (e.g., `"arg1 ⨁ arg2"`);
- `atom_parser::Base.Callable = Atom{String}`: a callable to be used for
    parsing atoms, once they are recognized in the expression. It must return
    the atom, or the `Atom` itself;
- `additional_whitespaces::Vector{Char} = Char[]`: characters to be stripped out from each
    syntax token.
    For example, if `'@' in additional_whitespaces`, "¬@p@" is parsed just as "¬p".
- `opening_parenthesis::String = $(repr(DEFAULT_OPENING_PARENTHESIS))`:
    the string signaling the opening of an expression block;
- `closing_parenthesis::String = $(repr(DEFAULT_CLOSING_PARENTHESIS))`:
    the string signaling the closing of an expression block;
- `arg_delim::String = $(repr(DEFAULT_ARG_DELIM))`:
    when `function_notation = true`,
    the string that delimits the different arguments of a function call.

!!! warning
    For a proper functioning,
    the `syntaxstring` of any syntax token cannot be prefixed/suffixed by whitespaces.
    For example, for any operator `⨁`,
    it should hold that `syntaxstring(⨁) == strip(syntaxstring(⨁))`.
    Also, `syntaxstring`s cannot contain special symbols
    (`opening_parenthesis`, `closing_parenthesis`, and `arg_delim`) as
    substrings.

# Examples
```julia-repl
julia> syntaxstring(parseformula("¬p∧q∧(¬s∧¬z)"))
"¬p ∧ q ∧ ¬s ∧ ¬z"

julia> syntaxstring(parseformula("∧(¬p,∧(q,∧(¬s,¬z)))", function_notation=true))
"¬p ∧ q ∧ ¬s ∧ ¬z"

julia> syntaxstring(parseformula("¬1→0"; atom_parser = (x -> Atom{Float64}(parse(Float64, x)))))
"(¬1.0) → 0.0"
```

!!! note
    For any `Formula` type `F`, this
    function should be the inverse of [`syntaxstring`](@ref);
    that is, if `φ::F` then the following should hold, for at least some `args`,
    and for every `kwargs` allowing correct parsing:
    `φ == parseformula(F, syntaxstring(φ, args...; kwargs...), args...; kwargs...)`.

See also [`SyntaxTree`](@ref), [`BASE_PARSABLE_CONNECTIVES`](@ref), [`syntaxstring`](@ref).
"""




# This is just an utility function used later
function strip_whitespaces(expr::String; additional_whitespaces::Vector{Char} = Char[])
    return strip(x -> isspace(x) || x in additional_whitespaces, expr)
end


"""$(doc_parseformula)"""
function parseformula(
    F::Type{<:SyntaxTree},
    expr::String,
    additional_operators::Union{Nothing,AbstractVector} = nothing;
    function_notation::Bool = false,
    atom_parser::Base.Callable = Atom{String},
    additional_whitespaces::Vector{Char} = Char[],
    opening_parenthesis::String = DEFAULT_OPENING_PARENTHESIS,
    closing_parenthesis::String = DEFAULT_CLOSING_PARENTHESIS,
    arg_delim::String = DEFAULT_ARG_DELIM
)::F
    additional_operators = (
        isnothing(additional_operators) ? Operator[] : additional_operators)
    @assert all(x->x isa Operator, additional_operators) "Unexpected object(s) passed" *
        " as additional operator:" *
        " $(filter(x->!(x isa Operator), additional_operators))"
    operators = Vector{Operator}(
        unique([BASE_PARSABLE_CONNECTIVES..., additional_operators...]))

    # TODO: expand special sequences to special *sequences* (strings of characters)
    # TODO: check that no special sequence is a substring of another one.
    @assert function_notation ||
        opening_parenthesis != arg_delim && closing_parenthesis != arg_delim
        "Invalid special sequences provided: " *
        "please, check that both the `opening_parenthesis` " *
        "and the `closing_parenthesis` are not equal to the `arg_delim`."

    opening_parenthesis = Symbol(opening_parenthesis)
    closing_parenthesis = Symbol(closing_parenthesis)
    arg_delim           = Symbol(arg_delim)

    # parseformula workflow:
    # 1) function_notation = false; _infixbuild -> _postfixbuild
    # 2) function_notation = true;  _fxbuild    -> _prefixbuild

    # A simple lexer capable of distinguish operators in a string.
    function tokenizer(
        expr::String,
        operators::Vector{<:Operator},
        atom_parser::Base.Callable,
        additional_whitespaces::Vector{Char},
        opening_parenthesis::Symbol = Symbol(DEFAULT_OPENING_PARENTHESIS),
        closing_parenthesis::Symbol = Symbol(DEFAULT_CLOSING_PARENTHESIS),
        arg_delim::Symbol = Symbol(DEFAULT_ARG_DELIM),
    )

        # Raw tokens are cutted out from the initial expr
        function _recognize_tokens(
            expr::String,
            splitters::Vector{String},
            additional_whitespaces::Vector{Char}
        )::Vector{String}
            potential_token = ""
            raw_tokens = String[]

            # Important: this allows to have splitters that are prefixes of other splitters.
            sort!(splitters, by=length, rev=true)

            i = 1
            while i <= sizeof(expr)
                splitter_found = false

                for splitter in splitters
                    # Here, splitter might be a special sequence (a Symbol, see above)
                    #  but we need to operate over string types here.
                    splitter = string(splitter)
                    # The longest correct splitter starting at index `i` is found (if possible)
                    splitrange = findnext(splitter, expr, i)

                    if (!isnothing(splitrange) && first(splitrange) == i)
                        # Iterator is teleported to the next (possibly UNICODE) char
                        i = nextind(expr, last(splitrange))

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
                    potential_token = potential_token * expr[i]
                    i = nextind(expr, i)
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
            string_to_op::Dict{String,<:Operator},
            atom_parser::Base.Callable;
            opening_parenthesis::Symbol,
            closing_parenthesis::Symbol,
            arg_delim::Symbol
        )

            # Check if a specific non-infix (= non-binary) operator is in a valid position, during token recognition
            function _check_noninfix_validity(
                tokens::Vector{<:STACK_TOKEN_TYPE},
                op::Operator,
                opening_parenthesis::Symbol,
                arg_delim::Symbol
            )
                # A non-infix (= non-binary) operator is always preceeded by some other operator or a opening_parenthesis
                if (arity(op) != 2 && !isempty(tokens) &&
                    (tokens[end] !== opening_parenthesis && tokens[end] !== arg_delim &&
                    !(tokens[end] isa Operator))
                )
                    error("Malformed input: operator `" * syntaxstring(op) *
                          "` encountered following `" *
                          (tokens[end] isa Symbol ? string(tokens[end]) : syntaxstring(tokens[end])) *
                          "`.")
                end
            end

            tokens = STACK_TOKEN_TYPE[]

            i = 1
            while i <= length(raw_tokens)
                tok = begin
                    if (Symbol(raw_tokens[i]) in [opening_parenthesis, closing_parenthesis, arg_delim])
                        # If the token is a special symbol -> push it as is
                        Symbol(raw_tokens[i])
                    else
                        st = syntaxstring(raw_tokens[i])
                        if (st in keys(string_to_op))
                            # If the token is an operator -> perform check and push it as is
                            op = string_to_op[st]
                            _check_noninfix_validity(tokens, op, opening_parenthesis, arg_delim)
                            op
                        else
                            # If the token is something else -> parse as Atom and push it
                            atom = Atom(atom_parser(st))
                            # @assert atom isa Atom string(atom) *
                            #     " is not an atom. Please, provide a valid atom_parser."
                            atom
                        end
                    end
                end

                push!(tokens, tok)
                i += 1
            end

            return tokens
        end

        # Strip input's whitespaces
        expr = String(
            strip_whitespaces(expr, additional_whitespaces = additional_whitespaces))

        # Get the string representions of the given `operators`
        string_to_op = Dict{String,Operator}([syntaxstring(op) => op for op in operators])

        # Operators whose syntaxstring is padded with spaces might cause ambiguities
        invalidops = filter(o -> syntaxstring(o) !=
            strip_whitespaces(syntaxstring(o), additional_whitespaces = additional_whitespaces),
            operators)
        @assert length(invalidops) == 0 "Cannot safely parse operators that are " *
            "prefixed/suffixed by whitespaces: " * join(invalidops, ", ")

        # Each parsing method has to know which symbols represent opening/closing a context;
        #  additionaly, parsing in function notation needs to know how arguments are separated.
        special_delimiters = vcat(opening_parenthesis, closing_parenthesis)
        if !(isnothing(arg_delim))
            push!(special_delimiters, arg_delim)
        end

        splitters = Vector{String}(vcat(string.(special_delimiters),
            collect(keys(string_to_op))))

        # Determine which tokens are separated for sure
        raw_tokens = _recognize_tokens(expr, splitters, additional_whitespaces)

        # Interpret each raw token
        return _interpret_tokens(raw_tokens, string_to_op, atom_parser;
            opening_parenthesis = opening_parenthesis, closing_parenthesis = closing_parenthesis,
            arg_delim = arg_delim)
    end

    # Rearrange a serie of token, from infix to postfix notation
    function shunting_yard!(
        tokens::Vector{<:STACK_TOKEN_TYPE};
        opening_parenthesis::Symbol = Symbol(DEFAULT_OPENING_PARENTHESIS),
        closing_parenthesis::Symbol = Symbol(DEFAULT_CLOSING_PARENTHESIS))
        tokstack = STACK_TOKEN_TYPE[] # support structure
        postfix = SyntaxToken[] # returned structure: tokens rearranged in postfix

        for tok in tokens
            if tok isa Symbol
                # If tok is a Symbol, then it might be a special parsing symbol
                if tok === opening_parenthesis
                    # Start a new "context" in the expr
                    push!(tokstack, tok)
                elseif tok === closing_parenthesis
                    # `tokstack` shrinks and postfix vector is filled
                    while !isempty(tokstack)
                        popped = pop!(tokstack)
                        if popped !== opening_parenthesis
                            push!(postfix, popped)
                        else
                            break
                        end
                    end
                else
                    error("Unexpected special symbol encountered: `$(tok)`.")
                end
            elseif tok isa SyntaxLeaf
                push!(postfix, tok)
            elseif tok isa Connective
                # If tok is an operator, something must be done until another operator
                #  is placed at the top of the stack.
                while !isempty(tokstack) &&
                    tokstack[end] isa Connective && (
                        precedence(tokstack[end]) > precedence(tok) ||
                        (precedence(tokstack[end]) == precedence(tok) && associativity(tokstack[end]) == :left)
                    )
                    push!(postfix, pop!(tokstack))
                end
                # Now push the current operator onto the tokstack
                push!(tokstack, tok)

            else
                error("Parsing error! Unexpected token type encountered: `$(typeof(tok))`.")
            end
        end

        # Consume the leftovers in the tokstack
        while !isempty(tokstack)
            popped = pop!(tokstack)

            # Starting expr is not well formatted, or a opening_parenthesis is found
            if !(popped isa Operator)
                error("Parsing error! Mismatching parentheses detected.")
            end
            push!(postfix, popped)
        end

        return postfix
    end

    # Build a formula starting from its postfix notation, preprocessed with shunting yard.
    #  In other words, all special symbols (e.g. opening_parenthesis) are already filtered
    #  out and only SyntaxToken are considered.
    function _postfixbuild(postfix::Vector{<:SyntaxToken})
        stack = SyntaxTree[]
        for tok in postfix
            # Stack collapses, composing a new part of the syntax tree
            if tok isa Connective
                try
                    children = [pop!(stack) for _ in 1:arity(tok)]
                    push!(stack, SyntaxTree(tok, Tuple(reverse(children))))
                catch e
                    if e isa ArgumentError
                        error("Parsing failed. " *
                        "Possible solution is to implement `precedence` for all the " *
                        "connectives. To know more about custom connectives interface, " *
                        "read the Connective documentation.")
                    else
                        rethrow(e)
                    end
                end
            elseif tok isa SyntaxLeaf
                push!(stack, tok)
            else
                error("Parsing error! Unexpected token type encountered: `$(typeof(tok))`.")
            end
        end

        stacklen = length(stack)
        if stacklen != 1
            error("Malformed input when parsing expression: " *
                "$(repr(expr)). (postfix: `$(postfix), stacklen = $(stacklen)`).")
        end

        return stack[1]
    end

    # Build a formula starting from its infix notation;
    # actually this is a preprocessing who fallbacks into `_postfixbuild`
    function _infixbuild()
        tokens = tokenizer(
            expr,
            operators,
            atom_parser,
            additional_whitespaces,
            opening_parenthesis,
            closing_parenthesis,
        )
        return _postfixbuild(
            shunting_yard!(tokens,
                opening_parenthesis = opening_parenthesis,
                closing_parenthesis = closing_parenthesis
            )
        )
    end

    # Build a formula starting from its function notation;
    # note that here, differently from the _postfixbuild case, operators associativity is
    # already covered by the function notation parenthesization.
    function _prefixbuild(prefix::Vector{<:STACK_TOKEN_TYPE})
        stack = Vector{Union{SyntaxTree,STACK_TOKEN_TYPE}}()

        for tok in reverse(prefix)
            if tok isa Symbol || tok isa SyntaxLeaf
                push!(stack, tok)
            elseif tok isa Connective
                if (arity(tok) == 1 && stack[end] isa SyntaxTree)
                    # If operator arity is 1, then what follows could be a single AST
                    newtok = SyntaxTree(tok, stack[end])
                    pop!(stack)
                    push!(stack, newtok)
                elseif (length(stack) >= (1 + 2*arity(tok)))
                    # Else, follow this general procedure;
                    # consider 1 opening parenthesis, `arity` SyntaxToken,
                    # `arity`-1 delims and 1 closing parenthesis for a total of
                    # 1 + (arity) + (arity-1) + 1 = (1 + 2*arity) tokens to read.
                    #
                    #   (           T      ,     T   ,   ...     ,     T        )
                    # stack[end]  end-1  end-2                           end-(1 + 2*arity)

                    # Extract needed tokens from `stack`
                    # (in other words, execute pop! (1+2*arity) times)
                    popped = reverse(stack[(length(stack) - 2*arity(tok)) : end])
                    stack = stack[1:(length(stack) - 1 - 2*arity(tok))]

                    # The following conditions must hold
                    # stack[1] == OPENING PARENTHESIS
                    # stack[end] == CLOSING PARENTHESIS
                    # stack[even indexes] <: AbstractSyntaxTree
                    # stack[odd indexes after 1 and before length(stack)] == SEP
                    # else an error has to be thrown

                    children =
                        [popped[s] for s in 2:length(popped) if popped[s] isa SyntaxTree]
                    delims =
                        [s for s in 3:(length(popped)-2) if popped[s] == arg_delim]

                    if (popped[1] == opening_parenthesis &&
                        popped[end] == closing_parenthesis &&
                        length(children) == arity(tok) &&
                        length(delims) == arity(tok) - 1)
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
                "empty when parsing $(repr(expr)).")
        end

        if (length(stack) > 1)
            error("Malformed expression: parsing stack could not interpret " *
                "`$(stack)` when parsing $(repr(expr)).")
        end

        return stack[1]
    end

    # Build a formula starting from its prefix notation;
    # actually this is a preprocessing who fallbacks into `_prefixbuild`
    function _fxbuild()
        tokens = tokenizer(expr, operators, atom_parser,
            additional_whitespaces, opening_parenthesis, closing_parenthesis, arg_delim)
        return _prefixbuild(tokens)
    end

    return (function_notation ? _fxbuild() : _infixbuild())
end

function parseformula(
    F::Type{<:SyntaxTree},
    expr::String,
    g::AbstractGrammar;
    kwargs...
)
    parseformula(F, expr, operators(g); kwargs...)
end

function parseformula(
    F::Type{<:SyntaxTree},
    expr::String,
    logic::AbstractLogic;
    kwargs...
)
    parseformula(F, expr, operators(logic); kwargs...)
end
