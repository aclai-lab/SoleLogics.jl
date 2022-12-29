export parseformula

export tokenizer

using ReadableRegex

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Input and construction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A simple lexer capable of distinguish operators in a string.
# Returns a Vector{SoleLogics.SyntaxTree}
function tokenizer(expression::String, operators::Vector{<:NamedOperator})
    # Symbolic represention of given operators
    symops = Symbol.(operators)

    # Collection responsible for split `expression` in the correct points.
    splitter = vcat(["(", ")"], String.(symops))

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
function buildformula()
    return
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

    # There are 4 possible cases
    for tok in tokens

        # tok is an operator, something must be done until another operator
        # is placed at the top of the stack.
        if tok isa NamedOperator
            while !isempty(opstack) &&
                (opstack[end] isa NamedOperator &&
                Base.operator_precedence(opstack[end]) >= Base.operator_precedence(tok))

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

    # Consume the leftovers in the opstack
    while !isempty(opstack)
        op = pop!(opstack)

        # Starting expression is not well formatted, or a "(" is found
        if !(op isa NamedOperator)
            throw(error("Mismatching brackets"))
        end
        push!(postfix, op)
    end

    return postfix
end

#= REPL TESTING
ops = [NEGATION, CONJUNCTION]
expr = "¬a∧b∧(¬c∧¬d)"
parseformula(expr,ops)
=#

#=
"""
    shunting_yard(expression::String)
Return `expression` in postfix notation.
"""
function shunting_yard(expression::String; logic::AbstractLogic=DEFAULT_LOGIC)
    postfix = Union{AbstractOperator,String}[]
    opstack = Stack{Union{AbstractOperator,String}}() # This contains operators or "("

    tokens = tokenizer(expression, ops = operators(logic))
    for tok in tokens
        _shunting_yard(postfix, opstack, tok, logic)
    end

    # Remaining tokens are pushed to postfix
    while !isempty(opstack)
        op = pop!(opstack)
        if op == "("
            throw(error("Mismatching brackets."))
        end
        push!(postfix, op)
    end

    return postfix
end

function _check_operator_validity(op, logic::AbstractLogic)
    if !(typeof(op) <: AbstractOperator) || !(op in operators(logic))
        throw(error("Operator $op is not legal for the specified logic $(typeof(logic))"))
    end
end

function _shunting_yard(postfix, opstack, tok, logic::AbstractLogic)
    # 2
    if tok == "("
        push!(opstack, tok)
    # 3
    elseif tok == ")"
        while !isempty(opstack) && (op = pop!(opstack)) != "("
            push!(postfix, op)
        end
    # 1 (tok is certainly a propositional letter)
    elseif typeof(tok) <: String
        push!(postfix, tok)
    # 4 (tok is certainly an operator)
    else
        while !isempty(opstack)
            if first(opstack) == "("
                break
            end

            op = pop!(opstack)  # This is not an "(", so it must be an operator
            _check_operator_validity(op, logic)

            if precedence(op) > precedence(tok)
                push!(postfix, op)
            else
                # Last pop is reverted since `tok` has to be pushed in `opstack` now.
                push!(opstack, op)
                break
            end
        end

        _check_operator_validity(tok, logic)
        push!(opstack, tok)
    end
end

#=
Formula (syntax) tree generation

Given a certain token `tok`, 1 of 3 possible scenarios may occur:
(regrouped in _parseformula function to keep code clean)

1. `tok` is an unary operator
    -> make a new FNode(tok), then link it with the FNode popped from `nodestack` top.
    Then push the new FNode into `nodestack`.

2. It is a binary operator
    -> analogue to step 2., but 2 nodes are popped and linked to the new FNode.

3. `tok` is a propositional letter, hence a leaf in the formula tree
    -> push a new FNode(tok) in the nodestack;

At the end, the only remaining FNode in `nodestack`
is the root of the formula (syntax) parseformula.
=#

"""
    parseformula(expression::Vector{Union{String,AbstractOperator}})
Return a formula-tree from its corresponding postfix-notation string.
Each propositional letter (tree's leaves) is a `SoleLogics.Letter`, with every
field set to `nothing`.

    parseformula(expression::Vector{<:Any})
Return a formula-tree forcing the cast of `expression`
into a `Vector{Union{MetaLetter, AbstractOperator}}`.

    parseformula(expression::String)
Return a formula-tree directly from an infix-notation string.
"""
function parseformula(
    expression::Vector{Union{MetaLetter, AbstractOperator}};
    logic::AbstractLogic=DEFAULT_LOGIC
)
    nodestack = Stack{FNode}()

    # This is needed to avoid memory waste repeating identical leaves.
    # We know for sure that each String in `_candidates` will become a `Letter`.
    _candidates = [s for s in expression if typeof(s) <: MetaLetter]
    letter_sentinels = Dict{MetaLetter, FNode(logic)}(_candidates .=> [FNode(x, logic=logic) for x in _candidates])

    for tok in expression
        _parseformula(tok, nodestack, logic, letter_sentinels)
    end

    SoleLogics.size!(first(nodestack))
    return Formula(first(nodestack))
end

function parseformula(expression::Vector{<:Any}; logic::AbstractLogic=DEFAULT_LOGIC)
    parseformula(convert(Vector{Union{MetaLetter, AbstractOperator}}, expression), logic=logic)
end

function parseformula(expression::String; logic::AbstractLogic=DEFAULT_LOGIC)
    parseformula(shunting_yard(expression, logic=logic), logic=logic)
end

function _parseformula(
    tok,
    nodestack,
    logic::AbstractLogic,
    letter_sentinels::Dict{MetaLetter, FNode{L}}
) where {L <: AbstractLogic}
    # Case 1 or 2
    if typeof(tok) <: AbstractOperator
        __parseformula(Val(ariety(tok)), tok, nodestack, logic)
    # Case 3
    # Identical propositional letters are not repeated,
    # but leaves works as "sentinels" instead, therefore, not wasting memory.
    elseif haskey(letter_sentinels, tok)
        newnode = letter_sentinels[tok]
        newnode.formula = string(tok)
        push!(nodestack, newnode)
    else
        throw(error("Unknown token $tok for the specified logic."))
    end
end

function __parseformula(::Val{1}, tok::AbstractOperator, nodestack, logic::AbstractLogic)
    newnode = FNode(tok, logic=logic)
    children = pop!(nodestack)

    SoleLogics.parent!(children, newnode)
    rightchild!(newnode, children)
    newnode.formula = string(tok, children.formula)

    push!(nodestack, newnode)
end

function __parseformula(::Val{2}, tok,nodestack,logic::AbstractLogic)
    newnode = FNode(tok, logic=logic)
    right_child = pop!(nodestack)
    left_child = pop!(nodestack)

    SoleLogics.parent!(right_child, newnode)
    SoleLogics.parent!(left_child, newnode)
    rightchild!(newnode, right_child)
    leftchild!(newnode, left_child)
    newnode.formula = string("(", left_child.formula, tok, right_child.formula, ")")

    push!(nodestack, newnode)
end

import SoleLogics: precedence
SoleLogics.precedence(letter::AbstractPropositionalLetter) = hash(letter)

"""
    fnormalize!(fx::Formula)
    fnormalize!(v::FNode)
Manipulate e formula (syntax) tree to follow a standard established order
between propositional letters and operators.

# Examples
```jldoctest
julia> ft = parseformula("(b∧a)∨(d∧c)")
(((b)∧(a))∨((d)∧(c)))
julia> ft = fnormalize!(ft)
julia> ft
(((a)∧(b))∨((c)∧(d)))
```
"""
function fnormalize!(fx::Formula{L}) where {L<:AbstractLogic}
    fnormalize!(tree(fx))
end

#=
parseformula("(b∧a)∨(d∧c)")
parseformula("(d∧c)∨(a∧b)")
Find a method to collapse those in the same formula
=#
function fnormalize!(v::FNode{L}) where {L<:AbstractLogic}
    if isleaf(v)
        return
    elseif is_commutative(token(v))
        left_child = leftchild(v)
        right_child = rightchild(v)
        if !is_less(token(left_child), token(right_child))
            rightchild!(v, left_child)
            leftchild!(v, right_child)
        end
    end

    if isdefined(v, :leftchild)
        fnormalize!(leftchild(v))
    end
    if isdefined(v, :rightchild)
        fnormalize!(rightchild(v))
    end
end

# Comparison function to normalize a formula.
function is_less(
    a::T1,
    b::T2,
) where {T1<:Union{<:AbstractPropositionalLetter,<:AbstractOperator},T2<:Union{<:AbstractPropositionalLetter,<:AbstractOperator}}
    return precedence(a) <= precedence(b) ? true : false
end

#= Part of the following code has already been rewritten in random.jl

############################################################################################
#       Formula random
#         generation
############################################################################################
"""
    gen_formula(
        height::Integer,
        P::Vector{<:AbstractPropositionalLetter};
        C::Operators=SoleLogics.operators(MODAL_LOGIC),
        max_modepth::Integer=height,
        pruning_factor::Float64=0.0,
        rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG
    )
Return a formula having the exact specified `height`.

# Arguments
- `height::Integer`: final height of the generated tree.
- `P::Vector{<:AbstractPropositionalLetter}`: pool of valid propositional letters,
    candidates to be leaves.
- `C::Operators`: pool of valid operators, candidates to be internal nodes.
- `max_modepth::Integer`: maximum number of modal operators in a path.
- `pruning_factor::Float64`: float number between 0.0 and 1.0.
    This correspond to the probability of "stop" the function at each step.
    It's useful to randomly prune the generated tree between the specified
    `height` and 0.
- `rng::Union{Integer,AbstractRNG}`: an rng, or the seed to initialize one.
"""
function gen_formula(
    height::Integer,
    P::Vector{<:AbstractPropositionalLetter};
    C::Operators=SoleLogics.operators(MODAL_LOGIC),
    max_modepth::Integer=height,
    pruning_factor::Float64=0.0,
    rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG
)
    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng
    fx = parseformula(
        _gen_formula(
            height,
            P,
            C,
            modal_depth = max_modepth,
            pruning_factor = pruning_factor,
            rng = rng,
        ),
    )
    return fx
end

"""
    gen_formula(
        height;
        logic::AbstractLogic,
        max_modepth::Integer=height,
        pruning_factor::Float64=0.0,
        rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG
    )
Return a formula having the exact specified `height`.

# Arguments
- `height::Integer`: final height of the generated tree.
- `L::AbstractLogic`: the logic where to find legal letters and operators.
- `max_modepth::Integer`: maximum number of modal operators in a path.
- `pruning_factor::Float64`: float number between 0.0 and 1.0.
    This correspond to the probability of "stop" the function at each step.
    It's useful to randomly prune the generated tree between the specified
    `height` and 0.
- `rng::Union{Integer,AbstractRNG}`: an rng, or the seed to initialize one.
"""
function gen_formula(
    height::Integer,
    P::Vector{<:AbstractPropositionalLetter},
    logic::AbstractLogic;
    max_modepth::Integer=height,
    pruning_factor::Float64=0.0,
    rng::Union{Integer,AbstractRNG}=Random.GLOBAL_RNG,
)
    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng
    fx = parseformula(
        _gen_formula(
            height,
            P,
            SoleLogics.operators(logic),
            modal_depth = max_modepth,
            pruning_factor = pruning_factor,
            rng = rng,
        ),
    )
    return fx
end

# gen_formula core
function _gen_formula(
    height::Integer,
    P::Vector{<:AbstractPropositionalLetter},
    C::Operators;
    modal_depth::Integer,
    pruning_factor::Float64 = 0.0,
    rng::AbstractRNG = Random.GLOBAL_RNG,
)
    # Propositional letters are always leaf
    if height == 0 || rand(rng) < pruning_factor
        return [rand(rng, P)]
    end

    # A random valid operator is chosen
    if modal_depth == 0
        op = rand(rng, filter(x -> !is_modal_operator(x), C))
    else
        op = rand(rng, C)
    end

    # Operator C refers to a number of subformulas equal to its ariety
    f = vcat(
        map(
            _ -> _gen_formula(
                height - 1,
                P,
                C,
                modal_depth = modal_depth - is_modal_operator(op),
                pruning_factor = pruning_factor,
                rng = rng,
            ),
            1:ariety(op),
        )...,
    )
    f = convert(Vector{Union{AbstractPropositionalLetter, AbstractOperator}}, f)
    push!(f, op)

    return f
end
=#
=#
