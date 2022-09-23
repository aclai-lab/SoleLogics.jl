#################################
#       FNode structure          #
#      getters & setters        #
#################################

const Token = Union{Letter,AbstractOperator}

mutable struct FNode{L<:Logic}
    token::Token            # token (e.g., Proposition)
    formula::String         # human-readable string of the formula
    size::Int               # size of the tree rooted here

    parent::FNode{L}         # parent FNode
    leftchild::FNode{L}      # left child FNode
    rightchild::FNode{L}     # right child FNode

    # root constructor
    FNode{L}(token::Token) where {L<:Logic} = new{L}(token)
end

FNode(token::Token) = FNode{typeof(DEFAULT_LOGIC)}(token)
FNode(token::Token, ::L) where {L} = FNode{L}(token)
FNode(token::Token, L::Type) = FNode{L}(token)

token(v::FNode) = v.token
parent(v::FNode) = v.parent
leftchild(v::FNode) = v.leftchild
rightchild(v::FNode) = v.rightchild
formula(v::FNode) = v.formula
fhash(v::FNode) = hash(formula(v))
size(v::FNode) = v.size

parent!(v::FNode, v′::FNode) = v.parent = v′
leftchild!(v::FNode, v′::FNode) = v.leftchild = v′
rightchild!(v::FNode, v′::FNode) = v.rightchild = v′
formula!(v::FNode, v′::FNode) = v.formula = v′.formula

extract_logic(v::FNode) = typeof(v).parameters[1]

#################################
#          Formula              #
#       and utilities           #
#################################

struct Formula
    tree::FNode  # syntax tree
end

tree(f::Formula) = f.tree
extract_logic(f::Formula) = extract_logic(tree(f))

show(io::IO, v::FNode) = print(io, inorder(v))
show(io::IO, f::Formula) = print(io, inorder(tree(f)))

function isleaf(v::FNode)
    return !(isdefined(v, :leftchild) || isdefined(v, :rightchild)) ? true : false
end

function size!(v::FNode)
    if isdefined(v, :leftchild)
        leftchild(v).size = size!(leftchild(v))
    end
    if isdefined(v, :rightchild)
        rightchild(v).size = size!(rightchild(v))
    end
    return v.size =
        1 +
        (isdefined(v, :leftchild) ? leftchild(v).size : 0) +
        (isdefined(v, :rightchild) ? rightchild(v).size : 0)
end

function height(v::FNode)
    return isleaf(v) ? 0 :
           1 + max(
        (isdefined(v, :leftchild) ? height(leftchild(v)) : 0),
        (isdefined(v, :rightchild) ? height(rightchild(v)) : 0),
    )
end

function modal_depth(v::FNode)
    return is_modal_operator(token(v)) + max(
        (isdefined(v, :leftchild) ? modal_depth(leftchild(v)) : 0),
        (isdefined(v, :rightchild) ? modal_depth(rightchild(v)) : 0),
    )
end

# Collect each FNode in a tree, then sort them by size.
function subformulas(root::FNode; sorted = true)
    nodes = FNode[]
    _subformulas(root, nodes)
    if sorted
        sort!(nodes, by = n -> SoleLogics.size(n))
    end
    return nodes
end

function _subformulas(FNode::FNode, nodes::Vector{FNode})
    if isdefined(FNode, :leftchild)
        _subformulas(FNode.leftchild, nodes)
    end

    push!(nodes, FNode)

    if isdefined(FNode, :rightchild)
        _subformulas(FNode.rightchild, nodes)
    end
end

# Return tree visit as a string, collecting tokens found on the path
function inorder(v::FNode)
    str = "("
    if isdefined(v, :leftchild)
        str = string(str, inorder(v.leftchild))
    end
    str = string(str, v.token)
    if isdefined(v, :rightchild)
        str = string(str, inorder(v.rightchild))
    end
    str = string(str, ")")
    return str
end

#################################
#        Formula input          #
#       and construction        #
#################################

# A simple lexer capable of distinguish operators in a string
function tokenizer(expression::String; ops = operators(MODAL_LOGIC))
    tokens = Union{AbstractOperator,String}[]

    sym_to_op = Dict{Symbol,AbstractOperator}()
    for op in ops
        sym_to_op[Symbol(op)] = op
    end

    # Classical operators such as ∧ are represented by one character
    # but "expression" is splitted (after whitespaces removal) in order to
    # recognize multicharacter-operators such as [L] or ⟨My_123_cus7om_456_0p3r4!or⟩.
    expression = filter(x -> !isspace(x), expression)
    slices = string.(split(expression, r"((?<=\])|(?=\[))|((?<=⟩)|(?=⟨))"))

    # Multicharacter-operators are recognized,
    # while the rest of the expression is expanded.
    for slice in slices
        if slice[1] == '[' || slice[1] == '⟨'
            push!(tokens, sym_to_op[Symbol(slice)])
        else
            append!(tokens, string.(split(slice, "")))
        end
    end

    # Other operators are recognized
    for i in eachindex(tokens)
        if tokens[i] isa String && haskey(sym_to_op, Symbol(tokens[i]))
            tokens[i] = sym_to_op[Symbol(tokens[i])]
        end
    end

    return tokens
end

#=
Shunting yard algorithm explanation.

Goal:
translate an infix expression to postfix notation. (also called Reverse Polish Notation)
e.g. "□c∧◊d" becomes "c□d◊∧"
This preprocessing is useful to simplify formula (syntax) trees generations.

Data structures involved:
* `postfix`: vector of tokens (String or AbstractOperator) in RPN; this is returned
* `opstack`: stack of tokens (AbstractOperators except for the "(" string)

Algorithm:
given a certain token `tok`, 1 of 4 possible scenarios may occur:
(regrouped in _shunting_yard function to keep code clean)

1. `tok` is a valid propositional letter
    -> push "p" in `postfix`

2. `tok` is an opening bracket
    -> push "(" in `operators`

3. `tok` is a closing bracket
    -> pop from `opstack`.
    If an operator is popped, then push it into `postfix` and repeat.
    Else, if an opening bracket it's found then process the next token.
    This algorithm step it's the reason why "(" are placed in `opstack`
    and `opstack` content type is Union{AbstractOperator, String}.

4. `tok` has to be an operator
    -> pop `op` from `opstack` and push it into `postfix` if it has an higher precedence
    than `tok` and repeat.
    When the condition is no more satisfied, then it means we have found the correct
    spot where to place `tok` in `opstack`.
=#

"""
    shunting_yard(expression::String)
Return `expression` in postfix notation.
"""
function shunting_yard(expression::String; logic = MODAL_LOGIC)
    postfix = Union{AbstractOperator,String}[]
    opstack = Stack{Union{AbstractOperator,String}}() # This contains operators or "("

    tokens = tokenizer(expression, ops = operators(logic))
    for tok in tokens
        _shunting_yard(postfix, opstack, tok, logic)
    end

    # Remaining tokens are pushed to postfix
    while !isempty(opstack)
        op = pop!(opstack)
        @assert op != "(" "Mismatching brackets"
        push!(postfix, op)
    end

    return postfix
end

function _shunting_yard(postfix, opstack, tok, logic::AbstractLogic)
    # 1
    if tok in alphabet(logic)
        push!(postfix, tok)
        # 2
    elseif tok == "("
        push!(opstack, tok)
        # 3
    elseif tok == ")"
        while !isempty(opstack) && (op = pop!(opstack)) != "("
            push!(postfix, op)
        end
        # 4 (tok is certainly an operator)
    else
        while !isempty(opstack)
            if first(opstack) == "("
                break
            end

            op = pop!(opstack)  # This is not an "(", so it must be an operator

            if precedence(op) > precedence(tok)
                push!(postfix, op)
            else
                # Last pop is reverted since `tok` has to be pushed in `opstack` now.
                push!(opstack, op)
                break
            end
        end
        push!(opstack, tok)
    end
end

# Use postfix notation to generate formula-trees

#=
Formula (syntax) tree generation

Given a certain token `tok`, 1 of 3 possible scenarios may occur:
(regrouped in _build_tree function to keep code clean)

1. `tok` is a propositional letter, hence a leaf in the formula tree
    -> push a new FNode(tok) in the nodestack;

2. `tok` is an unary operator
    -> make a new FNode(tok), then link it with the FNode popped from `nodestack` top.
    Then push the new FNode into `nodestack`.

3. It is a binary operator
    -> analogue to step 2., but 2 nodes are popped and linked to the new FNode.

At the end, the only remaining FNode in `nodestack` is the root of the formula (syntax) build_tree.
=#

function build_tree(expression::Vector{Union{String,AbstractOperator}})
    nodestack = Stack{FNode}()

    for tok in expression
        _build_tree(tok, nodestack)
    end

    SoleLogics.size!(first(nodestack))
    return Formula(first(nodestack))
end

# Safe type dispatching
function build_tree(expression::Vector{<:Any})
    build_tree(convert(Vector{Union{String,AbstractOperator}}, expression))
end

# Dispatch to directly create a formula tree from a non-preprocessed string
build_tre(expression::String) = build_tre(shunting_yard(expression))

# TODO: when a FNode will be internally associated with a Logic
# modify this function in order to create leaf nodes "without repetitions"
# thus not wasting memory
function _build_tree(tok, nodestack)
    newnode = FNode(tok)
    # 1
    if is_proposition(tok)
        newnode.formula = string(tok)
        push!(nodestack, newnode)
        # 2
    elseif typeof(tok) <: AbstractUnaryOperator
        children = pop!(nodestack)

        SoleLogics.parent!(children, newnode)
        rightchild!(newnode, children)
        newnode.formula = string(tok, children.formula)

        push!(nodestack, newnode)
        # 3
    elseif typeof(tok) <: AbstractBinaryOperator
        right_child = pop!(nodestack)
        left_child = pop!(nodestack)

        SoleLogics.parent!(right_child, newnode)
        SoleLogics.parent!(left_child, newnode)
        rightchild!(newnode, right_child)
        leftchild!(newnode, left_child)
        newnode.formula = string("(", left_child.formula, tok, right_child.formula, ")")

        push!(nodestack, newnode)
    else
        throw(error("Unknown token $tok"))
    end
end

# This has to be shifted somewhere in SoleLogics or SoleAlphabets
import SoleLogics: precedence
SoleLogics.precedence(l::Letter) = Int(only(l))

function fnormalize!(fx::Formula)
    fnormalize!(tree(fx))
end

function fnormalize!(FNode::FNode)
    if isleaf(FNode)
        return
    elseif is_commutative(token(FNode))
        left_child = leftchild(FNode)
        right_child = rightchild(FNode)
        if !is_less(token(left_child), token(right_child))
            rightchild!(FNode, left_child)
            leftchild!(FNode, right_child)
        end
    end

    if isdefined(FNode, :leftchild)
        fnormalize!(leftchild(FNode))
    end
    if isdefined(FNode, :rightchild)
        fnormalize!(rightchild(FNode))
    end
end

function is_less(
    a::T1,
    b::T2,
) where {T1<:Union{Letter,<:AbstractOperator},T2<:Union{Letter,<:AbstractOperator}}
    return precedence(a) < precedence(b) ? true : false
end


#################################
#       Formula random          #
#         generation            #
#################################

using Random

function gen_formula(
    height::Integer;
    P::LetterAlphabet = SoleLogics.alphabet(MODAL_LOGIC),
    C::Operators = SoleLogics.operators(MODAL_LOGIC),
    max_modepth::Integer = height,
    pruning_factor::Float64 = 0.0,
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
)
    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng
    fx = build_tree(
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

function gen_formula(
    height::Integer,
    logic::AbstractLogic;
    max_modepth::Integer = height,
    pruning_factor::Float64 = 0.0,
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
)
    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng
    fx = build_tree(
        _gen_formula(
            height,
            SoleLogics.alphabet(logic),
            SoleLogics.operators(logic),
            modal_depth = max_modepth,
            pruning_factor = pruning_factor,
            rng = rng,
        ),
    )
    return fx
end

function _gen_formula(
    height::Integer,
    P::LetterAlphabet,
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
    f = convert(Vector{Union{Letter,AbstractOperator}}, f)
    push!(f, op)

    return f
end
