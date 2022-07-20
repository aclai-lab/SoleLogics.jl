mutable struct Node{T}
    token::T            # token (e.g., Proposition)
    parent::Node        # parent node
    leftchild::Node     # left child node
    rightchild::Node    # right child node
    formula::String     # human-readable string of the formula
    height::Int         # height of the tree rooted here

    # root constructor
    Node{T}(token::T) where {T} = new{T}(token)
end

Node(token::T) where {T} = Node{T}(token)

token(ν::Node) = ν.token
parent(ν::Node) = ν.parent
leftchild(ν::Node) = ν.leftchild
rightchild(ν::Node) = ν.rightchild
formula(ν::Node) = ν.formula
height(v::Node) = v.height

parent!(ν::Node, ν′::Node) = ν.parent = ν′
leftchild!(ν::Node, ν′::Node) = ν.leftchild = ν′
rightchild!(ν::Node, ν′::Node) = ν.rightchild = ν′
formula!(ν::Node, ν′::Node) = ν.formula = ν′.formula
height!(ν::Node, ν′::Node) = ν.height = ν′.height

function size(ν::Node)
    leftchild_size = isdefined(ν, :leftchild) ? size(leftchild(ν)) : 0
    rightchild_size = isdefined(ν, :rightchild) ? size(rightchild(ν)) : 0
    return 1 + leftchild_size + rightchild_size
end

function isleaf(ν::Node)
    return !(isdefined(ν, :leftchild) || isdefined(ν, :rightchild)) ? true : false
end

#= Mauro:
I added height in Node definition so I comment this to avoid name conflict.
Anyway this will be useful to recompute height if a tree is modified but
we should modify the recursion to take advantage of memoization.

function height(ν::Node)
    return isleaf(ν) ? 1 : 1 + max(
        (isdefined(ν, :leftchild) ? height(leftchild(ν)) : 0),
        (isdefined(ν, :rightchild) ? height(rightchild(ν)) : 0))
end
=#

# TODO: add modaldepth() function (hint: use traits such as ismodal() function)

# fixed
function _printnode(io::IO, ν::Node)
    print(io, "(")
    if isdefined(ν, :leftchild)
        (_printnode(io, _leftchild(ν)))
    end
    print(io, token(ν))
    if isdefined(ν, :rightchild)
        _printnode(io, _rightchild(ν))
    end
    print(io, ")")
end

show(io::IO, ν::Node) = _printnode(io, ν)

struct Formula
    tree::Node # syntax tree
end

# # # testing
# println("\tformulas.jl testing")
# n = Node(IMPLICATION)
# n_l = Node("p")
# n_r = Node("q")
# _leftchild!(n, n_l)
# _rightchild!(n, n_r)
# _parent!(n_l, n)
# _parent!(n_r, n)

# @show n
# @show size(n)
# @show parent(n_l)
# @show rightchild(n)
# @show isleaf(n)
# @show isleaf(n_l)
# @show isleaf(n_r)
# @show height(n)
# @show height(n_l)
# @show height(n_r)
# @show token(n)
# @show token(n_l)
# @show token(n_r)
