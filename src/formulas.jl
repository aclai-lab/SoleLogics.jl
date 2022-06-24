mutable struct Node{T}
    token::T            # token (e.g., Proposition)
    parent::Node        # parent node
    leftchild::Node     # left child node
    rightchild::Node    # right child node
    formula::String     # human-readable string of the formula

    # root constructor
    Node{T}(token::T) where {T} = new{T}(token)
end

Node(token::T) where {T} = Node{T}(token)

_token(ν::Node) = ν.token
_parent(ν::Node) = ν.parent
_leftchild(ν::Node) = ν.leftchild
_rightchild(ν::Node) = ν.rightchild
_formula(ν::Node) = ν.formula

_parent!(ν::Node, ν′::Node) = ν.parent = ν′
_leftchild!(ν::Node, ν′::Node) = ν.leftchild = ν′
_rightchild!(ν::Node, ν′::Node) = ν.rightchild = ν′
_formula!(ν::Node, ν′::Node) = ν.formula = ν′

function _size(ν::Node)
    leftchild_size = isdefined(ν, :leftchild) ? _size(_leftchild(ν)) : 0
    rightchild_size = isdefined(ν, :rightchild) ? _size(_rightchild(ν)) : 0
    return 1 + leftchild_size + rightchild_size
end

function _isleaf(ν::Node)
    return !(isdefined(ν, :leftchild) || isdefined(ν, :rightchild)) ? true : false
end

function _height(ν::Node)
    return _isleaf(ν) ? 1 : 1 + max(
        (isdefined(ν, :leftchild) ? _height(_leftchild(ν)) : 0),
        (isdefined(ν, :rightchild) ? _height(_rightchild(ν)) : 0))
end

# TODO: add modaldepth() function (hint: use traits such as ismodal() function)

# fixed
function _printnode(io::IO, ν::Node)
    print(io, "(")
    if isdefined(ν, :leftchild)
        (_printnode(io, _leftchild(ν)))
    end
    print(io, _token(ν))
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
# @show _size(n)
# @show _parent(n_l)
# @show _rightchild(n)
# @show _isleaf(n)
# @show _isleaf(n_l)
# @show _isleaf(n_r)
# @show _height(n)
# @show _height(n_l)
# @show _height(n_r)
# @show _token(n)
# @show _token(n_l)
# @show _token(n_r)
