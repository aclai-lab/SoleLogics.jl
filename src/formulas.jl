#################################
#       Node structure          #
#      getters & setters        #
#################################
mutable struct Node{T}
    token::T            # token (e.g., Proposition)
    parent::Node        # parent node
    leftchild::Node     # left child node
    rightchild::Node    # right child node
    formula::String     # human-readable string of the formula
    size::Int           # size of the tree rooted here

    # root constructor
    Node{T}(token::T) where {T} = new{T}(token)
end

Node(token::T) where {T} = Node{T}(token)

token(ν::Node) = ν.token
parent(ν::Node) = ν.parent
leftchild(ν::Node) = ν.leftchild
rightchild(ν::Node) = ν.rightchild
formula(ν::Node) = ν.formula

parent!(ν::Node, ν′::Node) = ν.parent = ν′
leftchild!(ν::Node, ν′::Node) = ν.leftchild = ν′
rightchild!(ν::Node, ν′::Node) = ν.rightchild = ν′
formula!(ν::Node, ν′::Node) = ν.formula = ν′.formula

#################################
#       Node wrappers           #
#       and utilities           #
#################################
struct Formula
    tree::Node  # syntax tree
end

show(io::IO, v::Node) = print(io, inorder(v))
show(io::IO, f::Formula) = print(io, inorder(f.tree))

function isleaf(ν::Node)
    return !(isdefined(ν, :leftchild) || isdefined(ν, :rightchild)) ? true : false
end

function size(v::Node)
    if isdefined(v, :leftchild)
        leftchild(v).size = size(leftchild(v))
    end
    if isdefined(v, :rightchild)
        rightchild(v).size = size(rightchild(v))
    end
    return v.size = 1 +
        (isdefined(v, :leftchild) ? leftchild(v).size : 0) +
        (isdefined(v, :rightchild) ? rightchild(v).size : 0)
end

# TODO: implement memoization as in size
function height(ν::Node)
    return isleaf(ν) ? 1 : 1 + max(
        (isdefined(ν, :leftchild) ? height(leftchild(ν)) : 0),
        (isdefined(ν, :rightchild) ? height(rightchild(ν)) : 0))
end

# TODO: add modaldepth() function (hint: use traits such as ismodal() function)

# Return tree visit as a string, collecting tokens found on the path
function inorder(v::Node)
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
