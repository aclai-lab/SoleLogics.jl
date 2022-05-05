mutable struct Node{T}
    category::T         # category can be Proposition or Operator
    parent::Node        # parent node
    leftchild::Node     # left child node
    rightchild::Node    # right child node
    formula::String     # human readable string of the formula
end

struct Formula
    tree::Node # syntax tree
end
