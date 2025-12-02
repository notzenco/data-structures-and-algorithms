"""
Binary Search Tree - Ordered binary tree

Time Complexity (average):
- insert!: O(log n)
- contains: O(log n)
- remove!: O(log n)
- find_min/find_max: O(log n)
"""

mutable struct BSTNode{T}
    data::T
    left::Union{BSTNode{T}, Nothing}
    right::Union{BSTNode{T}, Nothing}
end

mutable struct BinarySearchTree{T}
    root::Union{BSTNode{T}, Nothing}
    count::Int

    BinarySearchTree{T}() where T = new{T}(nothing, 0)
end

BinarySearchTree() = BinarySearchTree{Any}()

function _insert_node(node::Union{BSTNode{T}, Nothing}, value::T) where T
    if isnothing(node)
        return BSTNode{T}(value, nothing, nothing)
    end
    if value < node.data
        node.left = _insert_node(node.left, value)
    elseif value > node.data
        node.right = _insert_node(node.right, value)
    end
    node
end

"""
    insert!(tree::BinarySearchTree{T}, value::T) where T

Insert a value into the tree.
"""
function Base.insert!(tree::BinarySearchTree{T}, value::T) where T
    old_root = tree.root
    tree.root = _insert_node(tree.root, value)
    if tree.root !== old_root || tree.count == 0
        tree.count += 1
    end
    tree
end

function _contains_node(node::Union{BSTNode{T}, Nothing}, value::T) where T
    if isnothing(node)
        return false
    end
    if value == node.data
        return true
    elseif value < node.data
        return _contains_node(node.left, value)
    else
        return _contains_node(node.right, value)
    end
end

"""
    contains(tree::BinarySearchTree{T}, value::T) where T

Check if value exists in the tree.
"""
function contains(tree::BinarySearchTree{T}, value::T) where T
    _contains_node(tree.root, value)
end

function _find_min_node(node::BSTNode{T}) where T
    current = node
    while !isnothing(current.left)
        current = current.left
    end
    current
end

function _find_max_node(node::BSTNode{T}) where T
    current = node
    while !isnothing(current.right)
        current = current.right
    end
    current
end

"""
    find_min(tree::BinarySearchTree{T}) where T

Find the minimum value. Throws ArgumentError if tree is empty.
"""
function find_min(tree::BinarySearchTree{T}) where T
    if isnothing(tree.root)
        throw(ArgumentError("Tree is empty"))
    end
    _find_min_node(tree.root).data
end

"""
    find_max(tree::BinarySearchTree{T}) where T

Find the maximum value. Throws ArgumentError if tree is empty.
"""
function find_max(tree::BinarySearchTree{T}) where T
    if isnothing(tree.root)
        throw(ArgumentError("Tree is empty"))
    end
    _find_max_node(tree.root).data
end

function _remove_node(node::Union{BSTNode{T}, Nothing}, value::T) where T
    if isnothing(node)
        return (nothing, false)
    end

    if value < node.data
        node.left, removed = _remove_node(node.left, value)
        return (node, removed)
    elseif value > node.data
        node.right, removed = _remove_node(node.right, value)
        return (node, removed)
    else
        # Found node to remove
        if isnothing(node.left)
            return (node.right, true)
        elseif isnothing(node.right)
            return (node.left, true)
        else
            # Node has two children
            successor = _find_min_node(node.right)
            node.data = successor.data
            node.right, _ = _remove_node(node.right, successor.data)
            return (node, true)
        end
    end
end

"""
    remove!(tree::BinarySearchTree{T}, value::T) where T

Remove a value from the tree. Returns true if value was found.
"""
function remove!(tree::BinarySearchTree{T}, value::T) where T
    tree.root, removed = _remove_node(tree.root, value)
    if removed
        tree.count -= 1
    end
    removed
end

"""
    isempty(tree::BinarySearchTree)

Check if the tree is empty.
"""
Base.isempty(tree::BinarySearchTree) = tree.count == 0

"""
    length(tree::BinarySearchTree)

Return the number of nodes.
"""
Base.length(tree::BinarySearchTree) = tree.count

"""
    size(tree::BinarySearchTree)

Return the number of nodes.
"""
Base.size(tree::BinarySearchTree) = tree.count

"""
    empty!(tree::BinarySearchTree)

Remove all nodes.
"""
function Base.empty!(tree::BinarySearchTree)
    tree.root = nothing
    tree.count = 0
    tree
end

function _inorder_node(node::Union{BSTNode{T}, Nothing}, result::Vector{T}) where T
    if !isnothing(node)
        _inorder_node(node.left, result)
        push!(result, node.data)
        _inorder_node(node.right, result)
    end
end

"""
    inorder(tree::BinarySearchTree{T}) where T

Get values in sorted order (in-order traversal).
"""
function inorder(tree::BinarySearchTree{T}) where T
    result = T[]
    _inorder_node(tree.root, result)
    result
end

function _preorder_node(node::Union{BSTNode{T}, Nothing}, result::Vector{T}) where T
    if !isnothing(node)
        push!(result, node.data)
        _preorder_node(node.left, result)
        _preorder_node(node.right, result)
    end
end

"""
    preorder(tree::BinarySearchTree{T}) where T

Get values in pre-order traversal.
"""
function preorder(tree::BinarySearchTree{T}) where T
    result = T[]
    _preorder_node(tree.root, result)
    result
end

function _postorder_node(node::Union{BSTNode{T}, Nothing}, result::Vector{T}) where T
    if !isnothing(node)
        _postorder_node(node.left, result)
        _postorder_node(node.right, result)
        push!(result, node.data)
    end
end

"""
    postorder(tree::BinarySearchTree{T}) where T

Get values in post-order traversal.
"""
function postorder(tree::BinarySearchTree{T}) where T
    result = T[]
    _postorder_node(tree.root, result)
    result
end

function _height_node(node::Union{BSTNode, Nothing})
    if isnothing(node)
        return -1
    end
    max(_height_node(node.left), _height_node(node.right)) + 1
end

"""
    height(tree::BinarySearchTree)

Get the height of the tree.
"""
function height(tree::BinarySearchTree)
    _height_node(tree.root)
end

# Iterator interface (in-order)
Base.iterate(tree::BinarySearchTree) = iterate(inorder(tree))
Base.iterate(tree::BinarySearchTree, state) = iterate(inorder(tree), state)
