-- Binary Search Tree - Ordered binary tree
--
-- Time Complexity (average):
-- - insert: O(log n)
-- - contains: O(log n)
-- - remove: O(log n)
-- - findMin/findMax: O(log n)

local BinarySearchTree = {}
BinarySearchTree.__index = BinarySearchTree

local function newNode(data)
    return { data = data, left = nil, right = nil }
end

function BinarySearchTree.new()
    local self = setmetatable({}, BinarySearchTree)
    self.root = nil
    self.count = 0
    return self
end

local function insertNode(node, value)
    if node == nil then
        return newNode(value)
    end
    if value < node.data then
        node.left = insertNode(node.left, value)
    elseif value > node.data then
        node.right = insertNode(node.right, value)
    end
    return node
end

function BinarySearchTree:insert(value)
    local oldRoot = self.root
    self.root = insertNode(self.root, value)
    if self.root ~= oldRoot or self.count == 0 then
        self.count = self.count + 1
    end
end

local function containsNode(node, value)
    if node == nil then
        return false
    end
    if value == node.data then
        return true
    elseif value < node.data then
        return containsNode(node.left, value)
    else
        return containsNode(node.right, value)
    end
end

function BinarySearchTree:contains(value)
    return containsNode(self.root, value)
end

local function findMinNode(node)
    local current = node
    while current.left ~= nil do
        current = current.left
    end
    return current
end

local function findMaxNode(node)
    local current = node
    while current.right ~= nil do
        current = current.right
    end
    return current
end

function BinarySearchTree:findMin()
    if self.root == nil then
        error("Tree is empty")
    end
    return findMinNode(self.root).data
end

function BinarySearchTree:findMax()
    if self.root == nil then
        error("Tree is empty")
    end
    return findMaxNode(self.root).data
end

local function removeNode(node, value)
    if node == nil then
        return nil, false
    end

    if value < node.data then
        node.left, removed = removeNode(node.left, value)
        return node, removed
    elseif value > node.data then
        node.right, removed = removeNode(node.right, value)
        return node, removed
    else
        if node.left == nil then
            return node.right, true
        elseif node.right == nil then
            return node.left, true
        else
            local successor = findMinNode(node.right)
            node.data = successor.data
            node.right, _ = removeNode(node.right, successor.data)
            return node, true
        end
    end
end

function BinarySearchTree:remove(value)
    local removed
    self.root, removed = removeNode(self.root, value)
    if removed then
        self.count = self.count - 1
    end
    return removed
end

function BinarySearchTree:isEmpty()
    return self.count == 0
end

function BinarySearchTree:size()
    return self.count
end

function BinarySearchTree:clear()
    self.root = nil
    self.count = 0
end

local function inorderNode(node, result)
    if node ~= nil then
        inorderNode(node.left, result)
        result[#result + 1] = node.data
        inorderNode(node.right, result)
    end
end

function BinarySearchTree:inorder()
    local result = {}
    inorderNode(self.root, result)
    return result
end

local function preorderNode(node, result)
    if node ~= nil then
        result[#result + 1] = node.data
        preorderNode(node.left, result)
        preorderNode(node.right, result)
    end
end

function BinarySearchTree:preorder()
    local result = {}
    preorderNode(self.root, result)
    return result
end

local function postorderNode(node, result)
    if node ~= nil then
        postorderNode(node.left, result)
        postorderNode(node.right, result)
        result[#result + 1] = node.data
    end
end

function BinarySearchTree:postorder()
    local result = {}
    postorderNode(self.root, result)
    return result
end

return BinarySearchTree
