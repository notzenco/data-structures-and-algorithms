## Binary Search Tree - Ordered binary tree
##
## Time Complexity (average):
## - insert: O(log n)
## - contains: O(log n)
## - remove: O(log n)
## - findMin/findMax: O(log n)

type
  BSTNode[T] = ref object
    data: T
    left: BSTNode[T]
    right: BSTNode[T]

  BinarySearchTree*[T] = object
    root: BSTNode[T]
    count: int

proc newBinarySearchTree*[T](): BinarySearchTree[T] =
  ## Create a new empty binary search tree
  BinarySearchTree[T](root: nil, count: 0)

proc insertNode[T](node: BSTNode[T], value: T): BSTNode[T] =
  if node == nil:
    return BSTNode[T](data: value, left: nil, right: nil)
  if value < node.data:
    node.left = insertNode(node.left, value)
  elif value > node.data:
    node.right = insertNode(node.right, value)
  node

proc insert*[T](tree: var BinarySearchTree[T], value: T) =
  ## Insert a value into the tree
  let oldRoot = tree.root
  tree.root = insertNode(tree.root, value)
  if tree.root != oldRoot or tree.count == 0:
    inc tree.count

proc containsNode[T](node: BSTNode[T], value: T): bool =
  if node == nil:
    return false
  if value == node.data:
    return true
  if value < node.data:
    return containsNode(node.left, value)
  containsNode(node.right, value)

proc contains*[T](tree: BinarySearchTree[T], value: T): bool =
  ## Check if value exists in the tree
  containsNode(tree.root, value)

proc findMinNode[T](node: BSTNode[T]): BSTNode[T] =
  var current = node
  while current.left != nil:
    current = current.left
  current

proc findMaxNode[T](node: BSTNode[T]): BSTNode[T] =
  var current = node
  while current.right != nil:
    current = current.right
  current

proc findMin*[T](tree: BinarySearchTree[T]): T =
  ## Find the minimum value
  ## Raises ValueError if tree is empty
  if tree.root == nil:
    raise newException(ValueError, "Tree is empty")
  findMinNode(tree.root).data

proc findMax*[T](tree: BinarySearchTree[T]): T =
  ## Find the maximum value
  ## Raises ValueError if tree is empty
  if tree.root == nil:
    raise newException(ValueError, "Tree is empty")
  findMaxNode(tree.root).data

proc removeNode[T](node: BSTNode[T], value: T): (BSTNode[T], bool) =
  if node == nil:
    return (nil, false)

  if value < node.data:
    let (newLeft, removed) = removeNode(node.left, value)
    node.left = newLeft
    return (node, removed)
  elif value > node.data:
    let (newRight, removed) = removeNode(node.right, value)
    node.right = newRight
    return (node, removed)
  else:
    # Found node to remove
    if node.left == nil:
      return (node.right, true)
    elif node.right == nil:
      return (node.left, true)
    else:
      # Node has two children
      let successor = findMinNode(node.right)
      node.data = successor.data
      let (newRight, _) = removeNode(node.right, successor.data)
      node.right = newRight
      return (node, true)

proc remove*[T](tree: var BinarySearchTree[T], value: T): bool =
  ## Remove a value from the tree
  ## Returns true if value was found and removed
  let (newRoot, removed) = removeNode(tree.root, value)
  tree.root = newRoot
  if removed:
    dec tree.count
  removed

proc isEmpty*[T](tree: BinarySearchTree[T]): bool =
  ## Check if the tree is empty
  tree.count == 0

proc size*[T](tree: BinarySearchTree[T]): int =
  ## Return the number of nodes
  tree.count

proc len*[T](tree: BinarySearchTree[T]): int =
  ## Return the number of nodes (alias for size)
  tree.count

proc clear*[T](tree: var BinarySearchTree[T]) =
  ## Remove all nodes
  tree.root = nil
  tree.count = 0

proc inorderNode[T](node: BSTNode[T], result: var seq[T]) =
  if node != nil:
    inorderNode(node.left, result)
    result.add(node.data)
    inorderNode(node.right, result)

proc inorder*[T](tree: BinarySearchTree[T]): seq[T] =
  ## Get values in sorted order (in-order traversal)
  result = @[]
  inorderNode(tree.root, result)

proc preorderNode[T](node: BSTNode[T], result: var seq[T]) =
  if node != nil:
    result.add(node.data)
    preorderNode(node.left, result)
    preorderNode(node.right, result)

proc preorder*[T](tree: BinarySearchTree[T]): seq[T] =
  ## Get values in pre-order traversal
  result = @[]
  preorderNode(tree.root, result)

proc postorderNode[T](node: BSTNode[T], result: var seq[T]) =
  if node != nil:
    postorderNode(node.left, result)
    postorderNode(node.right, result)
    result.add(node.data)

proc postorder*[T](tree: BinarySearchTree[T]): seq[T] =
  ## Get values in post-order traversal
  result = @[]
  postorderNode(tree.root, result)

proc heightNode[T](node: BSTNode[T]): int =
  if node == nil:
    return -1
  max(heightNode(node.left), heightNode(node.right)) + 1

proc height*[T](tree: BinarySearchTree[T]): int =
  ## Get the height of the tree
  heightNode(tree.root)

iterator items*[T](tree: BinarySearchTree[T]): T =
  ## Iterate over values in sorted order
  for value in tree.inorder():
    yield value

when isMainModule:
  var tree = newBinarySearchTree[int]()
  tree.insert(5)
  tree.insert(3)
  tree.insert(7)
  tree.insert(1)
  tree.insert(9)
  assert tree.contains(5)
  assert tree.contains(3)
  assert not tree.contains(6)
  assert tree.findMin() == 1
  assert tree.findMax() == 9
  assert tree.size() == 5
  assert tree.remove(3)
  assert not tree.contains(3)
  assert tree.size() == 4
  assert tree.inorder() == @[1, 5, 7, 9]
  echo "BinarySearchTree tests passed!"
