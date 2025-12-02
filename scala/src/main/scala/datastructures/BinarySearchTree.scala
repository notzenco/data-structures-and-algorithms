package datastructures

/**
 * Binary search tree with standard operations.
 * Time: O(log n) average, O(n) worst case
 * Space: O(n)
 */
class BinarySearchTree[T](using ord: Ordering[T]):
  private case class Node(var value: T, var left: Node = null, var right: Node = null)

  private var root: Node = null
  private var count = 0

  def insert(value: T): this.type =
    root = insertNode(root, value)
    this

  private def insertNode(node: Node, value: T): Node =
    if node == null then
      count += 1
      Node(value)
    else if ord.lt(value, node.value) then
      node.left = insertNode(node.left, value)
      node
    else if ord.gt(value, node.value) then
      node.right = insertNode(node.right, value)
      node
    else
      node.value = value
      node

  def contains(value: T): Boolean = findNode(root, value) != null

  private def findNode(node: Node, value: T): Node =
    if node == null then null
    else if ord.lt(value, node.value) then findNode(node.left, value)
    else if ord.gt(value, node.value) then findNode(node.right, value)
    else node

  def remove(value: T): Boolean =
    val originalCount = count
    root = removeNode(root, value)
    count < originalCount

  private def removeNode(node: Node, value: T): Node =
    if node == null then null
    else if ord.lt(value, node.value) then
      node.left = removeNode(node.left, value)
      node
    else if ord.gt(value, node.value) then
      node.right = removeNode(node.right, value)
      node
    else
      count -= 1
      if node.left == null then node.right
      else if node.right == null then node.left
      else
        val successor = findMin(node.right)
        node.value = successor.value
        count += 1
        node.right = removeNode(node.right, successor.value)
        node

  private def findMin(node: Node): Node =
    if node.left == null then node
    else findMin(node.left)

  def min: Option[T] =
    if root == null then None
    else Some(findMin(root).value)

  def max: Option[T] =
    if root == null then None
    else
      var current = root
      while current.right != null do current = current.right
      Some(current.value)

  def isEmpty: Boolean = count == 0

  def size: Int = count

  def clear(): this.type =
    root = null
    count = 0
    this

  def inorder: List[T] =
    val builder = List.newBuilder[T]
    def traverse(node: Node): Unit =
      if node != null then
        traverse(node.left)
        builder += node.value
        traverse(node.right)
    traverse(root)
    builder.result()

  def preorder: List[T] =
    val builder = List.newBuilder[T]
    def traverse(node: Node): Unit =
      if node != null then
        builder += node.value
        traverse(node.left)
        traverse(node.right)
    traverse(root)
    builder.result()

  def postorder: List[T] =
    val builder = List.newBuilder[T]
    def traverse(node: Node): Unit =
      if node != null then
        traverse(node.left)
        traverse(node.right)
        builder += node.value
    traverse(root)
    builder.result()

  def levelOrder: List[T] =
    if root == null then List.empty
    else
      val builder = List.newBuilder[T]
      val queue = scala.collection.mutable.Queue[Node](root)
      while queue.nonEmpty do
        val node = queue.dequeue()
        builder += node.value
        if node.left != null then queue.enqueue(node.left)
        if node.right != null then queue.enqueue(node.right)
      builder.result()
