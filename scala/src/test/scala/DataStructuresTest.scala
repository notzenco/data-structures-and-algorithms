import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import datastructures.*

class DataStructuresTest extends AnyFunSuite with Matchers:

  // Stack Tests
  test("Stack - push and pop"):
    val stack = Stack[Int]()
    stack.push(1).push(2).push(3)
    stack.pop() shouldBe Some(3)
    stack.pop() shouldBe Some(2)
    stack.pop() shouldBe Some(1)
    stack.pop() shouldBe None

  test("Stack - peek"):
    val stack = Stack[Int]()
    stack.push(42)
    stack.peek shouldBe Some(42)
    stack.size shouldBe 1

  test("Stack - isEmpty"):
    val stack = Stack[Int]()
    stack.isEmpty shouldBe true
    stack.push(1)
    stack.isEmpty shouldBe false

  // Queue Tests
  test("Queue - enqueue and dequeue"):
    val queue = Queue[Int]()
    queue.enqueue(1).enqueue(2).enqueue(3)
    queue.dequeue() shouldBe Some(1)
    queue.dequeue() shouldBe Some(2)
    queue.dequeue() shouldBe Some(3)
    queue.dequeue() shouldBe None

  test("Queue - peek"):
    val queue = Queue[Int]()
    queue.enqueue(42)
    queue.peek shouldBe Some(42)
    queue.size shouldBe 1

  // DynamicArray Tests
  test("DynamicArray - push and pop"):
    val arr = DynamicArray[Int]()
    arr.push(1).push(2).push(3)
    arr.pop() shouldBe Some(3)
    arr.size shouldBe 2

  test("DynamicArray - get and set"):
    val arr = DynamicArray[Int]()
    arr.push(1).push(2).push(3)
    arr.get(1) shouldBe Some(2)
    arr.set(1, 42) shouldBe true
    arr.get(1) shouldBe Some(42)

  test("DynamicArray - insert and removeAt"):
    val arr = DynamicArray[Int]()
    arr.push(1).push(3)
    arr.insert(1, 2) shouldBe true
    arr.toList shouldBe List(1, 2, 3)
    arr.removeAt(1) shouldBe Some(2)
    arr.toList shouldBe List(1, 3)

  // SinglyLinkedList Tests
  test("SinglyLinkedList - prepend and append"):
    val list = SinglyLinkedList[Int]()
    list.prepend(2).prepend(1).append(3)
    list.toList shouldBe List(1, 2, 3)

  test("SinglyLinkedList - get and indexOf"):
    val list = SinglyLinkedList[Int]()
    list.append(10).append(20).append(30)
    list.get(1) shouldBe Some(20)
    list.indexOf(20) shouldBe 1
    list.indexOf(100) shouldBe -1

  test("SinglyLinkedList - insert and removeAt"):
    val list = SinglyLinkedList[Int]()
    list.append(1).append(3)
    list.insert(1, 2) shouldBe true
    list.toList shouldBe List(1, 2, 3)
    list.removeAt(1) shouldBe Some(2)
    list.toList shouldBe List(1, 3)

  // DoublyLinkedList Tests
  test("DoublyLinkedList - prepend and append"):
    val list = DoublyLinkedList[Int]()
    list.prepend(2).prepend(1).append(3)
    list.toList shouldBe List(1, 2, 3)

  test("DoublyLinkedList - first and last"):
    val list = DoublyLinkedList[Int]()
    list.append(1).append(2).append(3)
    list.first shouldBe Some(1)
    list.last shouldBe Some(3)

  test("DoublyLinkedList - removeFirst and removeLast"):
    val list = DoublyLinkedList[Int]()
    list.append(1).append(2).append(3)
    list.removeFirst() shouldBe Some(1)
    list.removeLast() shouldBe Some(3)
    list.toList shouldBe List(2)

  // Deque Tests
  test("Deque - pushFront and pushBack"):
    val deque = Deque[Int]()
    deque.pushFront(2).pushFront(1).pushBack(3)
    deque.popFront() shouldBe Some(1)
    deque.popBack() shouldBe Some(3)
    deque.popFront() shouldBe Some(2)

  test("Deque - peekFront and peekBack"):
    val deque = Deque[Int]()
    deque.pushBack(1).pushBack(2)
    deque.peekFront shouldBe Some(1)
    deque.peekBack shouldBe Some(2)

  // HashTable Tests
  test("HashTable - put and get"):
    val table = HashTable[String, Int]()
    table.put("one", 1).put("two", 2).put("three", 3)
    table.get("one") shouldBe Some(1)
    table.get("two") shouldBe Some(2)
    table.get("four") shouldBe None

  test("HashTable - remove"):
    val table = HashTable[String, Int]()
    table.put("key", 42)
    table.remove("key") shouldBe Some(42)
    table.get("key") shouldBe None

  test("HashTable - keys and values"):
    val table = HashTable[String, Int]()
    table.put("a", 1).put("b", 2)
    table.keys.toSet shouldBe Set("a", "b")
    table.values.toSet shouldBe Set(1, 2)

  test("HashTable - resize"):
    val table = HashTable[Int, Int](4)
    for i <- 1 to 20 do table.put(i, i * 10)
    table.size shouldBe 20
    for i <- 1 to 20 do table.get(i) shouldBe Some(i * 10)

  // BinarySearchTree Tests
  test("BST - insert and contains"):
    val bst = BinarySearchTree[Int]()
    bst.insert(5).insert(3).insert(7).insert(1).insert(9)
    bst.contains(5) shouldBe true
    bst.contains(3) shouldBe true
    bst.contains(10) shouldBe false

  test("BST - min and max"):
    val bst = BinarySearchTree[Int]()
    bst.insert(5).insert(3).insert(7).insert(1).insert(9)
    bst.min shouldBe Some(1)
    bst.max shouldBe Some(9)

  test("BST - remove"):
    val bst = BinarySearchTree[Int]()
    bst.insert(5).insert(3).insert(7)
    bst.remove(5) shouldBe true
    bst.contains(5) shouldBe false
    bst.size shouldBe 2

  test("BST - inorder traversal"):
    val bst = BinarySearchTree[Int]()
    bst.insert(5).insert(3).insert(7).insert(1).insert(9)
    bst.inorder shouldBe List(1, 3, 5, 7, 9)

  // MinHeap Tests
  test("MinHeap - insert and extractMin"):
    val heap = MinHeap[Int]()
    heap.insert(5).insert(3).insert(7).insert(1).insert(9)
    heap.extractMin() shouldBe Some(1)
    heap.extractMin() shouldBe Some(3)
    heap.extractMin() shouldBe Some(5)

  test("MinHeap - peek"):
    val heap = MinHeap[Int]()
    heap.insert(5).insert(3).insert(7)
    heap.peek shouldBe Some(3)
    heap.size shouldBe 3

  test("MinHeap - from factory"):
    val heap = MinHeap.from(List(5, 3, 7, 1, 9))
    heap.extractMin() shouldBe Some(1)
    heap.extractMin() shouldBe Some(3)

  // DisjointSet Tests
  test("DisjointSet - makeSet and find"):
    val ds = DisjointSet[Int]()
    ds.makeSet(1).makeSet(2).makeSet(3)
    ds.find(1) shouldBe Some(1)
    ds.find(4) shouldBe None

  test("DisjointSet - union and connected"):
    val ds = DisjointSet[Int]()
    ds.makeSet(1).makeSet(2).makeSet(3)
    ds.connected(1, 2) shouldBe false
    ds.union(1, 2) shouldBe true
    ds.connected(1, 2) shouldBe true
    ds.connected(1, 3) shouldBe false

  test("DisjointSet - setCount"):
    val ds = DisjointSet[Int]()
    ds.makeSet(1).makeSet(2).makeSet(3).makeSet(4)
    ds.setCount shouldBe 4
    ds.union(1, 2)
    ds.setCount shouldBe 3
    ds.union(3, 4)
    ds.setCount shouldBe 2
    ds.union(1, 3)
    ds.setCount shouldBe 1
