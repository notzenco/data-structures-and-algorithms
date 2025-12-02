/**
 * Binary Search Tree implementation.
 * Time: O(log n) average, O(n) worst for all operations
 * Space: O(n)
 */

interface BSTNode<T> {
  value: T;
  left: BSTNode<T> | null;
  right: BSTNode<T> | null;
}

type Comparator<T> = (a: T, b: T) => number;

export class BinarySearchTree<T> {
  private root: BSTNode<T> | null = null;
  private _size: number = 0;
  private compare: Comparator<T>;

  constructor(comparator?: Comparator<T>) {
    this.compare = comparator || ((a, b) => {
      if (a < b) return -1;
      if (a > b) return 1;
      return 0;
    });
  }

  insert(value: T): void {
    this.root = this.insertNode(this.root, value);
    this._size++;
  }

  private insertNode(node: BSTNode<T> | null, value: T): BSTNode<T> {
    if (!node) {
      return { value, left: null, right: null };
    }

    const cmp = this.compare(value, node.value);
    if (cmp < 0) {
      node.left = this.insertNode(node.left, value);
    } else if (cmp > 0) {
      node.right = this.insertNode(node.right, value);
    }
    return node;
  }

  contains(value: T): boolean {
    return this.findNode(this.root, value) !== null;
  }

  private findNode(node: BSTNode<T> | null, value: T): BSTNode<T> | null {
    if (!node) {
      return null;
    }

    const cmp = this.compare(value, node.value);
    if (cmp < 0) {
      return this.findNode(node.left, value);
    } else if (cmp > 0) {
      return this.findNode(node.right, value);
    }
    return node;
  }

  remove(value: T): boolean {
    const sizeBefore = this._size;
    this.root = this.removeNode(this.root, value);
    return this._size < sizeBefore;
  }

  private removeNode(node: BSTNode<T> | null, value: T): BSTNode<T> | null {
    if (!node) {
      return null;
    }

    const cmp = this.compare(value, node.value);
    if (cmp < 0) {
      node.left = this.removeNode(node.left, value);
    } else if (cmp > 0) {
      node.right = this.removeNode(node.right, value);
    } else {
      this._size--;

      if (!node.left) {
        return node.right;
      }
      if (!node.right) {
        return node.left;
      }

      const minRight = this.findMin(node.right);
      node.value = minRight.value;
      node.right = this.removeNode(node.right, minRight.value);
      this._size++; // Compensate for the recursive remove
    }
    return node;
  }

  private findMin(node: BSTNode<T>): BSTNode<T> {
    while (node.left) {
      node = node.left;
    }
    return node;
  }

  private findMax(node: BSTNode<T>): BSTNode<T> {
    while (node.right) {
      node = node.right;
    }
    return node;
  }

  min(): T | undefined {
    if (!this.root) {
      return undefined;
    }
    return this.findMin(this.root).value;
  }

  max(): T | undefined {
    if (!this.root) {
      return undefined;
    }
    return this.findMax(this.root).value;
  }

  inorder(): T[] {
    const result: T[] = [];
    this.inorderTraverse(this.root, result);
    return result;
  }

  private inorderTraverse(node: BSTNode<T> | null, result: T[]): void {
    if (node) {
      this.inorderTraverse(node.left, result);
      result.push(node.value);
      this.inorderTraverse(node.right, result);
    }
  }

  preorder(): T[] {
    const result: T[] = [];
    this.preorderTraverse(this.root, result);
    return result;
  }

  private preorderTraverse(node: BSTNode<T> | null, result: T[]): void {
    if (node) {
      result.push(node.value);
      this.preorderTraverse(node.left, result);
      this.preorderTraverse(node.right, result);
    }
  }

  postorder(): T[] {
    const result: T[] = [];
    this.postorderTraverse(this.root, result);
    return result;
  }

  private postorderTraverse(node: BSTNode<T> | null, result: T[]): void {
    if (node) {
      this.postorderTraverse(node.left, result);
      this.postorderTraverse(node.right, result);
      result.push(node.value);
    }
  }

  isEmpty(): boolean {
    return this._size === 0;
  }

  size(): number {
    return this._size;
  }

  clear(): void {
    this.root = null;
    this._size = 0;
  }
}
