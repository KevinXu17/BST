import scala.collection.mutable.ArrayBuffer

class Node (val key: Int) {
  var left, right : Node = null
}
class BinarySearchTree {
  var root: Node = null

  def insert(value: Int): Unit = {
    if (!search(value))
    root = insertRec(root, value)
  }

  private def insertRec(root: Node, value: Int): Node = {
    root match {
      case null => new Node(value)
      case root if root.key > value =>
        root.left = insertRec(root.left, value)
        root
      case root if root.key < value =>
        root.right = insertRec(root.right, value)
        root
    }
  }


  def remove(value: Int): Unit = {
    val parent: Node = new Node(Int.MinValue)
    var isLeftChild: Boolean = true
    parent.left = root
    removeRec(parent, root, value, isLeftChild)
    root = parent.left
  }



  private def removeRec(parent: Node, child: Node, value: Int, isLeftChild: Boolean): Unit = {
    child match {
        // 1. child is null
      case null =>
      // 2.1 child has no children
      case c if c.left == null && c.right == null => {
        if (c.key == value) {
          if (isLeftChild) parent.left = null else parent.right = null
        }
      }
        // 2.2 child only has left child
      case c if c.right == null => {
        if (c.key == value) {
          if (isLeftChild) parent.left = child.left else parent.right = child.left
        } else if (c.key > value) {
          removeRec(child, child.left, value,true)
        }
      }
        // 2.3 child only has right child
      case c if c.left == null => {
        if (c.key == value) {
          if (isLeftChild) parent.left = child.right else parent.right = child.right
        } else if (c.key < value) {
          removeRec(child, child.right, value, false)
        }
      }
        // 3. child has 2 children : use max node in left branch
      case c => {
        if (c.key == value) {
          val maxLeftNode = takeOutMaxLeft(child, child.left, true)
          if (isLeftChild) parent.left = maxLeftNode else parent.right = maxLeftNode
          maxLeftNode.left = child.left
          maxLeftNode.right = child.right
        } else if (c.key > value) {
          removeRec(child, child.left, value,true)
        } else {
          removeRec(child, child.right, value, false)
        }
      }
    }
  }

  def takeOutMaxLeft(parent: Node, child: Node, isLeftChild: Boolean): Node = {
    var p: Node = parent
    var c: Node = child
    var isLeft: Boolean = isLeftChild
    while (c.right != null) {
      p = c
      c = c.right
      isLeft = false
    }
    if (isLeft) p.left = c.left else p.right = c.left
    c
  }

  def search(value: Int): Boolean = {
    searchRec(root: Node, value: Int)
  }

  private def searchRec(root: Node, value: Int): Boolean = {
    root match {
      case null => false
      case root if root.key > value => searchRec(root.left, value)
      case root if root.key < value => searchRec(root.right, value)
      case _ => true
    }
  }

  def printTree(): Unit = {
    if (root != null) {
      val treeBuffer: ArrayBuffer[Int] = ArrayBuffer()
      println(printTreeRec(root, treeBuffer).toArray.mkString(","))
    }
  }

  private def printTreeRec(root: Node, treeBuffer: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    if (root != null) {
      if (root.left == null) {
        treeBuffer += root.key
        printTreeRec(root.right, treeBuffer)
      } else {
        printTreeRec(root.left, treeBuffer)
        treeBuffer += root.key
        printTreeRec(root.right, treeBuffer)
      }
    }
    treeBuffer
  }

  def size(): Int = {
    sizeRec(root, 0)
  }

  private def sizeRec(node: Node, size: Int): Int = {
    if (node == null) size else {
      val updateSize = size + 1
      val updateLeftSize = sizeRec(node.left, updateSize)
      sizeRec(node.right, updateLeftSize)
    }
  }

  def findMax(): Int = {
    var max: Node = root
    if (max != null) {
      while (max.right != null) {
        max = max.right
      }
      max.key
    } else -1
  }
}