import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class BinarySearchTreeSpec extends FunSpec with BeforeAndAfter with Matchers {

  var nullTree, noChildTree, oneLeftTree, onlyLeftTree, oneRightTree, onlyRightTree, twoChildTree, largeTree: BinarySearchTree = _
  var nullTreeSize, noChildTreeSize, oneLeftTreeSize, onlyLeftTreeSize, oneRightTreeSize, onlyRightTreeSize, twoChildTreeSize, largeTreeSize: Int = 0

  private def concurrentInsertTree(trees: Array[BinarySearchTree], value: Int): Unit = {
    trees.foreach{_.insert(value)}
  }

  private def concurrentInsertNums(tree: BinarySearchTree, values: Array[Int]): Unit = {
    values.foreach{tree.insert}
  }


  before {
    nullTree = new BinarySearchTree
    noChildTree = new BinarySearchTree
    oneLeftTree = new BinarySearchTree
    onlyRightTree = new BinarySearchTree
    onlyLeftTree = new BinarySearchTree
    oneRightTree = new BinarySearchTree
    twoChildTree = new BinarySearchTree
    largeTree = new BinarySearchTree
    concurrentInsertTree(Array(noChildTree, oneLeftTree, onlyLeftTree, oneRightTree, onlyRightTree, twoChildTree, largeTree), 100)
    concurrentInsertTree(Array(oneLeftTree, onlyLeftTree, twoChildTree, largeTree), 50)
    concurrentInsertTree(Array(oneRightTree, onlyRightTree, twoChildTree, largeTree), 150)
    concurrentInsertTree(Array(onlyLeftTree, largeTree), 25)
    concurrentInsertTree(Array(onlyLeftTree, largeTree), 15)
    concurrentInsertTree(Array(onlyRightTree, largeTree), 200)
    concurrentInsertTree(Array(onlyRightTree, largeTree), 250)
    concurrentInsertNums(largeTree, Array(75, 45, 60, 90, 125, 110, 130, 160))
    nullTreeSize = nullTree.size()
    noChildTreeSize = noChildTree.size()
    oneLeftTreeSize = oneLeftTree.size()
    onlyLeftTreeSize = onlyLeftTree.size()
    oneRightTreeSize = oneRightTree.size()
    onlyRightTreeSize = onlyRightTree.size()
    twoChildTreeSize = twoChildTree.size()
    largeTreeSize = largeTree.size()
  }

  describe("insert") {
    it("should add element to tree if the value is not in the tree") {
      nullTree.size() shouldEqual nullTreeSize
      nullTree.insert(100)
      nullTree.size() shouldEqual nullTreeSize + 1
      nullTree.insert(100)
      nullTree.size() shouldEqual nullTreeSize + 1
      largeTree.insert(90)
      largeTree.size() shouldEqual largeTreeSize
      largeTree.insert(105)
      largeTree.size() shouldEqual largeTreeSize + 1
    }
  }

  describe("remove") {

    it("should can not remove the element if the node is null or the tree does not contain the value") {
      nullTree.remove(100)
      nullTree.size() shouldEqual nullTreeSize
      noChildTree.remove(900)
      noChildTree.size() shouldEqual noChildTreeSize
      onlyLeftTree.remove(200)
      onlyLeftTree.size() shouldEqual onlyLeftTreeSize
      largeTree.remove(898)
      largeTree.size() shouldEqual largeTreeSize
    }

    it("should remove element from tree if the node need to be removed has no children") {
      noChildTree.remove(100)
      noChildTree.size() shouldEqual noChildTreeSize - 1
      oneLeftTree.remove(50)
      oneLeftTree.size() shouldEqual oneLeftTreeSize - 1
      oneRightTree.remove( 150)
      oneRightTree.size() shouldEqual oneRightTreeSize - 1
      onlyLeftTree.remove(15)
      onlyLeftTree.size() shouldEqual onlyLeftTreeSize - 1
      onlyRightTree.remove(250)
      onlyRightTree.size() shouldEqual onlyRightTreeSize - 1
      largeTree.remove(90)
      largeTree.size() shouldEqual largeTreeSize - 1
      //largeTree.printTree()
    }

    it("should remove element from tree if the node only has left child") {
      onlyLeftTree.remove(25)
      onlyLeftTree.size() shouldEqual onlyLeftTreeSize - 1
    }

    it("should remove element from tree if the node only has right child") {
      onlyRightTree.remove(200)
      onlyRightTree.size() shouldEqual onlyRightTreeSize - 1
    }

    it("should remove the node in left with 2 children in perfect tree") {
      largeTree.printTree()
      largeTree.remove(50)
      largeTree.size() shouldEqual largeTreeSize - 1
      largeTree.printTree()
    }

    it("should remove the node in right with 2 children in perfect tree") {
      largeTree.printTree()
      largeTree.remove(150)
      largeTree.size() shouldEqual largeTreeSize - 1
      largeTree.printTree()
    }
  }
  describe("takeOutMaxLeft") {
    it("should take out the max code for left child only has lots of only left child") {
      nullTree.insert(100)
      nullTree.insert(110)
      nullTree.insert(80)
      nullTree.insert(40)
      nullTree.insert(20)
      nullTree.takeOutMaxLeft(nullTree.root, nullTree.root.left, true).key shouldEqual 80
      //nullTree.printTree()
    }

    it("should take out the max code for left child only has one right no-children child") {
      nullTree.insert(100)
      nullTree.insert(110)
      nullTree.insert(80)
      nullTree.insert(90)
      nullTree.takeOutMaxLeft(nullTree.root, nullTree.root.left, true).key shouldEqual 90
      //nullTree.printTree()
    }

    it("should take out the max code for left child only has one right only-left-child child") {
      nullTree.insert(100)
      nullTree.insert(110)
      nullTree.insert(80)
      nullTree.insert(90)
      nullTree.insert(20)
      nullTree.insert(85)
      nullTree.takeOutMaxLeft(nullTree.root, nullTree.root.left, true).key shouldEqual 90
      //nullTree.printTree()
    }

    it("should take out the max code for left child only has one right only-right-child child") {
      nullTree.insert(100)
      nullTree.insert(110)
      nullTree.insert(80)
      nullTree.insert(90)
      nullTree.insert(20)
      nullTree.insert(95)
      nullTree.takeOutMaxLeft(nullTree.root, nullTree.root.left, true).key shouldEqual 95
      //nullTree.printTree()
    }

    it("should take out the max code for left child only has one right 2-children child") {
      nullTree.insert(100)
      nullTree.insert(110)
      nullTree.insert(80)
      nullTree.insert(90)
      nullTree.insert(20)
      nullTree.insert(95)
      nullTree.insert(85)
      nullTree.takeOutMaxLeft(nullTree.root, nullTree.root.left, true).key shouldEqual 95
      //nullTree.printTree()
    }

    it("should take out the max code for left child only has one left 2-children child") {
      nullTree.insert(100)
      nullTree.insert(80)
      nullTree.insert(90)
      nullTree.insert(20)
      nullTree.insert(85)
      nullTree.insert(83)
      nullTree.insert(86)
      nullTree.takeOutMaxLeft(nullTree.root, nullTree.root.left, true).key shouldEqual 90
      //nullTree.printTree()
    }
  }
}