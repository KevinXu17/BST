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

  describe("findMax") {
    it("should return -1 if root is null") {
      nullTree.findMax() shouldEqual -1
    }
    it("should return correct max key value if root is not null") {
      noChildTree.findMax() shouldEqual 100
      oneLeftTree.findMax() shouldEqual 100
      oneRightTree.findMax() shouldEqual 150
      onlyLeftTree.findMax() shouldEqual 100
      onlyRightTree.findMax() shouldEqual 250
      largeTree.findMax() shouldEqual 250
    }
  }

  describe("findMin") {
    it("should return -1 if root is null") {
      nullTree.findMin() shouldEqual -1
    }

    it("should return correct min key value if root is not null") {
      noChildTree.findMin() shouldEqual 100
      oneLeftTree.findMin() shouldEqual 50
      oneRightTree.findMin() shouldEqual 100
      onlyLeftTree.findMin() shouldEqual 15
      onlyRightTree.findMin() shouldEqual 100
      largeTree.findMin() shouldEqual 15
    }
  }

  describe("maxHeight") {
    it("should return 0 if root is null") {
      nullTree.maxHeight(nullTree.root) shouldEqual 0
    }

    it("should return correct height of tree") {
      noChildTree.maxHeight(noChildTree.root) shouldEqual 1
      twoChildTree.maxHeight(twoChildTree.root) shouldEqual 2
      oneLeftTree.maxHeight(oneLeftTree.root) shouldEqual 2
      oneRightTree.maxHeight(oneRightTree.root) shouldEqual 2
      onlyLeftTree.maxHeight(onlyLeftTree.root) shouldEqual 4
      onlyRightTree.maxHeight(onlyRightTree.root) shouldEqual 4
      largeTree.maxHeight(largeTree.root) shouldEqual 4
    }
  }

  describe("diffOfHeight") {
    it("should return 0 if root is null") {
      nullTree.diffOfHeight(nullTree.root) shouldEqual 0
    }

    it("should return 0 if tree is balanced") {
      noChildTree.diffOfHeight(noChildTree.root) shouldEqual 0
      twoChildTree.diffOfHeight(twoChildTree.root) shouldEqual 0
      largeTree.diffOfHeight(largeTree.root) shouldEqual 0
      oneLeftTree.diffOfHeight(oneLeftTree.root) shouldEqual 0
      oneRightTree.diffOfHeight(oneRightTree.root) shouldEqual 0
    }

    it("should return -1 if left tree is heavier") {
      onlyLeftTree.diffOfHeight(onlyLeftTree.root) shouldEqual -1
    }

    it("should return 1 if right tree is heavier") {
      onlyRightTree.diffOfHeight(onlyRightTree.root) shouldEqual 1
    }
  }
}
