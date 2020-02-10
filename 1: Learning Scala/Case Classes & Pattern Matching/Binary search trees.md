These exercise focuses on defining and using a binary search tree using case classes.

The “Test” tab only contains tests for insert. 
You are encouraged (but not required) to write tests for contains, and size.

```scala
object Solution {
  /**
    * EXERCISE:
    * Define the case classes for Tree.
    * A tree can be a Leaf, or a Node with a value and two child trees.
    * The height function serves as an example for how the tree structure should be organized
    */
  sealed abstract class Tree   
  case class Leaf() extends Tree //Empty case
  case class Node(elem: Int, left: Tree, right: Tree) extends Tree //Element

  def height(tree: Tree): Int = tree match {
    case Leaf() => 0
    case Node(elem, left, right) => 1 + Math.max(height(left), height(right))
  }


  /**
    * EXERCISE:
    * Define the following functions.
    *
    * Do not worry about rebalancing the tree.
    *
    * Hint:
    * Pattern matches can be made more specific with arbitrary conditions:
    *     `case A(n) if n>3 => print("n is greater than 3!")`
    */
  def insert(e: Int, t: Tree): Tree = t match {
    case Leaf() => Node(e, Leaf(), Leaf())
    case Node(elem, left, right) if(elem ==e) => Node(e, left, right) 
    case Node(elem, left, right) => if(e < elem) Node(elem, insert(e, left), right) 
                          else Node(elem, left, insert(e, right))
    
  }

  def contains(e: Int, t: Tree): Boolean = t match {
    case Leaf() => false
    case Node(elem, left, right) if(e == elem) => true
    case Node(elem, left, right) => if(e > elem) contains(e, right) 
                        else contains(e, left)
    
  }

  def size(t: Tree): Int = t match { 
    case Leaf() => 0
    case Node(elem, left, right) => 1 + size(left) + size(right)
    
  }
}


```

```scala
//test: StudentTest

import Solution._
import org.scalatest.FunSuite

class StudentTest extends FunSuite {

  test("insert-1") {
    assertResult(Node(5, Leaf(), Leaf())) {
      insert(5, Leaf())
    }
  }

  test("insert-2") {
    assertResult(Node(5, Leaf(), Leaf())) {
      insert(5, Node(5, Leaf(), Leaf()))
    }
  }

}
```
