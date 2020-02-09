These exercise focuses on defining and using a binary search tree using case classes.

The “Test” tab only contains tests for insert. 
You are encouraged (but not required) to write tests for contains, and size.

```scala

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
