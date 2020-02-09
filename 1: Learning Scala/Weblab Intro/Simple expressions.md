In the “Solution” replace the comments with a statement that returns the addition of 40 and 2 and the multiplication of integers 3 and 4. You can ignore the “Test” editor for now. Verify that everything is working ([Save] & [▶ Spec-test]) and then submit your solution.

```scala
// the solution 

object Solution { 

  def fortyPlusTwo() : Int = {
    // your expression here
    42
  }

  def threeTimesFour() : Int = {
    // your expression here
    12
  }
}
```

```scala
//test: Test

import Solution._

import org.scalatest.FunSuite

class Test extends FunSuite {

  test("Should be 42") {
    assertResult(42) {
      fortyPlusTwo()
    }
  }

}

```
