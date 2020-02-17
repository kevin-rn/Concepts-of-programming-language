Implement the two example programs following the instructions in the solution template. This exercise does not count towards your grade, but provides a testing ground for you to experiment with the language you will be implementing this week.

```scala
object Solution {

  /**
    * Write an s-expression whose result is 42.
    */
  def is42 = "42"

  /**
    * Write an s-expression that evaluates to a list containing the numbers
    * between and including 0 and 3.
    */
  def between0and3 = "(list 0 1 2 3)"

  /**
    * You can also add your own defs below and test them by calling them
    * from your own set of tests (under the "Test" tab).
    *
    * This way you can validate your understanding of a given concept.
    */

}
```

```scala
//test: Test

import Interpreter._
import org.scalatest.FunSuite

class Test extends FunSuite with CatchErrorSuite {

  /**
    * Test that your solution programs meet their specifications
    */

  test("is42 is 42") {
    assertResult("42") {
      interp(Solution.is42)
    }
  }

  test("between0and3 is all the numbers between 0 and 3") {
    assertResult("(cons 0 (cons 1 (cons 2 (cons 3 nil))))") {
      interp(Solution.between0and3)
    }
  }

  /**
    * You can also add your own tests below that call defs in your solution.
    */

}


```
