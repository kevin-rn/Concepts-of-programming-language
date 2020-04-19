Develop a test suite for type inference. We test for 8 cases:

    Several tests to catch erroneous behavior of type inferring

An example of verifying a correct behavior:

test("Catch erroneous type inferring behavior") {
  intercept[TypeInferenceException] {
    typeOf("x", Nil)
  }
}

Your testsuite must pass on a correct implementation. If it does not, WebLab will throw a RunningFailure.

```scala
import org.scalatest.FunSuite
import Untyped._

abstract class Solution extends FunSuite { 
  test("Catch erroneous type inferring behavior") {
    intercept[TypeInferenceException] {
      typeOf("x", Nil)
    }
  }

  /**
    * Get the type of an expression.
    */
  def typeOf(e: String, nv: List[TBind]): Type

  /**
    * Interpret an expression with type-checking enabled.
    */
  def safeInterp(s: String): Value
}



```
