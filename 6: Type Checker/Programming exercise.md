Implement the following example programs, to be interpreted by an interpreter for week 6:

    Write an expression that implements a well-typed factorial function. The expression should have type ((Num) -> Num).
    Write an expression of type (Pair Num (Pair Bool ((Num) -> Num))).
    Write an expression of type (List Num)
    Write an expression of type ((Num) -> (Ref Num))

### Test:
```scala
//test: Test

import org.scalatest.FunSuite
import Interpreter._

class Test extends FunSuite with CatchErrorSuite {

  /**
    * This test demonstrates how to to interpret expressions that contain identifiers.
    *
    * By giving the interp-method a list of (identifier, expression) tuples,
    * these will be bound to the scope of your program
    */
  test("If statement with identifiers") {
    val bindings = List(("x", "true"), ("y", "1"), ("z", "2"))
    assertResult("NumV(1)") {
      interp("(if x y z)", bindings)
    }
    assertResult("NumT()") {
      typeOf("(if x y z)", bindings)
    }
  }

}


```


### Solution:
```scala
object Solution { 

  /**
    * Write an expression that implements a well-typed factorial function.
    * The expression should have type `((Num) -> Num)`.
    */
  def fact = "(rec-lam (fact : Num -> Num) (n) (if (num= n 1) 1 (* n (fact(- n 1)))))"

  /**
    * Write an expression of type `(Pair Num (Pair Bool ((Num) -> Num)))`.
    */
  def pairs = "(pair 1 (pair true (lambda ((n : Num)) n)))"

  /**
    * Write an expression of type `(List Num)`
    */
  def list = "(list : Num (1 2))"

  /**
    * Write an expression of type `((Num) -> (Ref Num))`
    */
  def refFun = "(lambda ((n : Num)) (box n))"
}
```
