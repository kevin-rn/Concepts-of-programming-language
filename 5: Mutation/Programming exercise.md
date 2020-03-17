Implement the following example programs, to be interpreted by an interpreter for week 5:

    Boxes and variable mutation are two different ways to do mutation in the language:
        Write a 0-argument function that sets the identifier x to the value of the identifier y and returns false. You can assume x and y are in scope.
        Write a 0-argument function that sets the value of the box identified by x to the value in the box identified by y and returns false. You can assume x and y are in scope.
    Implement two mutually-recursive single-argument functions, even and odd. Your even function should return true if the input is 0, false if the input is 1, and otherwise use a recursive call of odd to check whether the number is odd. Your odd function should return false if the input is 0, true if the input is 1, and otherwise use a recursive call of even to check whether the number is even.

```scala
object Solution {

  /**
    * Write a 0-argument function that sets the identifier `x` to the value
    * of the identifier `y` and returns false.
    * You can assume `x` and `y` are in scope.
    */
  val variable = "(lambda () (seq  (set x y) false))"

   /**
     * Write a 0-argument function that sets the value of the box identified by `x`
     * to the value in the box identified by `y` and returns false.
     * You can assume `x` and `y` are in scope.
     */
  val box = "(lambda () (seq (setbox x (unbox y)) false))"

  /**
    * Implement two mutually-recursive single-argument functions, `even` and `odd`.
    *
    * Your `even` function should return `true` if the input is `0`,
    * `false` if the input is `1`, and otherwise use a recursive call of `odd`
    * to check whether the number is odd.
    *
    * Your `odd` function should return `false` if the input is `0`,
    * `true` if the input is `1`, and otherwise use a recursive call of `even`
    * to check whether the number is even.
    */
  val evenAndOdd =
    """
      (letrec ((even (lambda (n) (if (num= 0 n) true (if (num= 1 n) false (odd (- n 1))))))
               (odd (lambda (n) (if (num= 0 n) false (if (num= 1 n) true (even (- n 1)))))))
          (list even odd)
        )
    """
}
```

### Test:
````

//test: Test

import Untyped._
import Interpreter._

import org.scalatest.FunSuite

class Test extends FunSuite with CatchErrorSuite {

  test("even returns true for 2") {
    assertResult("true") {
      interp("((head " + Solution.evenAndOdd + ") 2)", Nil)
    }
  }

  test("even returns false for 1") {
    assertResult("false") {
      interp("((head " + Solution.evenAndOdd + ") 1)", Nil)
    }
  }

}
```
