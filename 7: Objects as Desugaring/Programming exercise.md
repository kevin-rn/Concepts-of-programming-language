In this assignment you will create two objects in a language that supports objects as desugaring.
Clock Object

Implement a “clock” object.
The object must have a single field, and three methods:

    The field should contain a number which is initially 0.
    The 0-argument “tick” method should increment the number by 1.
    The 0-argument “reset” method should reset the clock to 0.
    The 0-argument “read” method should return the current clock value.

Clock Factory Object

Implement a “clock factory” object.
The clock factory object must have a single field, and two methods:

    The field should contain a number which is initially 0. The value of this field decides what the initial value is for the clocks that the factory produces. (See the test template for examples.)
    The “set-init” method updates the field of the clock factory (and hence the initial value of produced clocks).
    The “produce” method returns a “clock” object (where a “clock” object is as defined above).

### Tests:
```scala
//test: Test

import Untyped._
import Interpreter._

import org.scalatest.FunSuite

class Test extends FunSuite with CatchErrorSuite {

  // uses Scala string interpolation;
  // see, e.g., https://docs.scala-lang.org/overviews/core/string-interpolation.html
  def withClockObject(prog: String): String =
    s"""
      (let ((clock ${Solution.clock}))
        $prog)
    """

  def withClockFactory(prog: String): String =
    s"""
      (let ((clock-factory ${Solution.clockFactory}))
        $prog)
    """

  test("clock can tick") {
    assertResult("1") {
      interp(withClockObject(
        """
           (do-seq (msg clock tick)
                   (msg clock read))
        """), Nil)
    }
  }

  test("clock can tick twice") {
    assertResult("2") {
      interp(withClockObject(
        """
           (do-seq (msg clock tick)
                   (msg clock tick)
                   (msg clock read))
        """), Nil)
    }
  }

  test("clock can tick twice and reset") {
    assertResult("(cons 2 (cons 0 nil))") {
      interp(withClockObject(
        """
           (do-seq (msg clock tick)
                   (msg clock tick)
                   (cons
                     (msg clock read)
                     (cons
                       (seq (msg clock reset)
                            (msg clock read))
                       nil)))
        """), Nil)
    }
  }

  test("clock factory can produce clocks that tick") {
    assertResult("1") {
      interp(withClockFactory(
        """
          (let ((clock (msg clock-factory produce)))
            (do-seq (msg clock tick)
                    (msg clock read)))
        """
      ), Nil)
    }
  }

  test("clock factory can produce clocks with an initial value") {
    assertResult("42") {
      interp(withClockFactory(
        """
          (do-seq
            (msg clock-factory set-init 41)
            (let ((clock (msg clock-factory produce)))
              (do-seq (msg clock tick)
                      (msg clock read))))
        """
      ), Nil)
    }
  }

}


```

### Solution:
```scala
object Solution {

  /**
    * Implement a "clock" object.
    * The object must have a single field, and three methods:
    * - The field should contain a number which is initially 0.
    * - The "tick" method should increment the number by 1.
    * - The "reset" method should reset the clock to 0.
    * - The "read" method should return the current clock value.
    */
  val clock =
    """
      (object
        ((field counter 0))
        ((method tick () (set counter (+ counter 1)))
        (method reset () (set counter 0))
        (method read () counter))
        )
    """

  /**
    * Implement a "clock factory" object.
    * The clock factory object must have a single field, and two methods:
    * - The field should contain a number which is initially 0.  The value of this field decides
    *     what the initial value is for the clocks that the factory produces.  (See the test
    *     template for examples.)
    * - The "set-init" method updates the field of the clock factory (and hence the initial value
    *     of produced clocks).
    * - The "produce" method returns a "clock" object (where a "clock" object is as defined above).
    */
  val clockFactory =
    """
      (object
        ((field init 0))
        ((method set-init (x) (set init x))
        (method produce () (object ((field counter init)) 
          ((method tick () (set counter (+ counter 1))) 
          (method reset () (set counter 0))
          (method read () counter))
        )))
        )
    """

}


```
