Develop a test suite for lists and the list operations described in the course notes. You should have tests for validating the correct behavior of each operator for lists; i.e., cons, nil, head, tail, is-nil, is-list, and list.

An example of verifying a correct behavior:

test("Verify correct implementation") {
  assertResult(NumV(5)) {
    interp(desugar(parse("5")))
  }
}

An example of catching an erroneous behavior:

test("Catch erroneous parse behavior") {
  intercept[ParseException] {
    parse("()")
  }
}

test("Catch erroneous interp behavior") {
  intercept[InterpException] {
    interp(desugar(parse("(+ true 5)")))
  }
}

WebLab will throw a RunningFailure in one of the following cases.
- Your test suite does not pass on a correct implementation.
- You have multiple tests with the same name.

Note: You may only run the spec-tests.

```scala
import org.scalatest.FunSuite
import Library._
import Untyped._

abstract class Solution extends FunSuite { 

  test("Verify correct implementation") {
    assertResult(NumV(5)) {
      interp(desugar(parse("5")))
    }
  }

  test("Catch erroneous parse behavior") {
    intercept[ParseException] {
      parse("()")
    }
  }

  test("Catch erroneous interp behavior") {
    intercept[InterpException] {
      interp(desugar(parse("(+ true 5)")))
    }
  }
  
  test("cons") {
    assertResult(ConsV(NumV(1), NumV(2))) {
      interp(desugar(parse("(cons 1 2)")))
    }
  }
  
  test("nil") {
      assertResult(NilV()) {
        interp(desugar(parse("nil")))
    }
  }
  
  test("head correct") {
      assertResult(NumV(1)) {
        interp(desugar(parse("(head (cons 1 2))")))
    }
  }
  
  test("head incorrect") {
    intercept[InterpException] {
      interp(desugar(parse("(head (+ 1 1))")))
    }
  }
  
  test("tail correct") {
      assertResult(NumV(2)) {
      interp(desugar(parse("(tail (cons 1 2))")))
    }
  }
  
  test("tail incorrect") {
    intercept[InterpException] {
      interp(desugar(parse("(tail (+ 1 1))")))
    }
  }
  

  test("is-nil correct true") {
      assertResult(BoolV(true)) {
        interp(desugar(parse("(is-nil nil)")))
    }
  }
  
  test("is-nil correct false") {
      assertResult(BoolV(false)) {
        interp(desugar(parse("(is-nil (cons 1 2))")))
    }
  }
  
  test("is-nil incorrect") {
    intercept[InterpException] {
      interp(desugar(parse("(is-nil (+ 1 1))")))
    }
  }
  
  test("is-list false") {
      assertResult(BoolV(false)) {
        interp(desugar(parse("(is-list (+ 2 2))")))
    }
  }
  
  test("is-list true") {
      assertResult(BoolV(true)) {
        interp(desugar(parse("(is-list (cons 1 2))")))
    }
  }
  
    test("is-list nil") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-list nil)"))) 
    }
  }
  
  test("list correct") {
    assertResult(ConsV(NumV(1), ConsV(NumV(2), ConsV(NumV(3), NilV())))) {
      interp(desugar(parse("(list 1 2 3)")))
    }
  }
  
  def parse(s: String): ExprExt
  def desugar(expr: ExprExt): ExprC
  def interp(expr: ExprC): Value
}


```
