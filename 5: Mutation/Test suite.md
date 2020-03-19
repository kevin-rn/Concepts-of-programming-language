Develop a test suite for mutation. Include:

    Several tests to test good weather cases of interpreting mutation
    Several tests to test bad weather cases of interpreting mutation

For robustness, the interp function “hides” the output store of the interpreter. This way, tests do not depend on how the store is laid out.

An example of testing a good weather case:
```scala
test("Verify correct implementation") {
  assertResult(NumV(5)) {
    interp("5")
  }
}
```
or with pre-defined bindings in scope:
```scala
test("Verify correct implementation") {
  assertResult(NumV(5)) {
    interp(desugar(parse("x")), List(Bind("x", NumV(5))))
  }
}
```
Examples of testing a bad weather case:
```scala
test("Catch bad weather case parse") {
  intercept[ParseException] {
    parse("()")
  }
}

test("Catch bad weather case interp") {
  intercept[InterpException] {
    interp("(+ true 5)")
  }
}
```
### Solution:
```scala
import org.scalatest.FunSuite
import Untyped._

abstract class Solution extends FunSuite { 

  test("Verify correct implementation") {
    assertResult(NumV(5)) {
      interp("5")
    }
  }

  test("Catch erroneous parse behavior") {
    intercept[ParseException] {
      parse("()")
    }
  }

  test("Catch erroneous interp behavior") {
    intercept[InterpException] {
      interp("(+ true 5)")
    }
  }
  
  test("letrec-behaviour fibonnaci") {
    assertResult(NumV(5)) {
      interp("""(letrec ((fib (lambda (n) (if (num= n 0) 0 (if (num= n 1) 1  
          (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 5))""")
    }
  }
  
  test("set-behaviour scope") {
     assertResult(BoolV(true)) {
      interp("(letrec ((x 1) (y (set x true))) (or x y))")
    }
  }
  
  test("application-behaviour even-odd") {
    assertResult(BoolV(false)) {
      interp("""((head (letrec ((even (lambda (n) (if (num= 0 n) true (if (num= 1 n) false (odd (- n 1))))))
               (odd (lambda (n) (if (num= 0 n) false (if (num= 1 n) true (even (- n 1)))))))
          (list even odd))) 5)""")
    }
  }

  test("let-behaviour scope") {
    assertResult(NumV(2)) {
      interp("(let ((x 1)) (let ((y (set x 2)) (z x)) z))")
    }
    }
  
  def parse(expr: String): ExprExt
  def desugar(expr: ExprExt): ExprC
  // Binds are automatically converted to Pointers with Cells
  def interp(expr: ExprC, env: List[Bind]): Value
  def interp(s: String): Value = interp(desugar(parse(s)), Nil)
}
```
