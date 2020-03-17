Develop a test suite for let-bindings and lambda’s using environments.
You can probably reuse some of the tests from last week, but think of how the difference in representation affects the interpreter; and that the cases we test for differ from the cases tested for in the previous week.
You need several tests to:
1. test good weather cases of interpreting let-bindings
2. test good weather cases of interpreting lambdas
3. test good weather cases of interpreting rec-lam
4. test bad weather cases of interpreting let-bindings
5. test bad weather cases of interpreting lambdas
6. test bad weather cases of interpreting rec-lam
7. test bad weather cases of parsing let-bindings
8. test bad weather cases of parsing lambdas
9. test bad weather cases of parsing rec-lam

An example of testing a good weather case:

test("Verify correct implementation") {
  assertResult(NumV(5)) {
    interp("5")
  }
}

or with pre-defined bindings in scope:

test("Verify correct implementation") {
  assertResult(NumV(5)) {
    interp(desugar(parse("x")), List(Bind("x", NumV(5))))
  }
}

Examples of testing a bad weather case:

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
  
  test("let-behaviour correct") {
    assertResult(NumV(3)) {
      interp("(let ((x 1) (y 2)) (+ x y))")
    }
  }
  
    test("let-behaviour same identifier") {
      intercept[ParseException]  {
      parse("(let ((y 1) (y 2)) (* y y))")
    }
  }
  
  test("let-behaviour infinite scope") {
    intercept[ParseException] {
      parse("(let ([quadruple (lambda (x) (double (double x)))])(let ([double (lambda (x) (+ x x))])(quadruple 10)))")
    }
  }
  
  test("let-behaviour empty binding") {
    intercept[ParseException] {
      parse("(let () 1)")
    }
  }
  
  test("let-behaviour wrong identifier") {
    intercept[InterpException]  {
        interp("(let ((x 5) (z x)) (* x z))")
    }
  }
  
  test("let-behaviour using reserved words") {
    intercept[ParseException] {
      parse("(let ((+ (x 5)) (y 5)) (* x y))")
    }
  }
  
  test("lambda-behaviour correct") {
    assertResult(NumV(5)) {
      interp("(let ((f (lambda (self x) (if (num= x 0) 0 (+ 1 (self self (- x 1))))))) (f f 5))")
    }
  }
  
  test("lambda-behaviour wrong identifier") {
    intercept[ParseException] {
      interp("(lambda (x x) x)")
    }
  }
  
  test("Lambda-behaviour wrong usage") {
      intercept[ParseException] {
        parse("(lambda (nil) (is-nil y))")
    }
  }
  
  test("lambda-behaviour multiple arguments") {
    intercept[InterpException] {
      interp("((lambda (x) x) 5 10)")
    }
  }

  test("lambda-behaviour shadowing") {
    assertResult(NumV(10)) {
      interp("(((lambda (x) (lambda (x) x)) 5) 10)")
    }
  }
  
  test("lambda-behaviour scope") {
    intercept[InterpException] {
      interp("(let ((f1 (lambda (x) (f2 4))) (f2 (lambda (y) (+ x y)))) (f1 3))")
    }
  }

  test("rec-lam-behaviour correct") {
    assertResult(NumV(6)) {
      interp("((rec-lam sum (n) (if (num= n 0) 0 (+ n (sum (- n 1))))) 3)")
    }
  }
  
  test("rec-lam-behaviour reserved word function") {
    intercept[ParseException] {
      interp("(rec-lam not (n) (if (num= n 0) 0 (not (- n 1))))")
    }
  }

  test("rec-lam-behaviour reserved word value") {
    intercept[ParseException] {
      interp("(rec-lam foo (nil) (if (num= n 0) 0 (foo (- n 1))))")
    }
  }
 

  def parse(s: String): ExprExt
  def desugar(expr: ExprExt): ExprC
  def interp(expr: ExprC, env: List[Bind]): Value
  def interp(expr: String): Value = interp(desugar(parse(expr)), Nil)
  
}


```
