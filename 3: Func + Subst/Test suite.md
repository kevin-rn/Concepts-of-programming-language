Develop a test suite for let-bindings and lambda’s using substitution. Include:

    Several tests to test good weather cases of interpreting let-bindings
    Several tests to test good weather cases of interpreting lambdas and substitution
    Several tests to test bad weather cases of interpreting let-bindings
    Several tests to test bad weather cases of interpreting lambdas and substitution
    Several tests to test bad weather cases of parsing let-bindings
    Several tests to test bad weather cases of parsing lambdas

An example of testing a good weather case:

test("Verify correct implementation") {
  assertResult(NumV(5)) {
    interp(desugar(parse("5")))
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
    interp(desugar(parse("(+ true 5)")))
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
  
  test("lambda-behaviour wrong order") {
    intercept[ParseException] {
      interp("(lambda (if (num= x 0) 0 1) (x))")
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
  
  test("lambda-behaviour name capture ") {
    assertResult(NumV(2)) {
      interp("((lambda (x) ((lambda (y) (* x y)) 1)) 2)")
    }
  }

  def parse(s: String): ExprExt
  def desugar(expr: ExprExt): ExprC
  def subst(expr: ExprC, bnds: List[Bind]): ExprC
  def interp(expr: ExprC): Value
  def interp(expr: String): Value = interp(desugar(parse(expr)))
}
```

