Develop a test suite for lazy evaluation, letrecs, and lists.

You can probably reuse some of the tests from week 4, but think of how the difference in representation affects the interpreter; and that the cases we test for differ from the cases tested for in the previous weeks.

WebLab will throw a RunningFailure in one of the following cases.
- Your test suite does not pass on a correct implementation.
- You have multiple tests with the same name.

```java
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
  
  /**
   * Test cases:
   */ 
  test("let-behaviour") {
    assertResult(ThunkV(Left(NumC(1), Nil))) {interp("(let ((x 1)) x)") }
  }
  
  test("lambda-behaviour ") {
    assertResult(NumV(5)) {
      interp("(let ((f (lambda (self x) (if (num= x 0) 0 (+ 1 (self self (- x 1))))))) (f f 5))")
    }
    
    intercept[InterpException] {
      interp("(let ((f1 (lambda (x) (f2 4))) (f2 (lambda (y) (+ x y)))) (f1 3))")
    }
    assertResult(ThunkV(Left((NumC(10), Nil)))) {
      interp("(((lambda (x) (lambda (x) x)) 5) 10)")
    }
  }
  
  test("list-behaviour") {
    assertResult(ThunkV(Left((NumC(5), Nil)))) { interp("(head (list 5 5 5))") }
    assertResult(ConsV(NumV(1), NilV())) {interp("(force ((lambda (x y) (cons x y)) 1 nil))") }
  }
  
  test("is-nil-behaviour") {
    intercept[InterpException]  { interp("(is-nil 1)") }
  }
  
  test("letrec-behaviour") {
    assertResult(ConsV(NumV(1),ConsV(NumV(1),NilV()))) {
      interp("""(letrec ((ones (cons 1 ones))
        (nats (cons 0 (zip-with plus ones nats)))
        (facs (cons 1 (zip-with times (tail nats) facs)))
        (plus (lambda (n m) (+ n m)))
        (times (lambda (n m) (* n m)))
        (zip-with (lambda (f xs ys) (if (and (is-nil xs) (is-nil ys)) nil 
                  (cons (f (head xs) (head ys)) (zip-with f (tail xs) (tail ys))))))
        (take (lambda (n xs) (if (num= n 0) nil (cons (head xs) (take (- n 1) (tail xs)))))))
        (force (take 2 facs)))""")
    }
    
    assertResult(NumV(1)) {
      interp("(letrec ((x (cons 1 nil)) (y (strict (is-nil x)))) (force (head x)))")
    } 
  }

  def parse(s: String): ExprExt
  def desugar(expr: ExprExt): ExprC
  def interp(expr: ExprC, env: List[Bind]): Value
  def interp(expr: String): Value = interp(desugar(parse(expr)), Nil)
  def force(v: Value): Value
  def strict(v: Value): Value
}


```
