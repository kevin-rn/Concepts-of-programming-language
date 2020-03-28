Develop a test suite for type checking. You should include:

    Several tests to test good weather cases of type-checking
    Several tests to test bad weather cases of type-checking
    Two test cases to test bad weather cases of interpreting (safeInterp)

An example of testing a good weather case:
```scala
test("Verify correct types") {
  assertResult(NumT()) {
    typeOf("5")
  }
}

test("Verify correct interp behavior") {
  assertResult(NumV(3)) {
    safeInterp("(+ 1 2)")
  }
}
```
Note that safeInterp runs the type-checker before interpreting your code, and may thus throw a TypeException.

Examples of testing a bad weather case:
```scala
test("Catch erroneous types") {
  intercept[TypeException] {
    typeOf("x")
  }
}
```
Now that we have a type-checker, testing for runtime-errors is less essential. Think of what can still go wrong when interpreting programs that pass type-checking. There should not be many of there cases! Therefore, we only require you to have two tests that check for an InterpException:
```scala
test("Catch bad weather case interp") {
  intercept[InterpException] {
    safeInterp("(?????)")
  }
}
```
You will know when you have found these two cases when you no longer get hints about test cases for interpreting.


### Solution:
```scala
import org.scalatest.FunSuite
import Typed._

abstract class Solution extends FunSuite { 

  test("Verify correct type checking behavior") {
    assertResult(NumT()) {
      typeOf("5")
    }
  }

  test("Catch erroneous type checking behavior") {
    intercept[TypeException] {
      typeOf("x")
    }
  }

  test("Verify correct interp behavior") {
    assertResult(NumV(3)) {
      safeInterp("(+ 1 2)")
    }
  }
  
  
  /***
   * Type Check cases:
   */
  test("type check - lambda") {
    assertResult(FunT(List(NumT()), NumT())) { typeOf("(lambda ((n : Num)) n)") }
        assertResult(BoolT()) { typeOf("(((lambda ((x : Num)) (lambda ((x : Bool)) x)) 5) true)") }
  }
  
  test("type check - box") {
     assertResult(RefT(NumT())) { typeOf("((lambda ((n : Num)) (box n)) 5)") }
  }
  
  test("type check - rec-lam (fibonnaci)") {
    assertResult(NumT()) { 
      typeOf("((rec-lam (f : Num -> Num) (n) (if (num= n 1) 1 (* n (f(- n 1))))) 1)")
    }
  }
  
  test("type check - list") {
    assertResult(BoolT()) { typeOf("(head (list : Bool (true true false)))") }
  }
  
  test("type check - let") {
    assertResult(BoolT()) { typeOf("(let ((x 1)) (let ((x true)) x))") }
    assertResult(NumT()) { typeOf("(let ((x 1)) (let ((y (set x 2)) (z x)) z))") }
  }
  
  test("type check - bool") {
    assertResult(BoolT()) { typeOf("(let ((x true) (y false)) (and x y))") }
  }
  
 /***
  * Incorrect Type check cases
  */
  test("incorrect type check - application") {
    intercept[TypeException] { typeOf("((lambda ((x : Num)) x) true)") } 
    intercept[TypeException] { typeOf("((lambda () true) 2)") } 
  }
  
  test("incorrect type check - operators") {
    intercept[TypeException] { typeOf("(+ 1 true)") }
    intercept[TypeException] { typeOf("(and 1 1)") }
    intercept[TypeException] { typeOf("(num= true false)") }
  }
  
  test("incorrect type check - list operations") {
    intercept[TypeException] { typeOf("(head 5)") }
    intercept[TypeException] { typeOf("(tail 5)") }
    intercept[TypeException] { typeOf("(is-nil 5)") } //-
    intercept[TypeException] { typeOf("(list : Num (true false))") } //-
  }

  test("incorrect type check - if") {
    intercept[TypeException] { typeOf("(if 1 1 1)") }
    intercept[TypeException] { typeOf("(if false true 1)") }
    intercept[TypeException] { typeOf("(if false 1 true)") } //-
  }
  
  test("incorrect type check - cons & boolean nil") {
    intercept[TypeException] { typeOf("(cons 5 (nil : Bool))") }

  }
  
  /**
    * Think about this for a while,
    * a lot of bad behavior will now throw a
    * TypeException instead of InterpException!
    * What things can still actually cause an
    * exception on runtime, without causing a
    * TypeException?
    *
    * There will not be many cases!
    * This relates to the concept of type soundness.
    */
  test("Catch erroneous interp behavior - head of nil") {
    intercept[InterpException] {
      safeInterp("(head (nil : Num))")
    }
  }
  
  test("Catch erroneous interp behavior - tail of nil") {
    intercept[InterpException] {
      safeInterp("(tail (nil : Num))")
    }
  }

  /**
    * Get the type of an expression.
    */
  def typeOf(e: String): Type = typeOf(e, Nil)
  def typeOf(e: String, nv: List[TBind]): Type

  /**
    * Interpret an expression with type-checking enabled.
    */
  def safeInterp(s: String): Value

}
```
