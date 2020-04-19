Implement a type checker for a language (grammar below) with arithmetic, functions, lists, and a lettype expression allowing programmers to defining a named alias for a type.

The solution template defines the ExprExt and Type classes that your typeOf function accepts as input and returns as result, respectively.

Implement the missing functionality (i.e., all occurrences of throw NotImplementedException()) in the solution template.

For type errors, you should use the following predefined case class (which cannot be extended):

case class TypeException(msg: String) extends RuntimeException(msg)

Note: The typeOf function in the solution template is parameterized by two type environments. nv: List[TBind] is a “regular” type environment, while tnv: List[TVBind] is for type aliases. During type checking, type aliases must be normalized to their definition. You should implement and use the tlookup and normalizeType functions for this.

The “Test” tab contains example programs that should type check using your typeOf function.
Grammar

<expr> ::= <num>
         | (+ <expr> <expr>)
         | (* <expr> <expr>)
         | (lambda (<id> : <type>) <expr>)
         | (<expr> <expr>)
         | <id>
         | (cons <expr> <expr>)
         | (nil : <type>)
         | (head <expr>)
         | (tail <expr>)
         | (lettype (<id> <type>) <expr>)

<type> ::= Num
         | (List <type>)
         | (<type> -> <type>)
         | <id>

### Template:
```scala
object TypeCheck {
  
  def typeOf(e: ExprExt, nv: List[TBind], tnv: List[TVBind]): Type = e match {
    case NumExt(n) => NumT()
    case PlusExt(e1, e2) =>
      (typeOf(e1, nv, tnv), typeOf(e2, nv, tnv)) match {
        case (NumT(), NumT()) => 
          NumT()
        case r =>
          throw TypeException("expected both branches of plus expression to be num typed, but got: " + r)
      }
    case MultExt(e1, e2) =>
      (typeOf(e1, nv, tnv), typeOf(e2, nv, tnv)) match {
        case (NumT(), NumT()) => 
          NumT()
        case r =>
          throw TypeException("expected both branches of plus expression to be num typed, but got: " + r)
      }
    case FdExt(x, t, e) =>
      throw NotImplementedException()
    case AppExt(f, a) =>
      throw NotImplementedException()
    case IdExt(x) =>
      lookup(x, nv)
    case ConsExt(hd, tl) =>
      throw NotImplementedException()
    case NilExt(t) =>
      throw NotImplementedException()
    case HeadExt(e) =>
      throw NotImplementedException()
    case TailExt(e) =>
      throw NotImplementedException()
    case LetType(x, t, e) =>
      throw NotImplementedException()
  }
  
  def lookup(x: String, nv: List[TBind]): Type =
    throw NotImplementedException()
  
  def tlookup(x: String, tnv: List[TVBind]): Type =
    throw NotImplementedException()

  def normalizeType(t: Type, tnv: List[TVBind]): Type =
    throw NotImplementedException()
  
}

sealed abstract class ExprExt

case class NumExt(n: Int)                          extends ExprExt
case class PlusExt(e1: ExprExt, e2: ExprExt)       extends ExprExt
case class MultExt(e1: ExprExt, e2: ExprExt)       extends ExprExt
case class FdExt(x: String, t: Type, e: ExprExt)   extends ExprExt
case class AppExt(f: ExprExt, a: ExprExt)          extends ExprExt
case class IdExt(x: String)                        extends ExprExt
case class ConsExt(hd: ExprExt, tl: ExprExt)       extends ExprExt
case class NilExt(t: Type)                         extends ExprExt
case class HeadExt(e: ExprExt)                     extends ExprExt
case class TailExt(e: ExprExt)                     extends ExprExt
case class LetType(x: String, t: Type, e: ExprExt) extends ExprExt

sealed abstract class Type

case class NumT()                 extends Type
case class ListT(t: Type)         extends Type
case class FunT(a: Type, r: Type) extends Type
case class VarT(x: String)        extends Type

case class TBind(x: String, t: Type)

case class TVBind(x: String, t: Type)


```

### Test:
```scala
//test: Test

import org.scalatest.FunSuite

import Library._
import TypeCheck._

class Test extends FunSuite {
 
  def check(s: String): Type =
    typeOf(Parser.parse(s), Nil, Nil)
  
  test("Function") {
    assertResult(FunT(NumT(), NumT())) {
      check("""
        (lambda (x : Num) x)
      """)
    }
  }
  
  test("Application") {
    assertResult(NumT()) {
      check("""
        ((lambda (x : Num) x) 1)
      """)
    }
  }
  
  test("Cons and Nil") {
    assertResult(ListT(NumT())) {
      check("""
        (cons 1 (nil : Num))
      """)
    }
  }
  
  test("Head and tail") {
    assertResult(NumT()) {
      check("""
        (head (tail (cons 1 (cons 2 (nil : Num)))))
      """)
    }
  }
  
  test("Lettype") {
    assertResult(FunT(NumT(), FunT(NumT(), NumT()))) {
      check("""
        (lettype (Amount Num)
          (lettype (Price Num)
            (lambda (a : Amount)
              (lambda (p : Price)
                (* a p)))))
      """)
    }
  }
  
}

```

__________________________________________________________________________________________________________________________________


### Solution:
```scala

```
