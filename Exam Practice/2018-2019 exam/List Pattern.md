Implement an interpreter with support for pattern matching against list- and variable patterns.

The solution template contains a partial interpreter for the language whose grammar is summarized below, and whose core expression forms (ExprC), values (Value), and patterns (Pattern) are defined at the bottom of the solution template.

Implement the missing cases in the interp, patmatch, and valmatch functions in the solution template.

Pattern matching should work as follows (similarly as in Scala):

    (match e (p1 e1) (p2 e2)...) evaluates e to a value and attempts to match a pattern (p1, p2, …) against the value
    Patterns are tried in the order they appear; i.e., top-most patterns are tried first
    Patterns with variables should bind the variable to the value that was matched
    Cons patterns should match cons values
    Nil patterns should match nil values
    If no patterns match, an InterpException should be raised.

For interpretation errors, you should use the following predefined case class (which cannot be extended):

case class InterpException(msg: String) extends RuntimeException(msg)

The “Test” tab contains example programs that your interpreter should accept and reject.

Note that you are given a parse function parse(s: SExpr): ExprC for the language.

Note also that the valmatch function in the solution template makes use of the Option type found in the Scala Standard Library.
Grammar

<expr> ::= <num>
         | (+ <expr> <expr>)
         | (lambda (<id>) <expr>)
         | (<expr> <expr>)
         | <id>
         | (match <expr> (<pat> <expr>) (<pat> <expr>)...)
         | (cons <expr> <expr>)
         | nil

<pat> ::= <id>
        | (cons <pat> <pat>)
        | nil

Note: (match (<pat> <expr>) (<pat> <expr>)...) means match expressions can have one or more case branches (i.e., (<pat> <expr>)).

### Template:
```scala
object Interp {
  
  def interp(e: ExprC, nv: List[Bind]): Value = e match {
    case NumC(n) => NumV(n)
    case PlusC(e1, e2) => (interp(e1, nv), interp(e2, nv)) match {
      case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
      case _ => throw InterpException("addition expects two numbers")
    }
    case IdC(x) => lookup(x, nv)
    case FdC(x, e) =>
      throw NotImplementedException()
    case AppC(e1, e2) =>
      throw NotImplementedException()
    case MatchC(e, pats) =>
      patmatch(interp(e, nv), pats, nv)
    case ConsC(hd, tl) =>
      ConsV(interp(hd, nv), interp(tl, nv))
    case NilC() =>
      NilV()
  }
  
  // Pattern match value `v`
  // against the first pattern `p` that matches (if any)
  // and evaluate the corresponding expression
  // under the (potential) extension of environment `nv`
  def patmatch(v: Value, pats: List[(Pattern, ExprC)], nv: List[Bind]): Value = pats match {
    case (p, e) :: pats => 
      valmatch(p, v) match {
        case Some(nv_pats) => 
          throw NotImplementedException()
        case None => 
          throw NotImplementedException()
      }
    case Nil => 
      throw NotImplementedException()
  }
  
  // Match a value against a pattern.
  // If matching succeeds, return list of bindings
  // for each successful variable pattern match
  def valmatch(p: Pattern, v: Value): Option[List[Bind]] = 
    throw NotImplementedException()
  
  def lookup(x: String, nv: List[Bind]): Value = 
    throw NotImplementedException()
  
}

// DO NOT EDIT BELOW THIS LINE

sealed abstract class ExprC
case class NumC (n: Int)                       extends ExprC // n
case class PlusC(l: ExprC, r: ExprC)           extends ExprC // (+ e1 e2)
case class IdC  (x: String)                    extends ExprC // x
case class FdC  (param: String, body: ExprC)  extends ExprC // (lambda (x) e)
case class AppC (f: ExprC, a: ExprC)           extends ExprC // (e1 e2)
case class MatchC(e: ExprC, pats: List[(Pattern, ExprC)]) extends ExprC // (match e (p1 e1) ... (pn en))
case class ConsC(hd: ExprC, tl: ExprC)         extends ExprC // (cons e1 e2)
case class NilC()                              extends ExprC // nil

sealed abstract class Pattern
case class VarP(x: String)                     extends Pattern // x
case class ConsP(hd: Pattern, tl: Pattern)     extends Pattern // (cons p1 p2)
case class NilP()                              extends Pattern // nil

sealed abstract class Value
case class NumV(n: Int)                        extends Value
case class ClosV(fdc: FdC, nv: List[Bind])     extends Value
case class ConsV(hd: Value, tl: Value)         extends Value
case class NilV()                              extends Value

case class Bind(x: String, val v: Value)

```

### Test:
```scala
//test: Test

import org.scalatest.FunSuite

import Interp._
import Parser._

class Test extends FunSuite {
  
  def run(s: String): Value = interp(parse(s), Nil)
  def run(s: String, nv: List[Bind]): Value = interp(parse(s), nv)
  
  test("application") {
    assertResult(NumV(1)) {
      run("((lambda (x) x) 1)")
    }
  }
  
  test("pattern matching involving a variable") {
    assertResult(NumV(1)) {
      // match the number 1 against a variable pattern and return it
      run("""
        (match 1
          (x x))
      """)
    }
  }
  
  test("pattern matching involving a nil pattern") {
    assertResult(NumV(1)) {
      run("""
        (match nil
          (nil 1))
      """)
    }
  }
  
  test("pattern matching involving a cons pattern") {
    assertResult(NumV(1)) {
      run("""
        (match (cons 1 nil)
          ((cons x nil) x))
      """)
    }
  }
  
  test("pattern matching falls through until a pattern does not match") {
    assertResult(NumV(1)) {
      run("""
        (match (cons 1 (cons 2 nil))
          (nil 0)
          ((cons x nil) 0)
          ((cons x y) x))
      """)
    }
  }
  
  test("pattern matching throws an interp exception if no patterns match") {
    intercept[InterpException] {
      run("""
        (match 1
          (nil 0)
          ((cons x y) 0))
      """)
    }
  }
  
}

```

__________________________________________________________________________________________________________________________________


### Solution:
```scala

```
