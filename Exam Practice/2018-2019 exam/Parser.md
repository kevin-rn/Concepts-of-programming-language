Implement a parser for a type-annotated language whose grammar is summarized below.

The solution template defines the ExprExt type that your parse function should return values of.

The “Test” tab contains example programs that should parse using your parser.

Note that functions cannot bind reserved words. The reserved words are also given in the solution template.

For parse errors, you should use the following predefined class (which cannot be extended):

case class ParseException(msg: String) extends RuntimeException(msg)

Hint on Parsing Type Annotations

Character sequences like : and -> are parsed as SSymbols by the reader. To match a : character in your parser, match on an SSym(":").
Grammar

<expr> ::= <num>
         | (+ <expr> <expr>)
         | (lambda (<id> : <type>) <expr>)
         | (<expr> <expr>)
         | <id>
         | (nil : <type>)
         | (cons <expr> <expr>)
         | (head <expr>)
         | (tail <expr>)

<type> ::= Num
         | (List <type>)
         | (<type> -> <type>)

### Template:
```scala
import Library._

object Parser {

  def parse(s: SExpr): ExprExt = s match {
    case SNum(n) =>
      throw NotImplementedException()
    case SSym(s) =>
      throw NotImplementedException()
    case SList(ss) =>
      throw NotImplementedException()
    case _ =>
      throw ParseException("Parsing failed because the s-expression was not recognized: " + s)
  }

  def parseType(s: SExpr): Type =
    throw NotImplementedException()

  def reservedWords = 
    Set("lambda", "nil", "cons", "head", "tail", "Num", "List")

}

// DO NOT EDIT BELOW THIS LINE

sealed abstract class ExprExt

case class NumExt(n: Int)                        extends ExprExt
case class PlusExt(e1: ExprExt, e2: ExprExt)     extends ExprExt
case class FdExt(x: String, t: Type, e: ExprExt) extends ExprExt
case class AppExt(f: ExprExt, a: ExprExt)        extends ExprExt
case class IdExt(x: String)                      extends ExprExt
case class ConsExt(hd: ExprExt, tl: ExprExt)     extends ExprExt
case class NilExt(t: Type)                       extends ExprExt
case class HeadExt(e: ExprExt)                   extends ExprExt
case class TailExt(e: ExprExt)                   extends ExprExt

sealed abstract class Type

case class NumT()                 extends Type
case class ListT(t: Type)         extends Type
case class FunT(a: Type, r: Type) extends Type


```

### Test:
```scala
//test: Test

import org.scalatest.FunSuite

import Library._
import Parser._

class Test extends FunSuite {
  
  def read(s: String): SExpr = Reader.read(s)
  
  test("Number") {
    assertResult(NumExt(1)) {
      parse(read("1"))
    }
  }
  
  test("Plus") {
    assertResult(PlusExt(NumExt(1), NumExt(2))) {
      parse(read("(+ 1 2)"))
    }
  }
  
  test("Function") {
    assertResult(FdExt("x", NumT(), IdExt("x"))) {
      parse(read("(lambda (x : Num) x)"))
    }
  }
  
  test("Nil") {
    assertResult(NilExt(NumT())) {
      parse(read("(nil : Num)"))
    }
  }
  
  test("Nil with function type") {
    assertResult(NilExt(FunT(NumT(), NumT()))) {
      parse(read("(nil : (Num -> Num))"))
    }
  }
  
  test("Cons") {
    assertResult(ConsExt(NumExt(1), NumExt(2))) {
      parse(read("(cons 1 2)"))
    }
  }
  
  test("Head") {
    assertResult(HeadExt(ConsExt(NumExt(1), NilExt(NumT())))) {
      parse(read("(head (cons 1 (nil : Num)))"))
    }
  }
  
  test("Tail") {
    assertResult(TailExt(ConsExt(NumExt(2), NilExt(NumT())))) {
      parse(read("(tail (cons 2 (nil : Num)))"))
    }
  }
  
  test("Application") {
    assertResult(AppExt(FdExt("x", NumT(), IdExt("x")), NumExt(42))) {
      parse(read("((lambda (x : Num) x) 42)"))
    }
  }
  
}

```

__________________________________________________________________________________________________________________________________


### Solution:
```scala

```
