The Reader.read function parses an s-expression in textual notation and returns an SExpr object representing the s-expression. 
The read function is defined using Scala parser combinators. 
Take a look at the tests to see examples of s-expressions that the parseAll function should generate. 
Your assignment is to implement the SExpr type using case classes.

In future exercises, the SExpr type, its case classes, and Reader.read will (invisibly) be available to you.

```scala
sealed abstract class SExpr

// define the case classes for SExpr
case class SSym(str: String) extends SExpr
case class SList(list: List[SExpr]) extends SExpr
case class SNum(i: Int) extends SExpr

import scala.util.parsing.combinator._

object Reader extends JavaTokenParsers {

  def read(text: String): SExpr = {
    val result = parseAll(sexpr,text)
    result match {
      case Success(r,_) => r
      case Failure(msg,n) => 
        sys.error(msg+" (input left: \""+n.source.toString.drop(n.offset)+"\")")
      case Error(msg, n) =>
        sys.error(msg+" (input left: \""+n.source.toString.drop(n.offset)+"\")")
    }
  }
  def sexpr  : Parser[SExpr] = (num | symbol | slist)
  def symbol : Parser[SExpr] = not(wholeNumber) ~> "[^()\\s]+".r ^^ SSym
  def slist  : Parser[SExpr] = "(" ~> sexpr.+ <~ ")"          ^^ SList
  def num    : Parser[SExpr] = wholeNumber                    ^^ {s => SNum(s.toInt)}
}

```


```scala
//test: Test

// test of the solution

import org.scalatest.FunSuite

class Test extends FunSuite {

  test("Multiplication and Addition") {
    assert(
      SList(List(SSym("+"), SNum(23), SList(List(SSym("*"), SNum(5), SNum(6))))) == Reader.read("(+ 23 (* 5 6))")
    )
  }

  test("Symbols") {
    assert(
      SList(List(SSym("abc"), SNum(23), SList(List(SSym("*"), SSym("def"), SNum(6))))) == Reader.read("(abc 23 (* def 6))")
    )
  }

}

```
