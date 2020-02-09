Define the parse function that transforms an SExpr into an ArithC object, provided of course that the SExpr has the correct format. 
Throw a ParseException with an appropriate error message otherwise. 
ArithC should have case classes for numbers, addition, and multiplication, just like Chapter 2.4 in the PLAI book.

Note: The SExpr type, its case classes and Reader.read from the last assignment are defined outside of your solution editor. 
They are made available by the import Library._ line.

```scala
import Library._

// to do: define the other case classes of the ArithC type
sealed abstract class ArithC
case class NumC(num: Int) extends ArithC
case class PlusC(a: ArithC, b: ArithC) extends ArithC
case class MultC(a: ArithC, b: ArithC) extends ArithC

case class ParseException(string: String) extends RuntimeException

object Parser {
  def parse(str: String): ArithC = parse(Reader.read(str))
  
  // to do: define the parse function
  def parse(sexpr: SExpr): ArithC = sexpr match {
    case SNum(n) => NumC(n)
    case SList(l) => l match {
      case x if(l(0) == SSym("+")) => PlusC(parse(l(1)), parse(l(2))) 
      case y if(l(0) == SSym("*")) => MultC(parse(l(1)), parse(l(2)))
      case _ => throw ParseException("invalid list input")
    }
    case _ => throw ParseException("invalid input")
    
    
  }
}


```

```scala
//test: Test

import org.scalatest.FunSuite
import Parser._
import Library._

class Test extends FunSuite {
  test("parse test") {
    assertResult(
      PlusC(
        NumC(23),
        MultC(
          NumC(5),
          NumC(6)
        )
      )
    ) {
      parse("(+ 23 (* 5 6))")
    }
  }
}
```
