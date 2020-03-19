Summary:

    Implement the parser, desugarer, and interpreter described in the book, chapters 2-4.
    Develop more tests to support your understanding of the expected behaviour of the interpreter.
    Extend the implementation with the extra features specified in the course notes: numeric binary operators (num=, num<, and num>), conditionals (booleans, if, and, or, not, and the multi-armed conditional cond), and lists (cons, nil, head, tail, is-nil, is-list, and list).
    Do not change the signature of the functions given, or you’ll run into compilation problems with specification tests.

See notes for an extended explanation including a specification of the grammar and abstract syntax.

```scala
import Library._
import Untyped._

case class NotImplementedException(s: String) extends RuntimeException(s)

case class ParException() extends ParseException()
case class DesException() extends DesugarException()
case class InException() extends InterpException()

object Parser {
  def parse(str: String): ExprExt = parse(Reader.read(str))
  
  def condList(cs: List[SExpr], e: Boolean): List[(ExprExt, ExprExt)] = {
    val list = if(e) cs.take(cs.size-1) else cs
    if(list.size==0) throw new ParException()
    list.foldLeft(List[(ExprExt, ExprExt)]()) {
              case (previous, next) => next match {
                case SList(List(cond, out)) if(cond != SSym("else")) => previous :+ (parse(cond) , parse(out))
                case _ => throw new ParException
          }}
    
  }

  def parse(sexpr: SExpr): ExprExt = sexpr match {
    case SNum(i) => NumExt(i)
    case SSym(str) => str match {
      case "true" => TrueExt()
      case "false" => FalseExt()
      case "nil" => NilExt()
      case _ => throw new ParException()
      }
    case SList(SSym("if")::List(c, l, r)) => IfExt(parse(c), parse(l), parse(r))
    case SList(SSym("list")::l) => ListExt(l.map(x => parse(x)))
    case SList(SSym("cond")::cs) => {
        cs(cs.size - 1) match {
          case SList(List(SSym("else"), out)) => CondEExt(condList(cs, true), parse(out))
          case _ => CondExt(condList(cs, false))
        }
      }
    case SList(List(s, l, r)) => s match {
        case SSym("+") => BinOpExt("+", parse(l), parse(r))
        case SSym("*") => BinOpExt("*", parse(l), parse(r))
        case SSym("-") => BinOpExt("-", parse(l), parse(r))
        case SSym("and") => BinOpExt("and", parse(l), parse(r))
        case SSym("or") => BinOpExt("or", parse(l), parse(r))
        case SSym("num=") => BinOpExt("num=", parse(l), parse(r))
        case SSym("num<") => BinOpExt("num<", parse(l), parse(r))
        case SSym("num>") => BinOpExt("num>", parse(l), parse(r))
        case SSym("cons") => BinOpExt("cons", parse(l), parse(r))
        case _ => throw new ParException()
    }
    case SList(List(s, e)) => s match {
        case SSym("-") => UnOpExt("-", parse(e))
        case SSym("not") => UnOpExt("not", parse(e))
        case SSym("head") => UnOpExt("head", parse(e))
        case SSym("tail") => UnOpExt("tail", parse(e))
        case SSym("is-nil") => UnOpExt("is-nil", parse(e))
        case SSym("is-list") => UnOpExt("is-list", parse(e))
        case _ => throw new ParException()
    }
    case _ => throw new ParException()
  }
}

object Desugar {
  def desugar(e: ExprExt): ExprC = e match {
    case NilExt() => NilC()
    case NumExt(num) => NumC(num)
    case TrueExt() => TrueC()
    case FalseExt() => FalseC()

    case BinOpExt(s, l, r) => s match {
      case "+" => PlusC(desugar(l), desugar(r)) 
      case "*" => MultC(desugar(l), desugar(r))
      case "-" => PlusC(desugar(l), MultC(NumC(-1), desugar(r))) 
      case "and" => IfC(desugar(l), desugar(r), FalseC())
      case "or" => IfC(desugar(l), TrueC(), desugar(r))
      case "num=" => EqNumC(desugar(l), desugar(r))
      case "num<" => LtC(desugar(l), desugar(r))
      case "num>" => LtC(desugar(r), desugar(l))
      case "cons" => ConsC(desugar(l), desugar(r))
      case _ => throw new DesException()
    }
    case UnOpExt(s, e) => s match {
      case "-" => MultC(NumC(-1), desugar(e))
      case "not" => IfC(desugar(e), FalseC(), TrueC())
      case "head" => HeadC(desugar(e))
      case "tail" => TailC(desugar(e))
      case "is-list" => IsListC(desugar(e))
      case "is-nil" => IsNilC(desugar(e))
      case _ => throw new DesException()
    }
    case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))
    case ListExt(list) => list match {
      case head::tail => ConsC(desugar(head), desugar(ListExt(tail)))
      case _ => NilC()
    }
    case CondExt(cs) => cs match {
      case (cond,out)::next => IfC(desugar(cond), desugar(out), desugar(CondExt(next)))
      case _ => UndefinedC()
    }
   case CondEExt(cs, e) => cs match {
      case (cond, out)::next => IfC(desugar(cond), desugar(out), desugar(CondEExt(next,e)))
      case _ => desugar(e)
    }
    case _ => UndefinedC()
  }
}

object Interp {
  def intValue(v: Value): Int = v match { 
    case NumV(n) => n 
    case _ => throw new InException()
  }
  
  def interp(e: ExprC): Value = e match {
    case NilC() => NilV()
    case NumC(n) => NumV(n)
    case TrueC() => BoolV(true) 
    case FalseC() => BoolV(false)
    case PlusC(l, r) => (l,r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => NumV(intValue(interp(l))+ intValue(interp(r)))
    } 
    case MultC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => NumV(intValue(interp(l))*intValue(interp(r)))
    }
    case IfC(c, t, e) => interp(c) match {
      case BoolV(true) => interp(t)
      case BoolV(false) => interp(e)
      case _ => throw new InException()
    }
    case EqNumC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => BoolV(intValue(interp(l))==intValue(interp(r)))
    }
    case LtC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => BoolV(intValue(interp(l)) < intValue(interp(r)))
    } 
    
    case ConsC(l, r) => ConsV(interp(l), interp(r)) 
    case HeadC(e) => interp(e) match {
      case ConsV(l, r) => l
      case _ => throw new InException()
    }
    case TailC(e) => interp(e) match {
      case ConsV(l, r) => r
      case _ => throw new InException()
    }
    case IsNilC(e) => interp(e) match {
      case NilV() => BoolV(true)
      case ConsV(_, _) => BoolV(false)
      case _ => throw new InException()
    }
    case IsListC(e) => interp(e) match {
      case NilV() => BoolV(true)
      case ConsV(_, _) => BoolV(true)
      case _ => BoolV(false)
    } 
    case UndefinedC() | _ => throw new InException()
  }
}

```


```scala
//test: Test
import Untyped._
import Parser._
import Desugar._
import Interp._

import org.scalatest.FunSuite

class Test extends FunSuite with CatchErrorSuite {
  
  test("list") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-list nil)")))
    }
  }
  

  test("Failure") {
    assertResult(NumV(10)) {
      interp(desugar(parse("(+ 5 5̴)")))
    }
  }
  
  test("complex arithmetic") {
    assertResult(NumV(1)) {
      interp(desugar(parse("(- 5 (* 2 2))")))
    }
  }
  
   test("complex boolean") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(not (num= 1 1))")))
    }
  }
  
  test("and true 1") {
    assertResult(NumV(1)) {
      interp(desugar(parse("(and true 1)")))
    }
  }
  
  test("and false 1") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(and false 1)")))
    }
  }
  
  test("and 1 true") {
    intercept[InterpException] {
      interp(desugar(parse("(and 1 true)")))
    }
  }
  
  test("and 1 false") {
    intercept[InterpException] {
      interp(desugar(parse("(and 1 false)")))
    }
  }

  
  test("or true 1") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(or true 1)")))
    }
  }
  
  test("or false 1") {
    assertResult(NumV(1)) {
      interp(desugar(parse("(or false 1)")))
    }
  }
  
  test("or 1 true") {
    intercept[InterpException] {
      interp(desugar(parse("(or 1 true)")))
    }
  }
  
  test("or 1 false") {
    intercept[InterpException] {
      interp(desugar(parse("(or 1 false)")))
    }
  }
  
  test("Condition with else") {
    assertResult(
      (NumV(1))
    ) {
      interp(desugar(parse("(cond ((num< 1 0) 0)(else 1))")))
    }
  }
  
  test("Multi conditional with exception") {
    intercept[InterpException] {
      interp(desugar(parse("(cond ((num> 0 1) 1) ((num< 1 0) 2))")))
    }
  }
  
  test("Conditional with undefined construct") {
    intercept[InterpException] {
      interp(desugar(parse("(cond ((and true false) 3))")))
    }
  }

  /**
   * Tests for Parsing
   */

  test("Parse 5") {
    assertResult(
      NumExt(5)
    ) {
      parse("5")
    }
  }

  /**
   * Tests for Desugaring
   */

  test("Desugar 5") {
    assertResult(
      NumC(5)
    ) {
      desugar(NumExt(5))
    }
  }

  /**
   * Tests for Interpreting
   */

  test("Interp 5") {
    assertResult(
      NumV(5)
    ) {
      interp(NumC(5))
    }
  }
  
  test("Interp 5+true throws InterpException") {
    intercept[InterpException] {
      interp(PlusC(NumC(5), TrueC()))
    }
  }
  
   test("Interp and true 8 throws InterpException") {
    assertResult(NumV(8)) {
      interp(desugar(parse("(and true 8)")))
    }
  }
  
  
  test("Verify correct implementation") {
    assertResult(NumV(5)) {
      interp(desugar(parse("5")))
    }
  }

  test("Catch erroneous parse behavior") {
    intercept[ParseException] {
      parse("()")
    }
  }

  test("Catch erroneous interp behavior") {
    intercept[InterpException] {
      interp(desugar(parse("(+ true 5)")))
    }
  }
  
  test("cons") {
    assertResult(ConsV(NumV(1), NumV(2))) {
      interp(desugar(parse("(cons 1 2)")))
    }
  }
  
  test("nil") {
      assertResult(NilV()) {
        interp(desugar(parse("nil")))
    }
  }
  
  test("head correct") {
      assertResult(NumV(1)) {
        interp(desugar(parse("(head (cons 1 2))")))
    }
  }
  
  test("head incorrect") {
    intercept[InterpException] {
      interp(desugar(parse("(head (+ 1 1))")))
    }
  }
  
  test("tail correct") {
      assertResult(NumV(2)) {
      interp(desugar(parse("(tail (cons 1 2))")))
    }
  }
  
  test("tail incorrect") {
    intercept[InterpException] {
      interp(desugar(parse("(tail (+ 1 1))")))
    }
  }
  

  test("is-nil correct true") {
      assertResult(BoolV(true)) {
        interp(desugar(parse("(is-nil nil)")))
    }
  }
  
  test("is-nil correct false") {
      assertResult(BoolV(false)) {
        interp(desugar(parse("(is-nil (cons 1 2))")))
    }
  }
  
  test("is-nil incorrect") {
    intercept[InterpException] {
      interp(desugar(parse("(is-nil (+ 1 1))")))
    }
  }
  
  test("is-list false") {
      assertResult(BoolV(false)) {
        interp(desugar(parse("(is-list (+ 2 2))")))
    }
  }
  
  test("is-list true") {
      assertResult(BoolV(true)) {
        interp(desugar(parse("(is-list (cons 1 2))")))
    }
  }
  
    test("is-list nil") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-list nil)"))) 
    }
  }
  
  test("list correct") {
    assertResult(ConsV(NumV(1), ConsV(NumV(2), ConsV(NumV(3), NilV())))) {
      interp(desugar(parse("(list 1 2 3)")))
    }
  }

}

```
