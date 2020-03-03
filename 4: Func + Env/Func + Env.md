Do not use substitution! Your solution will be manually checked to see if you are using environments.

If you use substitution instead of environments, your solution will be rejected and you will fail this assignment.

Summary:
1. Implement the interpreter as specified by the notes. You should consult chapter 6 and chapter 7 from the book to understand the concepts.
2. Develop more tests to support your understanding of the expected behaviour of the interpreter.
3. Do not change the signature of the functions given, or you’ll run into compilation problems with specification tests.

```scala
import Library._
import Parser._
import Untyped._

// IMPORTANT: DO NOT USE SUBSTITUTION FOR THIS ASSIGNMENT
case class NotImplementedException(s: String) extends RuntimeException(s)
case class DesException() extends DesugarException()
case class InException() extends InterpException()

object Desugar {
  def desugar(e: ExprExt): ExprC = e match {
    case NilExt() => NilC()
    case NumExt(num) => NumC(num)
    case IdExt(c) => IdC(c)
    case TrueExt() => TrueC()
    case FalseExt() => FalseC()
    case BinOpExt(s, l, r) => s match {
      case "+" => PlusC(desugar(l), desugar(r)) 
      case "*" => MultC(desugar(l), desugar(r))
      case "-" => (desugar(l), desugar(r)) match {
        case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new DesException()
        case _ => PlusC(desugar(l), MultC(NumC(-1), desugar(r))) 
      }
      case "and" => IfC(desugar(l), desugar(r), FalseC())
      case "or" => IfC(desugar(l), TrueC(), desugar(r))

      case "num=" => EqNumC(desugar(l), desugar(r))
      case "num<" => LtC(desugar(l), desugar(r))
      case "num>" => LtC(desugar(r), desugar(l))
      case "cons" => ConsC(desugar(l), desugar(r))
      case _ => throw new DesException()
    }
    case UnOpExt(s, e) => s match {
      case "-" => desugar(e) match {
        case TrueC() | FalseC() => throw new DesException()
        case _ => MultC(NumC(-1), desugar(e))
      }
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
    case AppExt(f, args) => AppC(desugar(f), args.map(x => desugar(x)))
    case FdExt(params, body) => FdC(params, desugar(body))
    case LetExt(binds, body) => {
      val params = binds.map{ case LetBindExt(x, y) => x
                              case _ => throw new DesException()}
      val args = binds.map{ case LetBindExt(x, y) => desugar(y) 
                            case _ => throw new DesException()}
      AppC(FdC(params, desugar(body)), args)
    }
    case RecLamExt(name, param, body) => {
      val func = FdC(List("x"), AppC(IdC("f"), List(FdC(List("v"), AppC(AppC(IdC("x"), List(IdC("x"))), List(IdC("v")))))))
      val YComb = FdC(List("f"), AppC(func, List(func)))
      AppC(YComb, List(FdC(List(name), FdC(List(param), desugar(body)))))
    }
    case _ => throw new DesException()
  }
}

object Interp {
  def intValue(v: Value): Int = v match { 
    case NumV(n) => n 
    case _ => throw new InException()
  }
  
  def lookup(e: String, nv: Environment): Value = nv match {
    case Nil => throw new InException()
    case Bind(name, value)::rest => if(e == name) value else lookup(e, rest)
  }
  
  def interp(e: ExprC, nv: Environment): Value = e match {
    case NilC() => NilV()
    case NumC(n) => NumV(n)
    case TrueC() => BoolV(true) 
    case FalseC() => BoolV(false)
    case IdC(c) => lookup(c, nv)
    case PlusC(l, r) => (l,r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => NumV(intValue(interp(l, nv))+ intValue(interp(r, nv)))
    } 
    case MultC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => NumV(intValue(interp(l, nv))*intValue(interp(r, nv)))
    }
    case IfC(c, t, e) => interp(c, nv) match {
      case BoolV(true) => interp(t, nv)
      case BoolV(false) => interp(e, nv)
      case _ => throw new InException()
    }
    case EqNumC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => BoolV(intValue(interp(l, nv))==intValue(interp(r, nv)))
    }
    case LtC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => BoolV(intValue(interp(l, nv)) < intValue(interp(r, nv)))
    } 
    case ConsC(l, r) => ConsV(interp(l, nv), interp(r, nv)) 
    case HeadC(e) => interp(e, nv) match {
      case ConsV(l, r) => l
      case _ => throw new InException()
    }
    case TailC(e) => interp(e, nv) match {
      case ConsV(l, r) => r
      case _ => throw new InException()
    }
    case IsNilC(e) => interp(e, nv) match {
      case NilV() => BoolV(true)
      case ConsV(_, _) => BoolV(false)
      case _ => throw new InException()
    }
    case IsListC(e) => interp(e, nv) match {
      case NilV() => BoolV(true)
      case ConsV(_, _) => BoolV(true)
      case _ => BoolV(false)
    }
    case FdC(params, body) => ClosV(FdC(params, body), nv.filter{ case Bind(s,v) => !params.contains(s)})
    case AppC(f, args) => interp(f, nv) match {
      case ClosV(FdC(params, body), env) if(params.size == args.size) =>
      interp(body, env:::params.zip(args).map{ case (str, value) => Bind(str, interp(value, nv))})
      case _ => throw new InException()
    }
    case _ => throw new InException()
  }

  // IMPORTANT: DO NOT USE SUBSTITUTION FOR THIS ASSIGNMENT

  def interp(e: ExprC): Value = interp(e, Nil)
}
```
