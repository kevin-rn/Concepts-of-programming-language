In this assignment you extend the language from week 4 with lazy evaluation, letrecs, and lists as specified in the course notes.

A Parser implementation is provided by means of the import import Parser._.

```scala
import Library._
import Untyped._
import Parser._

case class NotImplementedException(s: String) extends RuntimeException(s)
case class DesException() extends DesugarException()
case class InException() extends InterpException()

object Desugar {
  def dummy(binds: List[LetBindExt], body: ExprExt): ExprC = binds match {
    case LetBindExt(name,value)::Nil => SeqC(SetC(name, desugar(value)), desugar(body))
    case LetBindExt(name,value)::tail => SeqC(SetC(name, desugar(value)), dummy(tail, body))
    case _ => throw new DesException() 
  }
  
  def desugar(e: ExprExt): ExprC = e match {
    case NilExt() => NilC()
    case NumExt(num) => NumC(num)
    case IdExt(c) => IdC(c)
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
      case "force" => ForceC(desugar(e))
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
     case LetRecExt(binds, body) => LetRecC(binds.map(x => LetBindC(x.name, desugar(x.value))), desugar(body))
    case _ => throw new DesException()
  }
}

object Interp {
  def interp(e: ExprC): Value = interp(e, Nil)
  
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
    case ForceC(e) => force(interp(e, nv))
     case PlusC(l, r) => (strict(interp(l, nv)), strict(interp(r,nv))) match {
      case (NumV(e1), NumV(e2)) => NumV(e1 + e2)
      case (_, _) => throw new InException()
    } 
    case MultC(l, r) => (strict(interp(l, nv)), strict(interp(r,nv))) match {
      case (NumV(e1), NumV(e2)) => NumV(e1 * e2)
      case (_, _) => throw new InException()
    }
    case IfC(c, t, e) => strict(interp(c, nv)) match {
      case BoolV(true) => interp(t, nv)
      case BoolV(false) => interp(e, nv)
      case _ => throw new InException()
    }
    case EqNumC(l, r) => (strict(interp(l, nv)), strict(interp(r,nv))) match {
      case (NumV(e1), NumV(e2)) => BoolV(e1 == e2)
      case (_, _) => throw new InException()
    }
    case LtC(l, r) => (strict(interp(l, nv)), strict(interp(r,nv))) match {
      case (NumV(e1), NumV(e2)) => BoolV(e1 < e2)
      case (_, _) => throw new InException()
    } 
    case ConsC(l, r) => ConsV(ThunkV(Left((l, nv))), ThunkV(Left((r, nv)))) 
    case HeadC(e) => strict(interp(e, nv)) match {
      case ConsV(l, _) => l
      case _ => throw new InException()
    }
    case TailC(e) => strict(interp(e, nv)) match {
      case ConsV(_, r) => r
      case _ => throw new InException()
    }
    case IsNilC(e) => strict(interp(e, nv)) match {
      case NilV() => BoolV(true)
      case ConsV(_, _) => BoolV(false)
      case _ => throw new InException()
    }
    case IsListC(e) => strict(interp(e, nv)) match {
      case NilV() | ConsV(_, _) => BoolV(true)
      case _ => BoolV(false)
    }
    case f@FdC(params, body) => ClosV(f, nv.filter{ case Bind(s,v) => !params.contains(s)})
    case AppC(f, args) => strict(interp(f, nv)) match {
      case ClosV(FdC(params, body), env) if(params.size == args.size) =>
       interp(body, params.zip(args).map{ case (str, value) => Bind(str, ThunkV(Left((value, nv))))}:::env)
      case _ => throw new InException()
    }
    case LetRecC(binds, body) => {
      val (exps, new_binds) = (binds.map(x => x.value), binds.map(x => Bind(x.name, UninitializedV())))
      var new_nv = new_binds:::nv
      exps.zip(new_binds).map{ case (exp, b) => b.value = ThunkV(Left((exp, new_nv))) }
      //new new_nv and new_binds now cyclically refer to each other
      interp(body, new_nv)
    }
    case UninitializedC() => UninitializedV()
    case _ => throw new InException()
  }

  def strict(v: Value): Value = v match {
    case t@ThunkV(v2) => v2 match {
        case Left((e, nv)) => {
          val lv = strict(interp(e, nv))
          t.value = Right(lv)
          lv
        }
      case Right(rv) => rv
    }
    case _ => v
  }

  def force(v: Value): Value = v match {
    case v0@ThunkV(_) => force(strict(v0))
    case ConsV(l, r) => ConsV(force(l), force(r))
    case _ => v
  }
}


```
