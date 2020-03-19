Implement a desugarer and interpreter for a language with mutation as specified in the assignment notes.

A Parser implementation is provided by means of the import import Parser._

Summary:

    The basis for the language is the language with functions as first-class citizens from chapter 7 of the book. You can probably reuse and adapt some of the code from the environment-based interpreter from week 4, but be aware of the differences. If your implementation of closures was not clean and elegant, try to fix that this time.
    Extend the implementation with mutation as specified in the course notes.
    Develop more tests to support your understanding of the expected behavior of the interpreter.
    Do not change the signature of the functions given, or you will run into compilation problems with specification tests.

If you receive a RunningFailure from WebLab, that means the spectests are running into an infinite loop. This is probably due to a mistake in mutation in your solution.
Hints

The hints for this assignment are numbered. We recommend focusing on hints with lower numbers first. The reason for this is that we cannot check some of the functionality if more basic functionality is incorrect. For example, assume you have hints like this:

(0) PLUS - You do not interpret plus correctly.
(3) FUNCTIONS - You do not interpret function application correctly.

It is possible that we simply cannot test function application properly because your plus implementation is incorrect. The second hint might go away once you fix plus. By fixing lower numbered problems first, you won’t be misled by our hints.

```scala
import Library._
import Parser._
import Untyped._

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
      case "-" =>  PlusC(desugar(l), MultC(NumC(-1), desugar(r))) 
      case "and" => IfC(desugar(l), desugar(r), FalseC())
      case "or" => IfC(desugar(l), TrueC(), desugar(r))
      case "num=" => EqNumC(desugar(l), desugar(r))
      case "num<" => LtC(desugar(l), desugar(r))
      case "num>" => AppC(FdC(List("left", "right"), LtC(IdC("right"), IdC("left"))), List(desugar(l), desugar(r)))
      case "cons" => ConsC(desugar(l), desugar(r))
      case "setbox" => SetboxC(desugar(l), desugar(r))
      case "seq" => SeqC(desugar(l), desugar(r))
      case _ => throw new DesException()
    }
    case UnOpExt(s, e) => s match {
      case "-" =>  MultC(NumC(-1), desugar(e))
      case "not" => IfC(desugar(e), FalseC(), TrueC())
      case "head" => HeadC(desugar(e))
      case "tail" => TailC(desugar(e))
      case "is-list" => IsListC(desugar(e))
      case "is-nil" => IsNilC(desugar(e))
      case "box" => BoxC(desugar(e))
      case "unbox" => UnboxC(desugar(e))
      case _ => throw new DesException()
    }
    case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))
    case ListExt(list) => list match {
      case head::tail => ConsC(desugar(head), desugar(ListExt(tail)))
      case _ => NilC()
    }
    case CondExt(cs) => cs match {
      case (cond,out)::next => IfC(desugar(cond), desugar(out), desugar(CondExt(next)))
      case _ => UninitializedC()
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
    case SetExt(id, e) => SetC(id, desugar(e))
    case LetRecExt(binds, body) => AppC(FdC(binds.map(x => x.name), dummy(binds, body)), binds.map(x => UninitializedC())) 
    
    case _ => throw new DesException()
  }
  
  def dummy(binds: List[LetBindExt], body: ExprExt): ExprC = binds match {
    case LetBindExt(_,_)::Nil => SeqC(SetC(binds.head.name, desugar(binds.head.value)), desugar(body))
    case LetBindExt(_,_)::tail => SeqC(SetC(binds.head.name, desugar(binds.head.value)), dummy(tail, body))
    case _ => throw new DesException() 
  }
}

object Interp {
  type Store = List[Cell]
  type PointerEnvironment = List[Pointer]
  
  // searches for correct location
  def lookup(e: String, nv: PointerEnvironment): Int = nv match {
    case Pointer(n, loc)::tail => if(n==e) loc else lookup(e, tail)
    case _ => throw new InException()
  }
   
  // get value from store
  def fetch(loc: Int, st: Store): Value = st match {
    case Nil => throw new InException()
    case Cell(location, value)::tail => if(loc == location) value else fetch(loc, tail)
  }
  
  def updateStore(bindings: List[(String, ExprC)], nv: PointerEnvironment, st: Store): (PointerEnvironment, Store) = bindings match {
    case Nil => (nv, st)
    case (str, e)::tail => {
      val (value, st1) = interp(e, nv, st)
      val location = st1.size
      val pointer = Pointer(str, location)
      updateStore(tail, pointer::nv, Cell(location, value)::st1)
    }
  }
  
  // Do not remove this method. We use this for grading.
  def interp(e: ExprC): Value = interp(e, Nil, Nil)._1

  def interp(e: ExprC, nv: PointerEnvironment, st1: Store): (Value, Store) = e match {
    case NilC() => (NilV(), st1)
    case NumC(n) => (NumV(n), st1)
    case TrueC() => (BoolV(true), st1)
    case FalseC() => (BoolV(false), st1)
    case IdC(c) => (fetch(lookup(c, nv), st1), st1)
    case PlusC(l, r) => (l,r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => {
        val (l2, st2) = interp(l, nv, st1)
        val (r2, st3) = interp(r, nv, st2)
        (l2, r2) match {
          case (NumV(a), NumV(b)) => (NumV(a+b), st3)
          case _ => throw new InException()
        }
      }
    } 
    case MultC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => {
        val (l2, st2) = interp(l, nv, st1)
        val (r2, st3) = interp(r, nv, st2)
        (l2, r2) match {
          case (NumV(a), NumV(b)) => (NumV(a*b), st3)
          case _ => throw new InException()
        }
      }
    }
    case IfC(c, t, e) => interp(c, nv, st1) match {
      case (BoolV(true), st2) => interp(t, nv, st2)
      case (BoolV(false), st2) => interp(e, nv, st2)
      case _ => throw new InException()
    }
    case EqNumC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => {
        val (l2, st2) = interp(l, nv, st1)
        val (r2, st3) = interp(r, nv, st2)
        (l2, r2) match {
          case (NumV(a), NumV(b)) => (BoolV(a==b), st3)
          case _ => throw new InException()
        }
      }
    }
    case LtC(l, r) => (l, r) match {
      case (TrueC(), _) | (FalseC(), _) | (_, TrueC()) | (_, FalseC()) => throw new InException()
      case _ => {
        val (l2, st2) = interp(l, nv, st1)
        val (r2, st3) = interp(r, nv, st2)
        (l2, r2) match {
          case (NumV(a), NumV(b)) => (BoolV(a<b), st3)
          case _ => throw new InException()
        }
      }
    } 
    case ConsC(l, r) => {
      val (l2, st2) = interp(l, nv, st1)
      val (r2, st3) = interp(r, nv, st2)
      (ConsV(l2, r2), st3)
    }
    case HeadC(e) => interp(e, nv, st1) match {
      case (ConsV(l, r), st2) => (l, st2)
      case _ => throw new InException()
    }
    case TailC(e) => interp(e, nv, st1) match {
      case (ConsV(l, r), st2) => (r, st2)
      case _ => throw new InException()
    }
    case IsNilC(e) => interp(e, nv, st1) match {
      case (NilV(), st2) => (BoolV(true), st2)
      case (ConsV(_, _), st2) => (BoolV(false), st2)
      case _ => throw new InException()
    }
    case IsListC(e) => interp(e, nv, st1) match {
      case (NilV(), st2) => (BoolV(true), st2)
      case (ConsV(_, _), st2) => (BoolV(true), st2)
      case _ => (BoolV(false), st1)
    }
    case FdC(params, body) => (PointerClosV(FdC(params, body), nv.filter{ case Pointer(s,v) => !params.contains(s)}), st1)
    case AppC(f, args) => interp(f, nv, st1) match {
      case (PointerClosV(FdC(params, body), nv_fun), st2) if(params.size == args.size) => {
        val (nv1, st3) = updateStore(params.zip(args), nv, st2)
        interp(body, nv1:::nv_fun, st3)
      }
      case _ => throw new InException()
    }
    case BoxC(v) => {
      val (v1, st2) = interp(v, nv, st1)
      val location = st2.size
      (BoxV(location), Cell(location, v1)::st2)
    }
    case UnboxC(b) => interp(b, nv, st1) match {
      case (BoxV(b1), st2) => (fetch(b1, st2), st2)
      case _ => throw new InException()
    }
    case SetboxC(b, v) => interp(b, nv, st1) match {
      case (BoxV(loc), st2) => {
        val (value, st3) = interp(v, nv, st2)
        val st4 = st3.map{ case x => if(x.location == loc) Cell(loc, value) else x}
        (value, st4)
      }
      case _ => throw new InException()
    }
    case SetC(str, e) => interp(e, nv, st1) match {
      case (value, st2) => {
        val loc = lookup(str, nv)
        val st3 = st2.map{ case x => if(x.location == loc) Cell(loc, value) else x}
        (value, st3)
      }
      case _ => throw new InException()
    }
    case SeqC(b1, b2) => {
      val (value, st2) = interp(b1, nv, st1)
      interp(b2, nv, st2)
    }
    case UninitializedC() => (UninitializedV(), st1)
    case _ => throw new InException()
  }
}


```
