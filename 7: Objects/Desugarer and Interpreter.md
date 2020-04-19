Week 7: Objects as desugaring

Implement a desugarer and interpreter for objects as specified in the assignment notes.

Summary:

    The basis for the language is the language you implemented in week 5.
    You can reuse and adapt some of the code from this week. However, be aware of the differences.
    If your implementation was not clean and elegant, try to fix that this time.
    Implement a desugarer for objects as specified in the notes.
    Extend the implementation with the new operations as specified in the notes.
    Do not change the signature of the functions given, or you’ll run into compilation problems with specification tests.

Tests:
```scala
//test: Test

import org.scalatest.FunSuite
import Parser._
import Desugar._
import Interp._
import Untyped._

class Test extends FunSuite with CatchErrorSuite {

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
        
  def withClockObject =
    s"""
      (let ((clock (object
        ((field counter 0))
        ((method tick () (set counter (+ counter 1)))
        (method reset () (set counter 0))
        (method read () counter)))))
         (do-seq (msg clock tick) (msg clock read)))
    """

  test("clock can tick") { assertResult(NumV(1)) { interp(withClockObject) } }

  test("Sending Messages to Objects") {
       assertResult(NumV(42)) {
        interp("""
        (let ((point
        (object ((field x 0) (field y 0))
          ((method get-x () x) (method get-y () y) (method set-x (nx) (set x nx))
           (method set-y (ny) (set y ny))))))
         (seq (msg point set-x 42) (msg point get-x)))
        """)
      }
    }


  test("Object Encapsulation") {
      intercept[InterpException] {
        interp("""
        (let ((point
        (object ((field x 0) (field y 0))
          ((method get-x () x) (method get-y () y) (method set-x (nx) (set x nx))
           (method set-y (ny) (set y ny))))))
        (msg point x))
        """)
      }
    }

 
  test("Self") {
      assertResult(NumV(1)) {
        interp("""
        (let ((point1
          (object ((field val 1))
            ((method get-value () val)
             (method set-value (nv) (set val nv))
             (method compare (p)
               (if (num< (msg p get-value) (msg self get-value))
                 p
                 self)))))
        (point2
          (object ((field val 2))
            ((method get-value () val)
             (method set-value (nv) (set val nv))
             (method compare (p)
               (if (num< (msg p get-value) (msg self get-value))
                 p
                 self))))))
    (msg (msg point1 compare point2) get-value))
        """)
      }
    }
 
  
   test("Message Forwarding") {
    assertResult(NumV(3)) {
      interp("""
      (letrec ((point2d
            (object
              ((field x 0)
               (field y 0))
              ((method get-x () x)
               (method get-y () y)
               (method set-x (nx) (set x nx))
               (method set-y (ny) (set y ny)))))
         (point3d
            (object-del point2d
              ((field z 0))
              ((method get-z () z)
               (method set-z (nz) (set z nz))))))
  (seq (msg point3d set-x 3)
       (msg point3d get-x)))
      """)
    }
  }
  
  
  test("late bind") {
    assertResult(ConsV(NumV(2), ConsV(NumV(4), NilV()))) {
      interp("""
      (letrec ((vehicle-factory
            (lambda ()
              (object
                ((field position 1))
                ((method speed-factor () 1) (method get-position () position)
                 (method move () (set position (* position (msg self speed-factor))))))))
         (car (object-del (vehicle-factory) () ((method speed-factor () 2))))
         (bicycle (object-del (vehicle-factory) () ((method speed-factor () 4)))))
    (seq (msg car move) (seq (msg bicycle move) (cons (msg car get-position) (cons (msg bicycle get-position) nil)))))
      """)
    }
  }


  /**
   * Helpers
   */

  def interp(expr: String): Value = Interp.interp(desugar(parse(expr)), Nil, Nil)._1
  def interp(expr: ExprC): Value = Interp.interp(expr, Nil, Nil)._1
  def interp(expr: ExprC, nv: PointerEnvironment, st: Store): (Value, Store) = Interp.interp(expr, nv, st)

}


```

Solution:
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
    case StringExt(str) => StringC(str)
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
      case "str=" => EqStrC(desugar(l), desugar(r))
      case "str++" => ConcStrC(desugar(l), desugar(r))
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
    case LetRecExt(binds, body) => AppC(FdC(binds.map(_.name), dummy(binds, body)), binds.map(x => UninitializedC())) 
    case ObjectExt(fields, methods)  => {
      val obj = AppC(FdC(fields.map(_.name), fil(fields, FdC(List("5command"),meth(methods)))), fields.map(x => UninitializedC()))
      AppC(FdC(List("self"), SeqC(SetC("self", obj), IdC("self"))), List(UninitializedC()))
    }
    case  ObjectDelExt(del, fields, methods) => {
      val obj = AppC(FdC(fields.map(_.name), fil(fields, FdC(List("5command"), meth(methods)))), fields.map(x => UninitializedC()))
      AppC(FdC(List("10command"), AppC(FdC(List("self"), SeqC(SetC("self", obj), IdC("self"))), List(UninitializedC()))), List(desugar(del)))
    }
    case MsgExt(recvr, msg, args) => AppC(FdC(List("self"), AppC(AppC(IdC("self"), List(StringC(msg))), IdC("self")::args.map(x => desugar(x)))), List(desugar(recvr)))
    case DoSeqExt(expr) => do_seq(expr)
    case _ => throw new DesException()
  }
  
  def do_seq(expr: List[ExprExt]): ExprC = expr match {
    case head::Nil => desugar(head)
    case head::tail => SeqC(desugar(head), do_seq(tail))
    case _ => throw new DesException()
  }
  
  // Used for objects
  def meth(methods: List[MethodExt]): ExprC = methods match {
    case MethodExt(name, args, body)::tail => IfC(EqStrC(StringC(name), IdC("5command")), FdC("self"::args, desugar(body)), meth(tail))
    case _ => AppC(IdC("10command"), List(IdC("5command")))
  }
  
  // Used for objects
  def fil(fields: List[FieldExt], body: ExprC): ExprC = fields match {
    case Nil => body
    case FieldExt(name, value)::tail => SeqC(SetC(name, desugar(value)), fil(tail, body))
  }
  
  // Used for Applications
  def dummy(binds: List[LetBindExt], body: ExprExt): ExprC = binds match {
    case LetBindExt(name, value)::Nil => SeqC(SetC(name, desugar(value)), desugar(body))
    case LetBindExt(name, value)::tail => SeqC(SetC(name, desugar(value)), dummy(tail, body))
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
    case StringC(str) => (StringV(str), st1)
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
    case EqStrC(l, r) => {
      val (l2, st2) = interp(l, nv, st1)
      val (r2, st3) = interp(r, nv, st2)
      (l2, r2) match {
        case (StringV(e1), StringV(e2)) => (BoolV(e1 == e2), st3)
        case _ => throw new InException()
      }
    }
    case ConcStrC(l, r) => {
      val (l2, st2) = interp(l, nv, st1)
      val (r2, st3) = interp(r, nv, st2)
      (l2, r2) match {
        case (StringV(e1), StringV(e2)) => (StringV(e1+e2), st3)
        case _ => throw new InException()
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
