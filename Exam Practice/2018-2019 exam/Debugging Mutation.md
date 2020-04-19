The solution template contains a desugarer and interpreter for a language with functions, mutation, and recursion. (Grammar below.)

The behavior of the desugarer and interpreter in the solution template should be similar to the behavior of the language that you implemented in the lab assignment week 5; i.e., a call-by-value language with statically-scoped functions, left-to-right order of evaluation, mutable boxes, and mutable lexical variables. However, the desugarer and interpreter in the solution template contains multiple bugs.

Find the bugs and fix them.

Note: the lookup and fetch functions are predefined and given as library functions. You can assume that these functions are correctly implemented.
Grammar

<expr> ::= true
         | false
         | <num>
         | (+ <expr> <expr>)
         | (- <expr> <expr>)
         | (* <expr> <expr>)
         | (- <expr>)
         | (num= <expr> <expr>)
         | (num< <expr> <expr>)
         | (num> <expr> <expr>)
         | (and <expr> <expr>)
         | (or <expr> <expr>)
         | (if <expr> <expr> <expr>)
         | (not <expr>)
         | (<expr> <expr>...)
         | <id>
         | (lambda (<id>...) <expr>)
         | (box <expr>)
         | (unbox <expr>)
         | (setbox <expr> <expr>)
         | (seq <expr> <expr>)
         | (set <id> <expr>)
         | (letrec ((<id> <expr>)...) <expr>)
         | (cons <expr> <expr>)
         | nil
         | (is-nil <expr>)
         | (head <expr>)
         | (tail <expr>)
         | (is-list <expr>)

### Template:
```scala
import Library._

object Desugar {
  
  def desugar(e: ExprExt): ExprC = e match {
    case NumExt(n) =>
      NumC(n)
    case TrueExt() =>
      TrueC()
    case FalseExt() =>
      FalseC()
    case IfExt(c, t, e) =>
      IfC(desugar(c), desugar(t), desugar(e))
    case BinOpExt(s, l, r) =>
      val (dl, dr) = (desugar(l), desugar(r))
      s match {
        case "+" =>
          PlusC(dl, dr)
        case "*" =>
          MultC(dl, dr)
        case "-" =>
          PlusC(dl, MultC(NumC(-1), dr))
        case "and" =>
          IfC(dl, dr, FalseC())
        case "or" =>
          IfC(dl, TrueC(), dr)
        case "num=" =>
          EqNumC(dl, dr)
        case "num<" =>
          LtC(dl, dr)
        case "num>" =>
          LtC(dr, dl)
        case "cons" =>
          ConsC(dl, dr)
        case "setbox" =>
          SetboxC(dl, dr)
        case "seq" =>
          SeqC(dl, dr)
        case _ =>
          throw DesugarException("Desugaring failed -- unknown binary operator: " + s)
      }
    case UnOpExt(s, eu) =>
      val de = desugar(eu)
      s match {
        case "-" =>
          de
        case "not" =>
          IfC(de, FalseC(), TrueC())
        case "head" =>
          HeadC(de)
        case "tail" =>
          TailC(de)
        case "is-nil" =>
          IsNilC(de)
        case "is-list" =>
          IsListC(de)
        case "box" =>
          BoxC(de)
        case "unbox" =>
          UnboxC(de)
        case _ =>
          throw DesugarException("Desugaring failed -- unknown unary operator: " + s)
      }
    case SetExt(x, e) =>
      SetC(x, desugar(e))
    case AppExt(f, as) =>
      AppC(desugar(f), as.map(desugar))
    case IdExt(id) =>
      IdC(id)
    case FdExt(params, body) =>
      FdC(params, desugar(body))
    case LetExt(binds, body) =>
      val names = binds.map({ case LetBindExt(name, _) => name })
      val exprs = binds.map({ case LetBindExt(_, expr) => desugar(expr) })
      AppC(FdC(names, desugar(body)), exprs)
    case LetRecExt(binds, body) =>
      val names = binds.map({ case LetBindExt(name, _) => name })
      val args  = binds.map(_ => UninitializedC())
      val bodyc = binds.foldRight[ExprC](desugar(body))((b, e) => 
                    SeqC(SetC(b.name, desugar(b.value)), e))
      AppC(FdC(names, bodyc), args)
    case NilExt() =>
      NilC()
    case e =>
      throw DesugarException("Desugaring failed for expression: " + e)
  }
  
}

object Interp { 

  def interp(e: ExprC): Value = interp(e, Nil, Nil)._1

  def interp(e: ExprC, nv: List[Pointer], st1: List[Cell]): (Value, List[Cell]) = e match {
    case TrueC() =>
      (BoolV(true), st1)
    case FalseC() =>
      (BoolV(false), st1)
    case NumC(n) =>
      (NumV(n), st1)
    case PlusC(l, r) =>
      val (vl, st2) = interp(l, nv, st1)
      val (vr, st3) = interp(r, nv, st2)
      (vl, vr) match {
        case (NumV(nl), NumV(nr)) =>
          (NumV(nl + nr), st3)
        case r =>
          throw InterpException("Expected both branches of + to evaluate to a number, but got: " + r)
      }
    case MultC(l, r) =>
      val (vl, st2) = interp(l, nv, st1)
      val (vr, st3) = interp(r, nv, st2)
      (vl, vr) match {
        case (NumV(nl), NumV(nr)) =>
          (NumV(nl * nr), st3)
        case r =>
          throw InterpException("Expected both branches of * to evaluate to a number, but got: " + r)
      }
    case EqNumC(l, r) =>
      val (vl, st2) = interp(l, nv, st1)
      val (vr, st3) = interp(r, nv, st2)
      (vl, vr) match {
        case (NumV(nl), NumV(nr)) =>
          (BoolV(nl == nr), st3)
        case r =>
          throw InterpException("Expected both branches of num= to evaluate to a number, but got: " + r)
      }
    case LtC(l, r) =>
      val (vl, st2) = interp(l, nv, st1)
      val (vr, st3) = interp(r, nv, st2)
      (vl, vr) match {
        case (NumV(nl), NumV(nr)) =>
          (BoolV(nl < nr), st3)
        case r =>
          throw InterpException("Expected both branches of num= to evaluate to a number, but got: " + r)
      }
    case ConsC(hd, tl) =>
      val (vhd, st2) = interp(hd, nv, st1)
      val (vtl, st3) = interp(tl, nv, st1)
      (ConsV(vhd, vtl), st3)
    case NilC() =>
      (NilV(), st1)
    case IsNilC(e) =>
      val (v, st2) = interp(e, nv, st1)
      v match {
        case NilV() => (BoolV(true), st2)
        case ConsV(_, _) => (BoolV(false), st2)
        case r =>
          throw InterpException("Expression is-nil expects a list value, but got: " + r)
      }
    case HeadC(e) =>
      e match {
        case ConsC(hd, _) => interp(hd, nv, st1)
        case r =>
          throw InterpException("Expression head expects a cons value, but got: " + r)
      }
    case TailC(e) =>
      val (v, st2) = interp(e, nv, st1)
      v match {
        case ConsV(_, tl) => (tl, st2)
        case r =>
          throw InterpException("Expression tail expects a cons value, but got: " + r)
      }
    case IsListC(e) =>
      val (v, st2) = interp(e, nv, st1)
      v match {
        case NilV() => (BoolV(true), st2)
        case ConsV(_, _) => (BoolV(true), st2)
        case _ => (BoolV(false), st2)
      }
    case IfC(c, t, e) =>
      val (b, st2) = interp(c, nv, st1)
      b match {
        case BoolV(true) => interp(t, nv, st2)
        case BoolV(false) => interp(e, nv, st2)
        case r =>
          throw InterpException("Expected conditional expression of 'if' to yield a boolean value, but got: " + r)
      }
    case AppC(f, as) =>
      val (fv, st2) = interp(f, nv, st1)
      val (avs, st3) = interpExprs(as, nv, st2)
      fv match {
        case PointerClosV(FdC(xs, e), nv_clo) =>
          if (xs.length == avs.length) {
            val (nv_new, st4) = extendEnv(xs, avs, Nil, st3)
            interp(e, nv_new ::: nv_clo ::: nv, st4)
          } else {
            throw InterpException("Arity of function closure does not match argument list: " + (fv, avs))
          }
        case r =>
          throw InterpException("Expected first branch of application expression to yield a function, but got: " + r)
      }
    case IdC(s)   => (fetch(lookup(s, nv), st1), st1)
    case fdc: FdC => (PointerClosV(fdc, nv), st1)

    case BoxC(e) =>
      val (v, st2) = interp(e, nv, st1)
      val loc = newLoc(st2)
      (BoxV(loc), extendStore(loc, v, st2))
    case UnboxC(e) =>
      val (v, st2) = interp(e, nv, st1)
      v match {
        case BoxV(l) =>
          (fetch(l, st2), st2)
        case r =>
          throw InterpException("Expression unbox expects a box value, but got: " + r)
      }
    case SetboxC(b, v) =>
      val (vv, st2) = interp(v, nv, st1)
      val (vb, st3) = interp(b, nv, st2)
      vb match {
        case BoxV(l) =>
          (vv, extendStore(l, vv, st3))
        case r =>
          throw InterpException("Expression setbox expects a box value, but got: " + r)
      }
    case SetC(x, e) =>
      val (v, st2) = interp(e, nv, st1)
      val loc      = lookup(x, nv)
      (v, extendStore(loc, v, st2))
    case SeqC(e1, e2) =>
      val (_, st2) = interp(e1, nv, st1)
      val (v, st3) = interp(e2, nv, st2)
      (v, st3)
      
    case UninitializedC() =>
      (UninitializedV(), st1)

    case e =>
      throw InterpException("Interpretation failed for expression: " + e)
  }
  
  def arithBinOp(l: ExprC, r: ExprC, nv: List[Pointer], st1: List[Cell])(op: (Int, Int) => Value) = {
    val (lv, st2) = interp(l, nv, st1)
    val (rv, st3) = interp(r, nv, st2)
    (lv, rv) match {
      case (NumV(n1), NumV(n2)) =>
        (op(n1, n2), st3)
      case r =>
        throw InterpException("Operator expected both branches to return a number, but got: " + r)
    }
  }
  
  def interpExprs(es: List[ExprC], nv: List[Pointer], st1: List[Cell]): (List[Value], List[Cell]) =
    es match {
      case Nil => (Nil, st1)
      case e :: es =>
        val (v, st2)  = interp(e, nv, st1)
        val (vs, st3) = interpExprs(es, nv, st2)
        (v :: vs, st3)
    }

  
  def extendEnv(names: List[String], vals: List[Value], nv: List[Pointer], st1: List[Cell]): (List[Pointer], List[Cell]) =
    (names, vals) match {
      case (Nil, Nil) =>
        (nv, st1)
      case (x :: xs, v :: vs) =>
        val loc = newLoc(st1)
        extendEnv(xs, vs, Pointer(x, loc) :: nv, extendStore(loc, v, st1))
      case _ =>
        throw InterpException("Arity of function closure does not match argument list")
    }
  
  def newLoc(s: List[Cell]): Int =
    (-1 :: s.map { _.location }).max + 1
  
  def extendStore(loc: Int, v: Value, st: List[Cell]): List[Cell] =
    Cell(loc, v) :: st.filter { _.location != loc }
  
}

// DO NOT EDIT BELOW THIS LINE

sealed abstract class ExprExt
case class BinOpExt(s: String,  l: ExprExt, r: ExprExt)          extends ExprExt // (+ e1 e2)
case class UnOpExt (s: String,  e: ExprExt)                      extends ExprExt // (- e)
case class IfExt   (c: ExprExt, t: ExprExt, e: ExprExt)          extends ExprExt // (if c t e)
case class AppExt  (f: ExprExt, args: List[ExprExt])             extends ExprExt // (f a b c)
case class IdExt   (c: String)                                   extends ExprExt // x
case class LetExt  (binds: List[LetBindExt], body: ExprExt)      extends ExprExt // (let ((x1 e1) (x2 e2) ...) e)
case class FdExt(params: List[String], body: ExprExt)            extends ExprExt // (lambda (x...) e)
case class NumExt  (num: Int)                                    extends ExprExt // 4
case class TrueExt ()                                            extends ExprExt // true
case class FalseExt()                                            extends ExprExt // false
case class SetExt(v: String, b: ExprExt)                         extends ExprExt // (set v b)
case class NilExt()                                              extends ExprExt // nil
case class LetRecExt(binds: List[LetBindExt], body: ExprExt)     extends ExprExt // (letrec ((x1 e1) (x2 e2) ...) e)

case class LetBindExt(name: String, value: ExprExt)

object ExprExt {
  val binOps        = Set("+", "*", "-", "and", "or", "num=", "num<", "num>", "cons", "seq", "setbox")
  val unOps         = Set("-", "not", "is-nil", "is-list", "head", "tail", "box", "unbox")
  val reservedWords = binOps ++ unOps ++
    Set("list", "if", "lambda", "let", "true", "false", "set", "letrec", "nil")
}

sealed abstract class ExprC                                             extends Product
case class TrueC   ()                                            extends ExprC // true
case class FalseC  ()                                            extends ExprC // false
case class NumC    (num: Int)                                    extends ExprC // 4
case class PlusC   (l: ExprC, r: ExprC)                          extends ExprC // (+ e1 e2)
case class MultC   (l: ExprC, r: ExprC)                          extends ExprC // (* e1 e2)
case class EqNumC  (l: ExprC, r: ExprC)                          extends ExprC // (num= e1 e2)
case class LtC(l: ExprC, r: ExprC)                               extends ExprC // (num< e1 e2)
case class IfC     (c: ExprC, t: ExprC, e: ExprC)                extends ExprC // (if c t e)
case class AppC    (f: ExprC, args: List[ExprC])                 extends ExprC // (f args)
case class IdC     (c: String)                                   extends ExprC // x
case class FdC     (params: List[String], body: ExprC)           extends ExprC // (lambda (x) (+ x x))
case class BoxC(v: ExprC)                                        extends ExprC // (box e)
case class UnboxC(b: ExprC)                                      extends ExprC // (unbox e)
case class SetboxC(b: ExprC, v: ExprC)                           extends ExprC // (setbox l r)
case class SeqC(b1: ExprC, b2: ExprC)                            extends ExprC // (seq l r)
case class SetC(v: String, b: ExprC)                             extends ExprC // (set v b)
case class UninitializedC()                                      extends ExprC // no surface syntax!
case class LetRecC(binds: List[LetBindC], body: ExprC)           extends ExprC // (letrec ((x1 e1) (x2 e2) ...) e)
case class ConsC(head: ExprC, tail: ExprC)                       extends ExprC // (cons e1 e2)
case class NilC()                                                extends ExprC // nil
case class IsNilC(e: ExprC)                                      extends ExprC // (is-nil e)
case class HeadC(e: ExprC)                                       extends ExprC // (head e)
case class TailC(e: ExprC)                                       extends ExprC // (tail e)
case class IsListC(e: ExprC)                                     extends ExprC // (is-list e)

case class LetBindC(name: String, value: ExprC)

sealed abstract class Value
case class NumV (v: Int)                                         extends Value
case class BoolV(v: Boolean)                                     extends Value
case class PointerClosV(f: FdC, env: List[Pointer])              extends Value
case class BoxV(l: Int)                                          extends Value
case class ConsV(head: Value, tail: Value)                       extends Value
case class NilV()                                                extends Value
case class UninitializedV()                                      extends Value // for letrecs

case class Cell(location: Int, value: Value)
case class Pointer(name: String, location: Int)


```

### Test:
```scala
//test: Test

// test of the solution

import org.scalatest.FunSuite

import Desugar._
import Interp._

class Test extends FunSuite {
  
  def run(str: String): Value = Interp.interp(Desugar.desugar(Parser.parse(str)), Nil, Nil)._1

  test("Unary minus") {
    assertResult(NumV(-1)) {
      run("(- 1)")
    }
  }

  test("Function application") {
    assertResult(NumV(1)) {
      run("((lambda (x) x) 1)")
    }
  }
  
  test("List") {
    assertResult(NumV(1)) {
      run("(head (cons 1 nil))")
    }
    
    assertResult(NilV()) {
      run("(tail (cons 1 nil))")
    }
  }
  
  test("Conditional") {
    assertResult(NumV(0)) {
      run("(if (num> 10 0) 0 1)")
    }
  }
  
  test("Box") {
    assertResult(NumV(10)) {
      run("""
        (let ((b (box 1)))
          (seq (setbox b 10)
               (unbox b)))
      """)
    }
  }
  
}

```

__________________________________________________________________________________________________________________________________


### Solution:
```scala

```
