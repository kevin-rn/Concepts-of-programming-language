Week 6: Type Checker

Implement a type checker as specified in the assignment notes.

Summary:

    The basis for the language is the language you implemented in Week 5.
    You can probably reuse and adapt some of the code from last week, however, be aware of the differences.
    When your implementation was not clean and elegant, try to fix that this time.

    Implement a type checker for the sugar expressions as specified in the assignment notes and following the method discussed in Chapter 15 of the book.

    Extend the implementation with pair operations as specified in the assignment notes.

    Do not change the signature of the functions given, or you will run into compilation problems with specification tests.

______________________________________________________________________________________________________________________________

### Solutions:
```scala
import Library._
import Typed._

case class NotImplementedException(s: String) extends RuntimeException(s)
case class DesException() extends DesugarException()
case class InException() extends InterpException()
case class TypException() extends TypeException()

object Desugar {
  def desugar(e: ExprExt): ExprC = e match {
    case NilExt(listTy) => NilC()
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
      case "pair" => PairC(desugar(l), desugar(r))
      case _ => throw new DesException()
    }
    case UnOpExt(s, e) => s match {
      case "-" =>  MultC(NumC(-1), desugar(e))
      case "not" => IfC(desugar(e), FalseC(), TrueC())
      case "head" => HeadC(desugar(e))
      case "tail" => TailC(desugar(e))
      case "is-nil" => IsNilC(desugar(e))
      case "box" => BoxC(desugar(e))
      case "unbox" => UnboxC(desugar(e))
      case "fst" => FstC(desugar(e))
      case "snd" => SndC(desugar(e))
      case _ => throw new DesException()
    }
    case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))
    case ListExt(listTy, list) => list match {
      case head::tail => ConsC(desugar(head), desugar(ListExt(listTy, tail)))
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
    case FdExt(params, body) => FdC(params.map(_.name), desugar(body))
    case LetExt(binds, body) => {
      val params = binds.map{ case LetBindExt(x, y) => x
                              case _ => throw new DesException()}
      val args = binds.map{ case LetBindExt(x, y) => desugar(y) 
                            case _ => throw new DesException()}
      AppC(FdC(params, desugar(body)), args)
    }
    case RecLamExt(name, paramTy, retTy, param, body) => {
      val func = FdC(List("x"), AppC(IdC("f"), List(FdC(List("v"), AppC(AppC(IdC("x"), List(IdC("x"))), List(IdC("v")))))))
      val YComb = FdC(List("f"), AppC(func, List(func)))
      AppC(YComb, List(FdC(List(name), FdC(List(param), desugar(body)))))
    }
    case SetExt(id, e) => SetC(id, desugar(e))
    case LetRecExt(binds, body) => AppC(FdC(binds.map(x => x.name), dummy(binds.map(x => LetBindExt(x.name, x.value)), body)), binds.map(x => UninitializedC())) 
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
    case PairC(l, r) => {
      val (e1, st2) = interp(l, nv, st1)
      val (e2, st3) = interp(r, nv, st2)
      (PairV(e1, e2), st3)
    }
    case FstC(e) => interp(e, nv, st1) match {
      case (PairV(l, r), st2) => (l, st2)
      case _ => throw new InException()
    }
    case SndC(e) => interp(e, nv, st1) match {
      case (PairV(l, r), st2) => (r, st2)
      case _ => throw new InException()
    }
    case UninitializedC() => (UninitializedV(), st1)
    case _ => throw new InException()
  }
}

object TypeChecker {
  type TEnvironment = List[TBind]
  
  def typeOf(e: ExprExt): Type = typeOf(e, Nil)
  
  // searches for correct type
  def lookup(e: String, nv: TEnvironment): Type = nv match {
    case TBind(str, ty)::tail => if(str==e) ty else lookup(e, tail)
    case _ => throw new TypException()
  }

  def typeOf(e: ExprExt, nv: TEnvironment = Nil): Type = e match {
    case NumExt(n) => NumT()
    case TrueExt() | FalseExt() => BoolT()
    case NilExt(listTy) => ListT(listTy)
    case IdExt(c) => lookup(c, nv)
    case BinOpExt(s, l, r) => s match {
      case "+" | "*" | "-" => (typeOf(l, nv), typeOf(r, nv)) match {
        case (NumT(), NumT()) => NumT()
        case (_,_) => throw TypException()
      }
      case "and" | "or" => (typeOf(l, nv), typeOf(r, nv)) match {
          case (BoolT(), BoolT()) => BoolT()
          case  (_,_) => throw TypException()
      }
      case "num=" | "num<" | "num>" => (typeOf(l, nv), typeOf(r, nv)) match {
          case (NumT(), NumT()) => BoolT()
          case  (_,_) => throw TypException()
      }
      case "cons" => (typeOf(l, nv), typeOf(r, nv)) match {
          case (e1, ListT(e2)) if(e1 == e2) => ListT(e1)
          case (_,_) => throw TypException()
      }
      case "setbox" => (typeOf(l, nv), typeOf(r, nv)) match { 
        case (RefT(e1), e2) if(e1 == e2) => e1
        case (_,_) => throw TypException()
      }
      case "seq" => val ty = typeOf(r, nv); if(typeOf(l, nv) == ty) ty else throw TypException()
      case "pair" => PairT(typeOf(l, nv), typeOf(r, nv))
      case _ => throw TypException()
    }
    case UnOpExt(s, e) => s match {
      case "-" => if(typeOf(e, nv) == NumT()) NumT() else throw TypException()
      case "not" => if(typeOf(e, nv) == BoolT()) BoolT() else throw TypException()
      case "head" | "tail" => typeOf(e, nv) match {
        case ListT(ty) => if(s =="head") ty else ListT(ty)
        case _ => throw TypException()
      }
      case "is-nil" => typeOf(e, nv) match {
        case ListT(_) => BoolT()
        case _ => throw TypException()
      }
      case "fst" | "snd" => typeOf(e, nv) match {
        case PairT(l, r) => if(s =="fst") l else r
        case _ => throw TypException()
      }
      case "box" => RefT(typeOf(e, nv))
      case "unbox" => typeOf(e, nv) match {
        case RefT(ty) => ty
        case _ => throw TypException()
      }
      case _ => throw TypException()
    }
    case ListExt(listTy, list) => {
      list.foreach(x => if(typeOf(x, nv) != listTy) throw TypException())
      ListT(listTy)
    }
    case IfExt(c, t, e) => typeOf(c, nv) match {
      case BoolT() => val ty = typeOf(t, nv); if (ty == typeOf(e, nv)) ty else throw new TypException()
      case _ => throw TypException()
    }
    case FdExt(params, body) => FunT(params.map(_.ty), typeOf(body, params.map(x => TBind(x.name, x.ty)):::nv))
    case AppExt(f, args) => typeOf(f, nv) match {
      case FunT(paramTy, retTy) => {
        if(paramTy.size != args.size) throw TypException()
        paramTy.zip(args).foreach{ case (p, a) => if (p != typeOf(a, nv)) throw TypException() }
        retTy
      }
      case _ => throw TypException()
    }
    case LetExt(binds, body) => typeOf(body, binds.map(x => TBind(x.name, typeOf(x.value, nv))):::nv)
    case RecLamExt(name, paramTy, retTy, param, body) => {
      val ty = typeOf(body, TBind(name,FunT(List(paramTy),retTy))::TBind(param, paramTy)::nv)
       if(ty == retTy) FunT(List(paramTy), retTy) else throw TypException()
    }
    case SetExt(id, e) => { val ty = typeOf(e, nv); if(lookup(id, nv) == ty) ty else throw TypException() }
    case LetRecExt(binds, body) => {
      val nv2 = binds.map(y => TBind(y.name, y.ty)):::nv
      val bnds = binds.foreach(x => if(typeOf(x.value, nv2) != x.ty) throw TypException() )
      typeOf(body, nv2)
    }
    case _ => throw TypException()
  }
}

object SafeInterp {

  def interp(e: ExprExt): Value = {
    TypeChecker.typeOf(e)
    Interp.interp(Desugar.desugar(e))
  }

}
```
