Extra: Type Inference

Implement a type inference algorithm as specified in the assignment notes.

Summary:

    The basis for the language is an eager language with functions as first-class citizens and list operations.
    Implement a type inferrer for the sugar expressions as specified in the assignment notes and following the method discussed in Chapter 15.3.2 of the book.
    Develop more tests to support your understanding of the expected behavior of the type inference algorithm.
    Do not change the signature of the functions given, or you will run into compilation problems with specification tests.

```scala
import Untyped._

case class NotImplementedException() extends RuntimeException

object Desugar {
  def desugar(e: ExprExt): ExprC = {
    throw NotImplementedException()
  }
}

object Interp {
  type Environment = List[Bind]

  def interp(e: ExprC): Value = interp(e, Nil)

  def interp(e: ExprC, nv: Environment): Value = {
    throw NotImplementedException()
  }
}


object TypeInferer {
  type TEnvironment = List[TBind]
  type TSubstitution = List[TEq]
  
  def typeOf(expr: ExprExt): Type = typeOf(expr, Nil)
  
  def typeOf(expr: ExprExt, env: TEnvironment): Type = {
    val v = freshTVar()
    val cs = generate(expr, v, env)
    val subs = unify(cs, Nil)
    lookup(subs, v)
  }
  
  def generate(expr: ExprExt, t_expr: Type, tenv: TEnvironment): List[TEq] =
    throw NotImplementedException()
  
  def unify(cs: List[TEq], sub: TSubstitution): TSubstitution =
    throw NotImplementedException()
  
  def lookup(sub: TSubstitution, v: TVar): Type =
    throw NotImplementedException()
  
  
  var currentVarIndex = 0
  def freshTVar(): TVar = {
    currentVarIndex += 1
    TVar("t" + currentVarIndex)
  }

  val numT: Type = TCon(NumTC(), Nil)
  val boolT: Type = TCon(BoolTC(), Nil)
  def funT(argTys: List[Type], retTy: Type): Type = TCon(FunTC(), argTys ::: List(retTy))
  def listT(elemTy: Type): Type = TCon(ListTC(), List(elemTy))
  
  def prettyprintType(t: Type): String = t match {
    case TCon(NumTC(), Nil)      => "Num"
    case TCon(BoolTC(), Nil)     => "Bool"
    case TCon(ListTC(), List(ty)) => "(List " + prettyprintType(ty) + ")"
    case TCon(FunTC(), args)     => "((" + args.take(args.size - 1).map(prettyprintType).mkString(", ") + ") -> " + prettyprintType(args.last) + ")"
    case TVar(x)                 => x
    case _                       => "?"
  }
}

// The code below is used for grading.
// DO NOT EDIT.

object TypeChecker {
  case class InternalException(msg: String) extends TypeException(msg)

  def ty2comparable(t: Type): Type = t match {
    case TVar(x)     => CTVar(x)
    case TCon(f, cs) => CTCon(f, cs)
    case t =>
      throw InternalException("Expected a TVar or TCon as return type, but got: " + t)
  }

  def typeOf(expr: ExprExt): Type = typeOf(expr, Nil)
  def typeOf(expr: ExprExt, nv: List[TBind]): Type = ty2comparable(TypeInferer.typeOf(expr, nv))
}

object SafeInterp {
  def interp(e: ExprExt): Value = {
    TypeChecker.typeOf(e)
    Interp.interp(Desugar.desugar(e))
  }
}


```
