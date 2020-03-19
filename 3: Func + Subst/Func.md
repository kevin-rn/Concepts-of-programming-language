Summary:

1. Implement the interpreter as specified by the notes. You should consult chapter 5 and chapter 7 from the book to understand the concepts.
2. Develop more tests to support your understanding of the expected behaviour of the interpreter.
3. Do not change the signature of the functions given, or you’ll run into compilation problems with specification tests.

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

  def appCase(sexpr: SExpr): ExprExt = sexpr match {
    case SList(func::args) => AppExt(parse(func), args.map(x => parse(x)))
    case _ => throw new ParException()
  }
  
  def parse(sexpr: SExpr): ExprExt = sexpr match {
    case SNum(i) => NumExt(i)
    case SSym(str) => str match {
      case "true" => TrueExt()
      case "false" => FalseExt()
      case "nil" => NilExt()
      case _ => if(ExprExt.reservedWords.contains(str)) throw new ParException() else IdExt(str) 
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
      case SSym("lambda") => l match {
        case SList(ids) => {
          val params = ids.map{ case SSym(id) => id 
                              case _ => throw new ParException() }
          params.foreach(s => if(ExprExt.reservedWords.contains(s)) throw new ParException())
          if(params.distinct.size != params.size) throw new ParException()
          FdExt(params, parse(r)) } 
        case _ => throw new ParException()
      }
      case SSym("let") => l match {
        case SList(list) => {
          val binds = list.foldLeft(List[LetBindExt]()) {
            case (prev, next) => next match {
              case SList(List(SSym(name), value)) => prev :+ LetBindExt(name, parse(value))
              case _ => prev
            }
          }
          binds.foreach{ case LetBindExt(str, _) => if(ExprExt.reservedWords.contains(str)) throw new ParException() }
          if(binds.size <= 0 | binds.map(_.name).distinct.size != binds.map(_.name).size) throw new ParException()
          LetExt(binds, parse(r))
        }
        case _ => throw new ParException()
      }
      case _ => appCase(sexpr)
    }
    case SList(List(s, e)) => s match {
        case SSym("-") => UnOpExt("-", parse(e))
        case SSym("not") => UnOpExt("not", parse(e))
        case SSym("head") => UnOpExt("head", parse(e))
        case SSym("tail") => UnOpExt("tail", parse(e))
        case SSym("is-nil") => UnOpExt("is-nil", parse(e))
        case SSym("is-list") => UnOpExt("is-list", parse(e))
        case _ => appCase(sexpr)
    }
    case _ => appCase(sexpr)
  }
}

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
      case "num>" => LtC(desugar(r), desugar(l))
      case "cons" => ConsC(desugar(l), desugar(r))
      case _ => throw new DesException()
    }
    case UnOpExt(s, e) => s match {
      case "-" =>  MultC(NumC(-1), desugar(e))
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
    case _ => throw new DesException()
  }
}

object Interp {
  def intValue(v: Value): Int = v match { 
    case NumV(n) => n 
    case _ => throw new InException()
  }
  
  def subst(expr: ExprC, binds: List[Bind]): ExprC = expr match {
    case NumC(_) | NilC() | TrueC() | FalseC() | ValC(_) => expr
    case IdC(str) => {
      val value = binds.filter(_.name == str)
      if (value.size > 0) ValC(value(0).value) else expr
    }
    case TailC(e) => TailC(subst(e, binds))
    case HeadC(e) => HeadC(subst(e, binds))
    case IsNilC(e) => IsNilC(subst(e, binds))
    case IsListC(e) => IsListC(subst(e, binds))
    case PlusC(l, r) => PlusC(subst(l, binds), subst(r, binds))
    case MultC(l, r) => MultC(subst(l, binds), subst(r, binds))
    case ConsC(l, r) => ConsC(subst(l, binds), subst(r, binds))
    case EqNumC(l, r) => EqNumC(subst(l, binds), subst(r, binds))
    case LtC(l, r) => LtC(subst(l, binds), subst(r, binds))
    case IfC(c, t, e) => IfC(subst(c, binds), subst(t, binds), subst(e, binds))
    case AppC(f, args) => AppC(subst(f, binds), args.map(x => subst(x, binds)))
  	case FdC(params, body) => FdC(params, subst(body, binds.filter{ case Bind(s, v) => !params.contains(s)}))
    case _ => UndefinedC()
  }
  
  def interp(e: ExprC): Value = e match {
    case NilC() => NilV()
    case NumC(n) => NumV(n)
    case TrueC() => BoolV(true) 
    case FalseC() => BoolV(false)
    case ValC(v) => v
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
    case FdC(params, body) => FunV(FdC(params, body))
    case AppC(f, args) => interp(f) match {
      case FunV(FdC(params, body)) if(params.size == args.size) =>
      interp(subst(body, params.zip(args).map{ case (str, value) => Bind(str, interp(value))}))
      case _ => throw new InException()
    }
    case UndefinedC() | _ => throw new InException()
  }
}


```
