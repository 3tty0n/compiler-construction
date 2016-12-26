package nonscala

import Base._
import Abssyn._
import Oper._
import Ty._

object TypeCheck {

  class TypeError(message: String = null, cause: Throwable = null)
  extends RuntimeException(message, cause)

  def typeError(m: String) = throw new TypeError(m)

  case class FuncTy(ts: List[Ty], result: Ty)

  // 式 e の型を返す
  // 型エラーがあった場合は, 例外 TypeError を発生させる
  // tcheck
  def tCheck(fenv: Map[Var, FuncTy], env: Map[Var, Ty], e: Exp): Ty = e match {
    case VarExp(x) => env.getOrElse(x, throw new TypeError(s"Cannot find variable: $x"))
    case IntExp(_) => IntTy
    case NilExp => IntListTy
    case UOpExp(o, e1) =>
      val t1 = tCheck(fenv, env, e1)
      (o, t1) match {
        case (IsEmptyOp, IntListTy) => BoolTy
        case (HeadOp, IntListTy) => IntTy
        case (TailOp, IntListTy) => IntListTy
        case _ => throw new TypeError(s"isEmpty, head, and tail should be applied for List[Int]")
      }
    case AppExp(f, es) =>
      val typeList = es.map(e => tCheck(fenv, env, e))
      val FuncTy(funcTypes, result) = fenv.getOrElse(f, throw new TypeError(s"Cannot get type of function $f"))
      if ((funcTypes, typeList).zipped.forall { case (t1, t2) => t1 == t2 }) {
        result
      } else {
        throw new TypeError(s"types of arguments of function $f are not equaled")
      }
    case BOpExp(o, e1, e2) =>
      val t1 = tCheck(fenv, env, e1)
      val t2 = tCheck(fenv, env, e2)
      o match {
        case PlusOp | MinusOp | TimesOp | DivideOp =>
          (t1, t2) match {
            case (IntTy, IntTy) => IntTy
            case _ => throw new TypeError("+, -, *, / should be (Int) <op> (Int)")
          }
        case LtOp =>
          (t1, t2) match {
            case (IntTy, IntTy) => BoolTy
            case _ => throw new TypeError("< should be (Int) <op> (Int)")
          }
        case EqOp =>
          (t1, t2) match {
            case (IntTy, IntTy) => BoolTy
            case _ => throw new TypeError(s"== should be (Int) <op> (Int)")
          }
        case ConsOp =>
          (t1, t2) match {
            case (IntTy, IntListTy) => IntListTy
            case _ => throw new TypeError(s":: should be (Int) <op> (List[Int])")
          }
        case _ =>
          throw new TypeError(s"+, -, *, /, <, ==, :: should be applied here, but actual $o")
      }
    case IfExp(exp, e1, e2) =>
      tCheck(fenv, env, exp) match {
        case BoolTy =>
          val t1 = tCheck(fenv, env, e1)
          val t2 = tCheck(fenv, env, e2)
          (t1, t2) match {
            case _ if t1 == t2 => t1
            case _ => throw new TypeError("If expression should be if (boolean) <same type> else <same type>")
          }
        case _ => throw new TypeError("If expression should have boolean expression first")
      }
  }

  def defs2fenv(ds: List[Def]): Map[Var, FuncTy] = {
    ds.map { d =>
      (d.name, FuncTy(d.args.map(_._2), d.rtype))
    }.toMap
  }

  def defs2env(ds: List[Def]): Map[Var, Ty] = {
    ds.flatMap(_.args).map { arg =>
      (arg._1, arg._2)
    }.toMap
  }

  // 型エラーがあった場合は, 例外 TypeError を発生させる
  def tCheckDefs(ds: List[Def]): List[Ty] = {
    val fenv = defs2fenv(ds)
    val env = defs2env(ds)
    ds.map(d => tCheck(fenv, env, d.body))
  }
}
