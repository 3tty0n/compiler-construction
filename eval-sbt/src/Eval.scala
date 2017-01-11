package nonscala

import Base._
import Abssyn._
import Oper._

object Eval {

  // 値
  trait Value

  case class IntVal(i: Int) extends Value

  case class BoolVal(b: Boolean) extends Value

  case class ListVal(l: List[Int]) extends Value

  // 関数
  case class FValue(xs: List[Var], e: Exp)

  def eval(fenv: Map[Var, FValue], env: Map[Var, Value], e: Exp): Value =
    e match {
      case IntExp(i) =>
        IntVal(i)
      case NilExp =>
        ListVal(Nil)
      case VarExp(v) =>
        env.getOrElse(v, throw new MyError("error"))
      case BOpExp(op, e1, e2) =>
        val v1 = eval(fenv, env, e1)
        val v2 = eval(fenv, env, e2)
        (op, v1, v2) match {
          case (PlusOp, IntVal(i1), IntVal(i2)) =>
            IntVal(i1 + i2)
          case (MinusOp, IntVal(i1), IntVal(i2)) =>
            IntVal(i1 - i2)
          case (TimesOp, IntVal(i1), IntVal(i2)) =>
            IntVal(i1 * i2)
          case (DivideOp, IntVal(i1), IntVal(i2)) =>
            IntVal(i1 / i2)
          case (EqOp, IntVal(b1), IntVal(b2)) =>
            BoolVal(b1 == b2)
          case (EqOp, ListVal(l1), ListVal(l2)) =>
            BoolVal(l1 == l2)
          case (LtOp, IntVal(b1), IntVal(b2)) =>
            BoolVal(b1 < b2)
          case (ConsOp, IntVal(i1), IntVal(i2)) =>
            ListVal(i1 :: i2 :: Nil)
          case (ConsOp, IntVal(i), ListVal(l)) =>
            ListVal(i :: l)
        }
      case IfExp(exp, e1, e2) =>
        eval(fenv, env, exp) match {
          case BoolVal(bool) if bool =>
            eval(fenv, env, e1)
          case BoolVal(bool) if !bool =>
            eval(fenv, env, e2)
        }
      case UOpExp(o, e1) =>
        val v1 = eval(fenv, env, e1)
        (o, v1) match {
          case (IsEmptyOp, ListVal(Nil)) => BoolVal(true)
          case (IsEmptyOp, ListVal(_ :: _)) => BoolVal(false)
          case (HeadOp, ListVal(h :: t)) => IntVal(h)
          case (TailOp, ListVal(h :: t)) => ListVal(t)
        }
      case AppExp(f, es) =>
        val FValue(xs, body) = fenv(f)
        val vs = es.map(e => eval(fenv, env, e))
        eval(fenv, xs.zip(vs).toMap, body)
    }

  def defs2env(ds: List[Def]): Map[Var, FValue] =
    ds.map(d => (d.name, FValue(d.args.map(_._1), d.body))).toMap
}


