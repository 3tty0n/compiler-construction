import Base._

object Abssyn {
  trait Exp
  case class VarExp(s: Var) extends Exp
  case class IntExp(i: Int) extends Exp
  case class PlusExp(e1: Exp, e2: Exp) extends Exp
  case class MinusExp(e1: Exp, e2: Exp) extends Exp
  case class TimesExp(e1: Exp, e2: Exp) extends Exp
  case class DevideExp(e1: Exp, e2: Exp) extends Exp
}



