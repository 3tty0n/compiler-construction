import Base._

object Oper {
  trait BOp                         // 二項(Binary)演算子
  case object PlusOp extends BOp
  case object MinusOp extends BOp
  case object TimesOp extends BOp
  case object DivideOp extends BOp
  case object EqOp extends BOp       // E == E
  case object LtOp extends BOp       // E < E
  case object ConsOp extends BOp     // E :: C

  trait UOp                        // 課題(a)では必要ない
  case object IsEmptyOp extends UOp
  case object HeadOp extends UOp
  case object TailOp extends UOp
}

object Ty {                        // 課題(a)では必要ない
  trait Ty
  case object IntTy extends Ty
  case object BoolTy extends Ty
  case object IntListTy extends Ty
}

object Abssyn {
  import Oper._
  import Ty._
  
  trait Exp
  case class VarExp(s: Var) extends Exp
  case class IntExp(i: Int) extends Exp
  case object NilExp extends Exp
  case class UOpExp(o: UOp, e: Exp) extends Exp       // 課題(a)では必要ない
  case class BOpExp(o: BOp, e1: Exp, e2: Exp) extends Exp
  case class IfExp(e: Exp, e1: Exp, e2: Exp) extends Exp
  case class AppExp(f: Var, es: List[Exp]) extends Exp // 課題(a)では必要ない

  case class Def(name: Var, args: List[(Var,Ty)], rtype: Ty, body: Exp)
  case class Prog(funcs: List[Def])
}


