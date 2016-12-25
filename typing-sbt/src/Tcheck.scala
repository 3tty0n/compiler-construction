package nonscala
import Base._
import Abssyn._
import Oper._
import Ty._

object TypeCheck {

  class TypeError(s: String) extends Throwable
  def typeError(s: String) = throw(new TypeError(s))

  case class FuncTy(ts: List[Ty], result: Ty)

  // 式 e の型を返す
  // 型エラーがあった場合は, 例外 TypeError を発生させる
  // tcheck
  def tcheck (fenv: Map[Var, FuncTy], env: Map[Var, Ty], e: Exp) : Ty =
    e match {
      case VarExp(x) =>
        env.get(x) match {
          case Some(t) => t
          case None => typeError("Cannot find variable: "+x)
        }
      case UOpExp(o, e1) => {
        val t1 = tcheck (fenv, env, e1)
          (o, t1) match {
          case (IsEmptyOp, IntListTy) => BoolTy
          case (HeadOp, IntListTy) => IntTy
          case (TailOp, IntListTy) => IntListTy
          case _ => typeError("UOpExp")
        }
      }
/*      case AppExp(f, es) => {
        val FuncTy(ts,t) = fenv(f)
        ... 
        // Listのmapを使うと簡単            
      }  */

    }

  def defs2fenv (ds: List[Def]): Map[Var, FuncTy] =
    ds.map(d =>
      (d.name, FuncTy(d.args.map(_._2), d.rtype))).toMap

  // 型エラーがあった場合は, 例外 TypeError を発生させる
  def tcheckDefs (ds: List[Def]): Unit = {
    val fenv = defs2fenv(ds)
    for (d <- ds)
      tcheck(fenv, Map[Var, Ty](), d.body)
  }
}
