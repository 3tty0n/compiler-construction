package nonscala

import Base._
import Oper._
import CodegenBase._
import Asm._

object Codegen {
  def v2operand (v:IL.Val): Operand =
    v match {
      case IL.IntVal(i) => IntOpd(i)
      case IL.VarVal(x) => RegOpd(x)
      case IL.NilVal => IntOpd(0)
    }

  // 代入文 d <- e のコードを生成
  // 変数 d が式 e に現れないことを仮定してよい
  def genExp (e: IL.Exp, d: Reg) : List[Instr] =
    e match {
      case IL.ValExp(v) =>
        List(Movq(v2operand(v), d))
      case IL.BOpExp(PlusOp, v1, v2) =>
        error () // 適切なプログラムに置き換えてください
      case IL.BOpExp(MinusOp, v1, v2) =>
        error () // 適切なプログラムに置き換えてください
      case IL.BOpExp(TimesOp, v1, v2) =>
        error () // 適切なプログラムに置き換えてください
      case IL.BOpExp(DivideOp, v1, v2) => {
        val r = freshVar()
        List(Movq(v2operand(v1), "%rax"), Cqto,
          Movq(v2operand(v2), r), Idivq(r), Movq(RegOpd("%rax"), d))
      }
      case IL.CallExp(f, vs) => {
        if (vs.length > 6) error();
        error () // 適切なプログラムに置き換えてください
      }
      case IL.UOpExp(HeadOp, IL.VarVal(x)) => 
        List(Loadq(MemReg(x, 0), d))
      case IL.UOpExp(TailOp, IL.VarVal(x)) => 
        List(Loadq(MemReg(x, 8), d))
      case IL.UOpExp(HeadOp, v1) => notSupported("HeadOp")
      case IL.UOpExp(TailOp, v1) => notSupported("TailOp")
      case IL.BOpExp(ConsOp, v1, v2) => 
        List(
          Movq(v2operand(v1), argRegs(0)),
          Movq(v2operand(v2), argRegs(1)),
          Callq("cons", 2),
          Movq(RegOpd(retReg),d))
    }

  def cmp(o1: Operand, o2: Operand): List[Instr] = {
    val r = freshVar()
    List(Movq(o1, r), Cmpq(o2, r))
  }

  def genCExp (e:IL.CExp) : (List[Instr], CC) =
    e match {
      case IL.EqCExp(v1, v2) => 
        (cmp(v2operand(v1), v2operand(v2)), EqCC)
      case IL.LtCExp(v1, v2) => 
        (cmp(v2operand(v1), v2operand(v2)), LtCC)
      case IL.IsEmptyCExp(v1) =>
        (cmp(v2operand(v1), v2operand(IL.IntVal(0))), EqCC)
    }

  def genInstr (s:IL.Instr) : Code =
    s match {
      case IL.AssignInstr(x, e) => genExp(e, x)
      case IL.LabelInstr(l) => List(LabelInstr(l))
      case IL.GotoInstr(l) => List(Jmp(l))
      case IL.IfGotoInstr(ce, l) => {
        val (c, cc) = genCExp(ce) 
        c++List(Jcc(cc,l))
      }
    }

  def genCode (c: IL.Code) : Code = c.flatMap(genInstr(_))

  def genDef (d: IL.Def) : Def = {
    val c1 =
      (d.args zip argRegs).map { case (x, r) => Movq(RegOpd(r), x) }
    val c2 = genCode(d.code)
    val c3 = List(Movq(v2operand(d.result), retReg))
    Def(d.name, d.args.length, c1++c2++c3)
  }
}
