package nonscala

object Asm {
  import Base._

  // レジスタ 
  type Reg = Var

  // ソースオペランドを表現する型
  trait Operand {
    // 文字列に変換するメソッドを提供
    override def toString(): String
  }

  case class RegOpd(r: Reg) extends Operand {
    override def toString() = r
  }
  case class IntOpd(i: Int) extends Operand {
    override def toString() = "$"+i.toString
  }

  // movq 命令のソースまたはデスティネーションがメモリの場合のオペランド
  trait Mem {
    override def toString(): String
  }

  case class MemReg(r: Reg, offset: Int) extends Mem {
    override def toString() = s"${offset}(${r})"
  }

  // スピルした変数（フレームに割り当てる前）
  // 課題6bでは使用しない
  case class MemTmp(s: String) extends Mem {
    override def toString() = "L_"+s 
  }
  // スピルした変数（フレームに割り当てた後）
  // 課題6bでは使用しない
  case class MemBp(i: Int) extends Mem {
    override def toString() =s"${(i+1) * -8}(%rbp)"
  }

  // 関数引数に使われるレジスタ，第１引数から並べた
  val argRegs =
    List("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9")

  val calleeSaveRegs =
    List("%rbx", "%r10", "%r13", "%r14", "%r15")

  val retReg = "%rax"

  val allRegs = retReg::argRegs:::calleeSaveRegs

  val numRegs = allRegs.length

  type Label = String

  trait Instr 

  case class Movq (s: Operand, d: Reg) extends Instr
  // movq 命令で ソースがメモリの場合は，Loadq型で表現
  case class Loadq (s: Mem, d: Reg) extends Instr
  // movq 命令で デスティーネーションがメモリの場合は，Storeq型で表現
  case class Storeq (s: Reg, d: Mem) extends Instr 
  case class Addq(s: Operand,  d: Reg) extends Instr 
  case class Subq(s: Operand,  d: Reg) extends Instr 
  case class Imulq(s: Operand, d: Reg) extends Instr
  case class Cmpq(s1: Operand, s2: Reg) extends Instr
  case object Cqto extends Instr
  case class Idivq (s: Reg) extends Instr
  // Callq(s: 関数名, num: 引数の数) 
  case class Callq(s: String, num: Int) extends Instr
  case class LabelInstr(l: Label) extends Instr 
  case class Jmp(l: Label) extends Instr

  trait CC
  case object EqCC extends CC
  case object LtCC extends CC

  case class Jcc(cc: CC, l: Label) extends Instr

  // 課題6bでは使用ない命令
  case class Pushq (s: Reg) extends Instr 
  case class Popq (d: Reg) extends Instr
  case object Retq extends Instr   
  case class Incq (d: Reg) extends Instr 
  case class Decq (d: Reg) extends Instr



  type Code = List[Instr]

  case class Def(name: String, numArgs: Int, body: Code)
}

