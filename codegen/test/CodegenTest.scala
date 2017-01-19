package nonscala

import org.scalatest.FlatSpec
import Base._
import IL._
import Oper._
import AsmExec._

import scala.util.{Failure, Success, Try}

class CodegenTest extends FlatSpec {

  def exec(fenv: FEnv, env: Env, c: Asm.Code): (Env, Mem) = {
    val (env1, mem1, alloc1) = execCode(fenv, env, Map(), 16000, c)
    (env1, mem1)
  }

  "変数" should "正しい値" in {
    assert(exec(Map(), Map("x" -> 1),
      Codegen.genInstr(AssignInstr("y", ValExp(VarVal("x")))))._1("y") === 1)
  }

  "+" should "正しい値" in {
    assert(exec(Map(), Map("x" -> 1),
      Codegen.genInstr(AssignInstr("y", BOpExp(PlusOp, VarVal("x"), IntVal(2)))))._1("y") === 3
    )
  }

  "-" should "正しい値" in {
    assert(exec(Map(), Map("x" -> 1),
      Codegen.genInstr(AssignInstr("y", BOpExp(MinusOp, VarVal("x"), IntVal(2)))))._1("y") === -1)
  }

  "*" should "正しい値" in {
    assert(exec(Map(), Map("x" -> 2),
      Codegen.genInstr(AssignInstr("y", BOpExp(TimesOp, VarVal("x"), IntVal(2)))))._1("y") === 4)
  }

  "/" should "正しい値" in {
    assert(exec(Map(), Map("x" -> 5),
      Codegen.genInstr(AssignInstr("y", BOpExp(DivideOp, VarVal("x"), IntVal(2)))))._1("y") === 5 / 2)
  }

  "例: arith (x-y) * z" should "正しい値" in {
    val ds = Try(Test.codegenFileDefs("examples/arith.scala")) match {
      case Success(d) => d
      case Failure(_) => Test.codegenFileDefs("codegen/examples/arith.scala")
    }
    val fenv = AsmExec.defs2env(ds)
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs.head -> 4, Asm.argRegs(1) -> 2, Asm.argRegs(2) -> 3), Map(), 16000, List(Asm.Callq("test", 3)))
    assert(env2(Asm.retReg) === 6)
  }


  "例: fact" should "正しい値" in {
    val ds = Try(Test.codegenFileDefs("examples/fact.scala")) match {
      case Success(_def) => _def
      case Failure(_) => Test.codegenFileDefs("codegen/examples/fact.scala")
    }
    val fenv = AsmExec.defs2env(ds)
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs(0) -> 4), Map(), 16000, List(Asm.Callq("fact", 1)))
    assert(env2(Asm.retReg) === 24)
  }


  def memOf(l: List[Int]): (Mem, Int, Int) = {
    val a0 = 16000

    def loop(l: List[Int]): (Mem, Int, Int) =
      l match {
        case Nil => (Map(), a0, 0)
        case i :: l => {
          val (mem, alloc, a) = loop(l)
          val mem1 = mem + (alloc -> i)
          val mem2 = mem1 + (alloc + 8 -> a)
          (mem2, alloc + 16, alloc)
        }
      }

    val (mem, alloc, a) = loop(l)
    (mem, a, alloc)
  }

  "例: sort" should "正しい値" in {
    val ds = Try(Test.codegenFileDefs("examples/sort.scala")) match {
      case Success(_def) => _def
      case Failure(_) => Test.codegenFileDefs("codegen/examples/sort.scala")
    }
    val fenv = AsmExec.defs2env(ds)
    val (mem, a0, a1) = memOf(List(3, 2, 1))
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs.head -> a0), mem, a1, List(Asm.Callq("sort", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) === List(1, 2, 3))
  }

  "例: qsort" should "正しい値" in  {
    val ds = Try(Test.codegenFileDefs("examples/qsort.scala")) match {
      case Success(_def) => _def
      case Failure(_) => Test.codegenFileDefs("codegen/examples/qsort.scala")
    }
    val fenv = AsmExec.defs2env(ds)
    val (mem, a0, a1) = memOf(List(3, 2, 1, 3, 5, 3, 1, 4))
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs.head -> a0), mem, a1, List(Asm.Callq("qsort", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) === List(1, 1, 2, 3, 3, 3, 4, 5))
  }

  "例: primes" should "正しい値" in {
    val ds = Try(Test.codegenFileDefs("examples/primes.scala")) match {
      case Success(_def) => _def
      case Failure(_) => Test.codegenFileDefs("codegen/examples/primes.scala")
    }
    val fenv = AsmExec.defs2env(ds)
    val (env2, mem2, _) =
      execCode(fenv, Map(Asm.argRegs.head -> 20), Map(), 16000, List(Asm.Callq("primes", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) === List(2, 3, 5, 7, 11, 13, 17, 19))
  }
}
