package nonscala

import org.scalatest._
import Base._
import Abssyn._
import Oper._
import ILExec._

import scala.util.control.NonFatal

class ILTest extends FlatSpec {

  def evalCV(fenv: FEnv, env: Env, cv: (IL.Code, IL.Val)): Value =
    evalVal(exec(fenv, env, cv._1), cv._2)

  "変数" should "正しい値" in {
    assert(evalCV(Map(), Map("x" -> IntValue(1)), IL.trans(VarExp("x"))) === IntValue(1))
  }

  "+" should "正しい値" in {
    assert(evalCV(Map(), Map("x" -> IntValue(1)),
      IL.trans(BOpExp(PlusOp, IntExp(1), VarExp("x")))) === IntValue(2))
  }

  "算術式" should "正しい値" in {
    assert(
      evalCV(Map(), Map("x" -> IntValue(1), "y" -> IntValue(2)), IL.trans(BOpExp(PlusOp, BOpExp(TimesOp, IntExp(1), VarExp("x")), BOpExp(TimesOp, VarExp("y"), VarExp("y")))))
      === IntValue(5))
  }

  "関数適用" should "正しい値" in {
    val (c, v) = IL.trans(BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    assert(
      evalCV(Map("f" -> FValue(List("x", "y"), c, v)),
        Map("x" -> ListValue(List(2))),
        IL.trans(AppExp("f", List(IntExp(1), VarExp("x"))))) === ListValue(List(1, 2)))
  }

  "例: arith (x-y) * z" should "正しい値" in {
    val ds = try {
      Test.transFileDefs("examples/arith.scala")
    } catch {
      case NonFatal(_) => Test.transFileDefs("codegen/examples/arith.scala")
    }
      val fenv = defs2env(ds)
      assert(
        evalCV(fenv, Map(), IL.trans(AppExp("test", List(IntExp(4), IntExp(2), IntExp(3))))) === IntValue(6))
  }

  "例: fact" should "正しい値" in {
    val ds = try {
      Test.transFileDefs("examples/fact.scala")
    } catch {
      case NonFatal(_) => Test.transFileDefs("codegen/examples/fact.scala")
    }

    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("fact", List(IntExp(4))))) === IntValue(24))
  }


  "例: sort" should "正しい値" in {
    val ds = try {
      Test.transFileDefs("examples/sort.scala")
    } catch {
      case NonFatal(_) => Test.transFileDefs("codegen/examples/sort.scala")
    }
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map("x" -> ListValue(List(3, 2, 1))), IL.trans(AppExp("sort", List(VarExp("x"))))) === ListValue(List(1, 2, 3)))

  }

  "例: qsort" should "正しい値" in {
    val ds = try {
      Test.transFileDefs("examples/qsort.scala")
    } catch {
      case NonFatal(_) => Test.transFileDefs("codegen/examples/qsort.scala")
    }
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map("x" -> ListValue(List(3, 2, 1, 3, 5, 3, 1, 4))), IL.trans(AppExp("qsort", List(VarExp("x"))))) === ListValue(List(1, 1, 2, 3, 3, 3, 4, 5)))

  }

  "例: primes" should "正しい値" in  {
    val ds = try {
      Test.transFileDefs("examples/primes.scala")
    } catch {
      case NonFatal(_) => Test.transFileDefs("codegen/examples/primes.scala")
    }
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("primes", List(IntExp(20))))) ===
        ListValue(List(2, 3, 5, 7, 11, 13, 17, 19)))

  }


}
