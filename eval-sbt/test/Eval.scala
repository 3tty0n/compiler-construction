package nonscala

import org.scalatest._
import Base._
import Abssyn._
import Oper._
import Eval._

class EvalTest extends FlatSpec {
  "変数" should "正しい値" in {
    assert(eval(Map(), Map("x" -> IntVal(1)), VarExp("x")) == IntVal(1))
  }

  "+" should "正しい値" in {
    assert(eval(Map(), Map("x" -> IntVal(1)),
      BOpExp(PlusOp, IntExp(1), VarExp("x"))) == IntVal(2))
  }


  "isEmpty" should "正しい値" in {
    assert(eval(Map(), Map("x" -> ListVal(Nil)),
      UOpExp(IsEmptyOp, VarExp("x"))) == BoolVal(true))
    assert(eval(Map(), Map("x" -> ListVal(List(1, 2))),
      UOpExp(IsEmptyOp, VarExp("x"))) == BoolVal(false))

  }


  "関数適用" should "正しい値" in {
    assert(
      eval(
        Map("f" -> FValue(List("x", "y"), BOpExp(ConsOp, VarExp("x"), VarExp("y")))),
        Map("x" -> ListVal(List(2))),
        AppExp("f", List(IntExp(1), VarExp("x")))
      ) === ListVal(List(1, 2))
    )
  }

  it should "正しい値を返す" in {
    assert(
      eval(
        Map("f" -> FValue(List("x"), VarExp("x"))),
        Map(),
        AppExp("f", List(IntExp(1)))
      ) === IntVal(1)
    )
  }

  "例: sort" should "正しい値" in {
    val ds = Main.parseFileDefs("eval-sbt/examples/sort.scala")
    val fenv = defs2env(ds)
    assert(
      eval(
        fenv,
        Map("x" -> ListVal(List(3, 2, 1))),
        AppExp("sort", List(VarExp("x")))
      ) === ListVal(List(1, 2, 3))
    )
  }

  "例: qsort" should "正しい値" in {
    val ds = Main.parseFileDefs("eval-sbt/examples/qsort.scala")
    val fenv = defs2env(ds)
    assert(
      eval(
        fenv,
        Map("x" -> ListVal(List(3, 2, 5, 1, 4))),
        AppExp("qsort", List(VarExp("x")))
      ) === ListVal(List(1, 2, 3, 4, 5))
    )
  }

  "例: primes" should "正しい値" in {
    val ds = Main.parseFileDefs("eval-sbt/examples/primes.scala")
    val fenv = defs2env(ds)
    assert(
      eval(
        fenv,
        Map(),
        AppExp("primes", List(IntExp(20)))
      ) === ListVal(List(2, 3, 5, 7, 11, 13, 17, 19))
    )
  }


}
