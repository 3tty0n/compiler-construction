package nonscala

import org.scalatest._
import Base._
import Abssyn._
import Oper._
import Ty._
import TypeCheck._

class TcheckTest extends FlatSpec {
  "変数" should "正しい型付け" in {
    assert(tCheck(Map(), Map("x" -> IntTy), VarExp("x")) === IntTy)
    assertThrows[TypeError] {
      tCheck(Map(), Map(), VarExp("x"))
    }
  }

  "isEmpty" should "正しい型付け" in {
    assert(tCheck(
      Map(),
      Map("x" -> IntListTy),
      UOpExp(IsEmptyOp, VarExp("x"))
    ) === BoolTy)
    assertThrows[TypeError] {
      tCheck(
        Map(),
        Map("x" -> IntTy),
        UOpExp(IsEmptyOp, VarExp("x"))
      )
    }
  }

  "+" should "正しい型付け" in {
    assert(tCheck(
      Map(),
      Map("x" -> IntTy),
      BOpExp(PlusOp, IntExp(1), VarExp("x"))
    ) === IntTy)
    intercept[TypeError] {
      tCheck(
        Map(),
        Map("x" -> BoolTy),
        BOpExp(PlusOp, IntExp(1), VarExp("x"))
      )
    }
  }

  "関数" should "正しい型付け" in {
    assert(tCheck(
      Map("f" -> FuncTy(List(IntTy, BoolTy), IntListTy)),
      Map("x" -> BoolTy),
      AppExp("f", List(IntExp(1), VarExp("x")))
    ) === IntListTy)
  }

  "if" should "正しい型付け" in {
    assert(tCheck(
      Map(),
      Map("x" -> IntTy),
      IfExp(BOpExp(EqOp, VarExp("x"), IntExp(2)), IntExp(1), IntExp(2))
    ) === IntTy)
  }

  "例: sort" should "正しい型付け" in {
    val ds = Main.parseFileDefs("typing-sbt/examples/sort.scala")
    tCheckDefs(ds)
    succeed
  }

  "例: insert" should "正しく型付けできる" in {
    val exp =
      """
        |def insert(x: Int, l: List[Int]): List[Int] =
        |  if (l.isEmpty)
        |    x::Nil
        |  else if (x<l.head)
        |    x::l
        |  else
        |    l.head::insert(x, l.tail)
      """.stripMargin
    val ds = Main.parseStrDefs(exp)
    tCheckDefs(ds)
    succeed
  }

}
