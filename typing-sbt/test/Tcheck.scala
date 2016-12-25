package nonscala

import org.scalatest._
import Base._
import Abssyn._
import Oper._
import Ty._
import TypeCheck._
import nonscala.Tokens.EQEQ

class TcheckTest extends FlatSpec {
  "変数" should "正しい型付け" in {
    assert(tCheck(
      Map(),
      Map("x" -> IntTy),
      VarExp("x")
    ) === IntTy)
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

  "+, -, *, /" should "正しい型付け" in {
    assert(tCheck(
      Map(),
      Map("x" -> IntTy),
      BOpExp(DivideOp, IntExp(1), VarExp("x"))
    ) === IntTy)
    assert(tCheck(
      Map(),
      Map("x" -> IntTy, "y" -> IntTy),
      BOpExp(PlusOp, BOpExp(MinusOp, IntExp(1), VarExp("x")), BOpExp(TimesOp, VarExp("y"), IntExp(1)))
    ) === IntTy)
    assertThrows[TypeError] {
      tCheck(
        Map(),
        Map("x" -> BoolTy),
        BOpExp(PlusOp, IntExp(1), VarExp("x"))
      )
    }
  }

  "::" should "正しい型付け" in {
    assert(tCheck(
      Map(),
      Map("x" -> IntTy, "y" -> IntListTy),
      BOpExp(ConsOp, IntExp(1), BOpExp(ConsOp, IntExp(2), NilExp))
    ) === IntListTy)
  }

  "<, ==" should "正しい型付け" in {
    assert(tCheck(
      Map(),
      Map("x" -> IntTy),
      BOpExp(EqOp, VarExp("x"), IntExp(1))
    ) === BoolTy)
    assertThrows[TypeError] {
      tCheck(
        Map(),
        Map(),
        BOpExp(EqOp, BOpExp(ConsOp, IntExp(1), NilExp), BOpExp(PlusOp, IntExp(2), IntExp(1)))
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
    assert(tCheckDefs(ds) === IntListTy :: IntListTy :: IntListTy :: Nil)
  }

  "例: insert" should "正しく型付け" in {
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
    assert(tCheckDefs(ds) === IntListTy :: Nil)
  }

  "例: f" should "正しく型付け" in {
    val exp =
      """
        |def reverse(l: List[Int], m: List[Int]): List[Int] =
        |  if (l == Nil) m
        |  else reverse(l.tail, l.head :: m)
        |
      """.stripMargin
    assert(tCheckDefs(Main.parseStrDefs(exp)) === IntListTy :: Nil)
  }

}
