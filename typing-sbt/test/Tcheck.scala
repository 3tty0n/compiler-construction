package nonscala

import org.scalatest._
import Base._
import Abssyn._
import Oper._
import Ty._
import TypeCheck._

class TcheckTest extends FlatSpec {
  "変数" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy), VarExp("x"))== IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map(), VarExp("x"))
    }
  }

  "isEmpty" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntListTy),
      UOpExp(IsEmptyOp, VarExp("x")))== BoolTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntTy),
        UOpExp(IsEmptyOp, VarExp("x")))
    }
  }

  "+" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(PlusOp, IntExp(1), VarExp("x")))== IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(PlusOp, IntExp(1), VarExp("x")))
    }
  }

  "関数" should "正しい型付け" in
  {
    assert(
      tcheck(Map("f"->FuncTy(List(IntTy,BoolTy),IntListTy)),
        Map("x"->BoolTy),
        AppExp("f", List(IntExp(1), VarExp("x"))))== IntListTy)
  }

  "例: sort" should "正しい型付け" in
  {
    val ds = Main.parseFileDefs("examples/sort.scala")
    tcheckDefs(ds)
    succeed
  }

}
