import org.scalatest._
import Abssyn._
import Oper._

class ParserTest extends FlatSpec {
  "*" should "左に強く結合" in
  {
    assert(Main.parseStr("abc * 10 * 20") ==
      BOpExp(TimesOp, BOpExp(TimesOp, VarExp("abc"), IntExp(10)), IntExp(20)))
  }

  "カッコ" should "正しく結合" in
  {
    assert(Main.parseStr("abc * (10 * 20)") ==
      BOpExp(TimesOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
  }

  "*と+" should "*の方が強く結合" in
  {
    assert(Main.parseStr("abc + 10 * 20") ==
      BOpExp(PlusOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
  }

  "カッコの中に*と+" should "" in
  {
    assert(Main.parseStr("(abc + 10 * 20)") ==
      BOpExp(PlusOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
  }


  "::" should "右に強く結合" in
  {
    assert(Main.parseStr("10 :: 20 :: Nil") ==
      BOpExp(ConsOp, IntExp(10), BOpExp(ConsOp, IntExp(20), NilExp)))
  }

  "::と*" should "*の方が強く結合" in
  {
    assert(Main.parseStr("5 :: 10 * 20 :: Nil") ==
      BOpExp(ConsOp, IntExp(5),
        BOpExp(ConsOp, BOpExp(TimesOp, IntExp(10), IntExp(20)), NilExp)))
  }

  "if文と==" should "" in
  {
    assert(Main.parseStr("if (10 == 20) x else y") ==
      IfExp(BOpExp(EqOp,IntExp(10), IntExp(20)), VarExp("x"), VarExp("y")))
  }

  "if文と<" should "" in
  {
    assert(Main.parseStr("if (10 < 20) x else y") ==
      IfExp(BOpExp(LtOp,IntExp(10), IntExp(20)), VarExp("x"), VarExp("y")))
  }

  "if文のネスト" should "" in
  {
    assert(Main.parseStr("if (10 == 20) x else if (x == y) 1 else 2") ==
    IfExp(BOpExp(EqOp,IntExp(10), IntExp(20)), VarExp("x"),
      IfExp(BOpExp(EqOp, VarExp("x"), VarExp("y")), IntExp(1), IntExp(2))))
  }


}
