import org.scalatest._
import Oper._
import Ty._
import Abssyn._

class ParserNonscalaTest extends FlatSpec {
  def parseStr(s: String) = {
    val lexer = new Yylex (new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.I()
  }

  def parseStrDef(s: String) = {
    val lexer = new Yylex (new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.D()
  }

  def parseFileDef(s: String) = {
    val f = new java.io.File(s)
    val lexer = new Yylex (new java.io.FileReader(f))
    val parser = new ParserNonscala(lexer)
    parser.D()
  }




  "*" should "左に強く結合" in
  {
    assert(parseStr("abc * 10 * 20") ==
      BOpExp(TimesOp, BOpExp(TimesOp, VarExp("abc"), IntExp(10)), IntExp(20)))
  }

  "カッコ" should "正しく結合" in
  {
    assert(parseStr("abc * (10 * 20)") ==
      BOpExp(TimesOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
  }

  "*と+" should "*の方が強く結合" in
  {
    assert(parseStr("abc + 10 * 20") ==
      BOpExp(PlusOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
  }

  "カッコの中に*と+" should "" in
  {
    assert(parseStr("(abc + 10 * 20)") ==
      BOpExp(PlusOp, VarExp("abc"), BOpExp(TimesOp, IntExp(10), IntExp(20))))
  }

  "::" should "右に強く結合" in
  {
    assert(parseStr("10 :: 20 :: Nil") ==
      BOpExp(ConsOp, IntExp(10), BOpExp(ConsOp, IntExp(20), NilExp)))
  }

  "::と*" should "*の方が強く結合" in
  {
    assert(parseStr("5 :: 10 * 20 :: Nil") ==
      BOpExp(ConsOp, IntExp(5),
        BOpExp(ConsOp, BOpExp(TimesOp, IntExp(10), IntExp(20)), NilExp)))
  }

  "if文と==" should "" in
  {
    assert(parseStr("if (10 == 20) x else y") ==
      IfExp(BOpExp(EqOp,IntExp(10), IntExp(20)), VarExp("x"), VarExp("y")))
  }

  "if文と<" should "" in
  {
    assert(parseStr("if (10 < 20) x else y") ==
      IfExp(BOpExp(LtOp,IntExp(10), IntExp(20)), VarExp("x"), VarExp("y")))
  }

  "if文のネスト" should "" in
  {
    assert(parseStr("if (10 == 20) x else if (x == y) 1 else 2") ==
    IfExp(BOpExp(EqOp,IntExp(10), IntExp(20)), VarExp("x"),
      IfExp(BOpExp(EqOp, VarExp("x"), VarExp("y")), IntExp(1), IntExp(2))))
  }

  "関数適用" should "" in
  {
    assert(parseStr("f(10,20)") ==  AppExp("f", List(IntExp(10), IntExp(20))))
  }

  "isEmpty" should "" in
  {
    assert(parseStr("x.isEmpty") ==  UOpExp(IsEmptyOp, VarExp("x")))
  }

  "head" should "" in
  {
    assert(parseStr("x.head") ==  UOpExp(HeadOp, VarExp("x")))
  }

  "tail" should "" in
  {
    assert(parseStr("x.tail") ==  UOpExp(TailOp, VarExp("x")))
  }


  "関数定義" should "" in
  {
    assert(parseStrDef("def f(x:Int, y:Boolean): List[Int] = Nil") ==
      Def ("f", List(("x", IntTy), ("y", BoolTy)), IntListTy, NilExp))
  }

  "例: insert" should "" in
  {
    assert(parseFileDef("examples/insert.scala") ==
      Def("insert",List(("x",IntTy), ("l",IntListTy)),IntListTy,
        IfExp(UOpExp(IsEmptyOp,VarExp("l")),
          BOpExp(ConsOp,VarExp("x"),NilExp),
          IfExp(BOpExp(LtOp,VarExp("x"),UOpExp(HeadOp,VarExp("l"))),
            BOpExp(ConsOp,VarExp("x"),VarExp("l")),
            BOpExp(ConsOp,UOpExp(HeadOp,VarExp("l")),
              AppExp("insert",List(VarExp("x"), UOpExp(TailOp,VarExp("l")))))))))
  }

}
