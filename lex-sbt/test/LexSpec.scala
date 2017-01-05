import java.io.StringReader

import Base.MyError
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import scala.sys.process._
import Tokens._

class LexSpec
  extends FlatSpec
    with Matchers
    with BeforeAndAfterAll
{

  "flex" should "正しく正規表現を定義できている" in {
    new Yylex(new StringReader("1")).yylex() should equal (NUM(1))
    new Yylex(new StringReader("if")).yylex() should equal (IF)
    new Yylex(new StringReader("123.14")).yylex() should equal (REAL(123.14))
  }

  it should "0から始まる数字は0以外 Exception を送出する" in {
    intercept[MyError] {
      new Yylex(new StringReader("01")).yylex()
    }
  }

  it should "予約語を `` で囲むと識別子として使用できる" in {
    new Yylex(new StringReader("`if`")).yylex() should equal (ID("if"))
  }

}
