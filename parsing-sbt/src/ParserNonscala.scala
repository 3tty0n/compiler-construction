import Base._
import Oper._
import Ty._
import Abssyn._
import Tokens._

class ParserNonscala (val src: Yylex) {

  var tok: Token = src.yylex()

  def advance(): Unit =  tok = src.yylex()

  def eat(t: Token): Unit  =
    if (tok == t) advance() else error()


  def F(): Exp =    // 拡張が必要
    tok match {
      case NIL =>
        advance(); NilExp
      case INT(i) =>
        advance(); IntExp(i)
      case ID(s) =>
        advance(); VarExp(s)
      case LPAREN =>
        eat(LPAREN)
        val e = E()
        eat(RPAREN)
        e
      case _ =>
        error(
          message = s"expected NIL, num, (E), id, but actual $tok"
        )
    }

  def T(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL =>
        TPrime(F())
      case _ =>
        error(
          message = s"expected id, int, (, Nil, but actual $tok"
        )
    }

  def TPrime(e: Exp): Exp =
    tok match {
      case TIMES =>
        eat(TIMES); TPrime(BOpExp(TimesOp, e, F()))
      case DIV =>
        eat(DIV); TPrime(BOpExp(DivideOp, e, F()))
      case PLUS | MINUS| RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON =>
        e
      case _ =>
        error(
          message = s"expected +, -, (, EOF, else, ==, <, ::, but actual $tok"
        )
    }

  def U(): Ty =
    tok match {
      case ID("Int") =>
        IntTy
      case ID("Boolean") =>
        BoolTy
      case ID("List[Int]") =>
        IntListTy
      case _ =>
        error(message = "expected Int, Boolean, List[Int]")
    }

  def E(): Exp = T()  // プログラムを書く．補助関数も必要

  def C(): Exp = E()  // プログラムを書く．補助関数も必要

  def B(): Exp = E()  // プログラムを書く．補助関数も必要

  def I(): Exp = C()  // プログラムを書く．

  def D(): Def = Base.error() // プログラムを書く．

  def Ds(): List[Def] = {
    tok match {
      case DEF =>
        val d = D()
        val ds = Ds()
        d :: ds
      case EOF =>
        Nil
    }
  }
}

