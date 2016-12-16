import Base._
import Oper._
import Abssyn._
import Tokens._

class Parser(val src: Yylex) {

  var tok: Token = src.yylex()

  def advance(): Unit = {
    val lex = src.yylex()
    tok = lex
  }

  def eat(t: Token): Unit =
    if (tok == t)
      advance()
    else
      throw new UnboundValidException(
        message = s"eating error... token is $t"
      )

  def F(): Exp =
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
      case _ @ token =>
        throw new UnboundValidException(
          message = s"param: F\nexpected: Nil, num, id, (E)\nactual: $token"
        )
    }

  def T(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => TPrime(F())
      case _ @ token =>
        throw new UnboundValidException(
          message = s"param: T\nexpected: id, num, (, Nil\nactual: $token"
        )
    }

  def TPrime(e: Exp): Exp =
    tok match {
      case TIMES =>
        eat(TIMES); TPrime(BOpExp(TimesOp, e, F()))
      case DIV =>
        eat(DIV); TPrime(BOpExp(DivideOp, e, F()))
      case PLUS | MINUS | RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON =>
        e
      case _ @ token =>
        throw new UnboundValidException(
          message = s"param: T'\nexpected: *, / +, -, ), EOF, else, ==, <, ::\nactual: $token"
        )
    }

  def E(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL =>
        EPrime(T())
      case _ @ token =>
        throw new UnboundValidException(
          message = s"param: E\nexpected: id, num, (, Nil\nactual: $token"
        )
    }

  def EPrime(e: Exp): Exp =
    tok match {
      case PLUS =>
        eat(PLUS); EPrime(BOpExp(PlusOp, e, T()))
      case MINUS =>
        eat(MINUS); EPrime(BOpExp(MinusOp, e, T()))
      case RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON =>
        e
      case _  @ token =>
        throw new UnboundValidException(
          message = s"param: E'\nexpected: +, -, ), EOF, else, ==, <, ::\nactual: $token"
        )
    }

  def C(): Exp =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN =>
        CPrime(E())
      case _ @ token =>
        throw new UnboundValidException(
          message = s"param: C\nexpected: id, num, Nil, (\nactual: $token"
        )
    }

  def CPrime(e: Exp): Exp =
    tok match {
      case COLONCOLON =>
        eat(COLONCOLON); BOpExp(ConsOp, e, C())
      case _ =>
        e
    }

  def B(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => BPrime(E())
      case _ @ token =>
        throw new UnboundValidException(
          message = s"param: B\nexpected: id, num, (, Nil\nactual: $token"
        )
    }

  def BPrime(e: Exp): Exp =
    tok match {
      case EQEQ =>
        eat(EQEQ); BOpExp(EqOp, e, E())
      case LESS =>
        eat(LESS); BOpExp(LtOp, e, E())
      case RPAREN | EOF =>
        e
      case _ @ token =>
        throw new UnboundValidException(
          message = s"param: B'\nexpected: ==, <, ), EOF\nactual: $token"
        )
    }

  def I(): Exp = tok match {
    case ID(_) | INT(_) | LPAREN | NIL =>
      C()
    case IF =>
      eat(IF)
      eat(LPAREN)
      val b = B()
      eat(RPAREN)
      val i1 = I()
      eat(ELSE)
      val i2 = I()
      IfExp(b, i1, i2)
    case _ @ token =>
       throw new UnboundValidException(
         message = s"param: I\nexpected: if, id, num\nactual: $token"
       )
  }

  def skipTo(stop: Token): Unit = {
    if (tok == stop)
      ()
    else
      eat(tok)
      skipTo(stop)
  }
}

