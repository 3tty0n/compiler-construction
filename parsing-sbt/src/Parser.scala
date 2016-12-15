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
    if (tok == t) advance() else error()

  def F(): Exp =
    tok match {
      case NIL => advance(); NilExp
      case INT(i) => advance(); IntExp(i)
      case ID(s) => advance(); VarExp(s)
      case LPAREN =>
        eat(LPAREN)
        val e = E()
        eat(RPAREN)
        e
      case _ => error()
    }

  def T(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => TPrime(F())
      case _ => error()
    }

  def TPrime(e: Exp): Exp =
    tok match {
      case TIMES => eat(TIMES); TPrime(BOpExp(TimesOp, e, F()))
      case DIV => eat(DIV); TPrime(BOpExp(DivideOp, e, F()))
      case PLUS | MINUS | RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON => e
      case _ => error()
    }

  def E(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => EPrime(T())
      case _ => error()
    }

  def EPrime(e: Exp): Exp =
    tok match {
      case PLUS => eat(PLUS); EPrime(BOpExp(PlusOp, e, T()))
      case MINUS => eat(MINUS); EPrime(BOpExp(MinusOp, e, T()))
      case RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON => e
      case _ => error()
    }

  def C(): Exp =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN => CPrime(E())
      case _ => error()
    }

  def CPrime(e: Exp): Exp =
    tok match {
      case COLONCOLON => eat(COLONCOLON); BOpExp(ConsOp, e, C())
      case _ => e
    }

  def B(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => BPrime(E())
      case _ => error()
    }

  def BPrime(e: Exp): Exp =
    tok match {
      case EQEQ => eat(EQEQ); BOpExp(EqOp, e, E())
      case LESS => eat(LESS); BOpExp(LtOp, e, E())
      case RPAREN | EOF => e
      case _ => error()
    }

  def I(): Exp = tok match {
    case ID(_) | INT(_) | LPAREN | NIL => C()
    case IF =>
      eat(IF)
      eat(LPAREN)
      val b = B()
      eat(RPAREN)
      val i1 = I()
      eat(ELSE)
      val i2 = I()
      IfExp(b, i1, i2)
    case _ => error()
  }
}

