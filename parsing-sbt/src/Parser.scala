import Base._
import Oper._
import Abssyn._
import Tokens._

class Parser(val src: Yylex) {

  var tok: Token = src.yylex()

  def advance(): Unit = tok = src.yylex()

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
      case COLONCOLON => eat(COLONCOLON); CPrime(BOpExp(ConsOp, e, C()))
      case EOF => e
      case _ => error()
    }

  def B(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => BPrime(E())
      case _ => error()
    }

  def BPrime(e: Exp): Exp = tok match {
    case EQEQ => eat(EQEQ); E()
    case LESS => eat(LESS); E()
    case LPAREN | EOF => e
    case _ => error()
  }

  def I(): Exp = tok match {
    case ID(_) | INT(_) | LPAREN | NIL => C()
    case IF => eat(IF); eat(LPAREN); B(); eat(RPAREN); I(); eat(ELSE); I()
    case _ => error()
  }
}

