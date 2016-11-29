import Base._
import Abssyn._
import Tokens._

class Parser (aSrc: Iterator[Token]) {

  val src = aSrc
  var tok: Token = src.next()

  def advance () =  tok = src.next() 

  def eat (t: Token) = 
    if (tok == t) advance() else error()

  def F(): Exp =
    tok match {
      case INT(i) => advance(); IntExp(i)
      case VAR(s) =>  advance(); VarExp(s)
      case LPAREN =>
        {
          eat(LPAREN)
          val e = T()
          eat(RPAREN)
          e
        }
      case _ => error()
    }

  def T(): Exp =
    tok match {
      case VAR(_) | INT(_) | LPAREN =>  Tprime(F())
      case _ => error()
    }

  def Tprime(e: Exp): Exp =
    tok match {
      case TIMES => eat(TIMES); Tprime(TimesExp(e, F()))
      case DIV => eat(DIV); Tprime(DevideExp(e, F()))
      case PLUS | MINUS| RPAREN | EOF => e
      case _ => error()
    }

  def E(): Exp =  
    tok match {
      case VAR(_) | INT(_) | LPAREN =>  Eprime(T())
      case _ => error()
    }

  def Eprime(e: Exp): Exp =
    tok match {
      case PLUS => eat(PLUS); Eprime(PlusExp(e, T()))
      case MINUS => eat(MINUS); Eprime(MinusExp(e, T()))
      case RPAREN | EOF => e
      case _ => error()
    }
}


