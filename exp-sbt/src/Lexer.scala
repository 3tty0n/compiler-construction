import Base._
import Tokens._

class Lexer (aSrc: String) extends Iterator[Token] {
  val src = aSrc.iterator
  var lastChar: Option[Char] = None

  def getChar(): Option[Char] = {
    lastChar match {
      case Some(c) => {
        lastChar = None
        Some(c)
      }
      case None =>
        if (src.hasNext) Some(src.next) else None
    }
  }

  def ungetChar(c: Char) = {
    lastChar = Some(c)
  }

  def lexMain(): Token =
    getChar() match {
      case None => EOF
      case Some(c) => {
        c match {
          case _ if c.isLetter => lexId(c.toString)
          case _ if c.isDigit => lexInt(c.toString)
          case '(' => LPAREN
          case ')' => RPAREN
          case '+' => PLUS
          case '-' => MINUS
          case '*' => TIMES
          case '/' => DIV
        }
      }
    }

  def lexId(ts: String): Token =
    getChar() match {
      case None => VAR(ts)
      case Some(c) =>
        if (c.isLetter) lexId (ts+c)
        else {
          ungetChar(c); VAR(ts)
        }
    }

  def lexInt(ts: String): Token =
    getChar() match {
      case None => INT(ts.toInt)
      case Some(c) =>
        if (c.isDigit) lexInt (ts+c)
        else {
          ungetChar(c)
          INT(ts.toInt)
        }
    }

  def next() = lexMain()
  var hasNext= true
}
