package nonscala
import Base._
import Abssyn._
import Oper._
import Ty._


object Main {
  def parseStrDefs(s: String) = {
    val lexer = new Yylex (new java.io.StringReader(s))
    val parser = new Parser(lexer)
    parser.Ds()
  }

  def parseFileDefs(s: String) = {
    val f = new java.io.File(s)
    val lexer = new Yylex (new java.io.FileReader(f))
    val parser = new Parser(lexer)
    parser.Ds()
  }
}
