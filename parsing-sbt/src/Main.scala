import Abssyn.{Def, Exp}

object Main {
  def parseStr(s: String): Exp = {
    val lexer = new Yylex(new java.io.StringReader(s))
    val parser = new Parser(lexer)
    parser.I()
  }

  def parseStrNonscala(s: String): Exp = {
    val lexer = new Yylex(new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.I()
  }

  def parseStrNonscalaDef(s: String): Def = {
    val lexer = new Yylex(new java.io.StringReader(s))
    val parser = new ParserNonscala(lexer)
    parser.D()
  }
}
