package nonscala

object Test {
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

  def transFileDefs(s: String) = {
    val ds = parseFileDefs(s)
    ds.map(IL.transDef(_))
  }

  def codegenFileDefs(s: String) = {
    val ds = transFileDefs(s)
    ds.map(Codegen.genDef(_))
  }

}
