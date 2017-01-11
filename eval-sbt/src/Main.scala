package nonscala
import Base._
import Abssyn._
import Oper._
import Ty._


object Main {
  def main(args: Array[String]){
    if(args.size == 2) {
      val file = args(0)
      val method = args(1)
      val ds = Main.parseFileDefs(file)
      val fenv = Eval.defs2env(ds)
      val v =  Eval.eval(fenv, Map(), AppExp(method, Nil))
      printf("%s\n",v)
    } else {
      printf("Invalid args.\n")
    }
  }

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
