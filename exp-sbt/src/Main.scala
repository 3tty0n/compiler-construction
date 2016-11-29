import Abssyn.Exp

object Main {
  def parseStr(s: String): Exp = {
    val l = new Lexer(s)
    val p = new Parser(l)
    p.E()
  }
  def evalStr(env:Map[String,Int], s: String): Int = {
    val e = parseStr(s)
    Eval.eval(env, e)
  }

}
