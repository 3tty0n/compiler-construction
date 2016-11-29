import org.scalatest.{ FlatSpec, Matchers }
import Abssyn._
import Eval._
import Main.evalStr

class EvalSpec extends FlatSpec with Matchers {

  "evalStr" should "変数の値が見つからなかったときに0を返す" in {
    assert(evalStr(Map("x"->1, "y"->2), "x-y") === -1)
    assert(evalStr(Map("x"->1),	"x+y") === 1)
  }

}