
trait RegExp
case class CharExp(c: Char)
case object EmptyExp        // 空集合
case object EpsExp          // ε
case class ConcatExp(r1: RegExp, r2: RegExp)  // 連接
case class AltExp(r1: RegExp, r2: RegExp)     // 選択
case class StarExp(r: RegExp)                  // 繰り返し

