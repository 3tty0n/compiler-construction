object Tokens {
  trait Token
  case class VAR(s: String) extends Token
  case class INT(i: Int) extends Token
  case object LPAREN extends Token
  case object RPAREN extends Token
  case object PLUS extends Token
  case object MINUS extends Token
  case object TIMES extends Token
  case object DIV extends Token
  case object EOF extends Token
}

