import Base._
import Oper._
import Ty._
import Abssyn._
import Tokens._

class ParserNonscala(val src: Yylex) {

  var tok: Token = src.yylex()

  def advance(): Unit = tok = src.yylex()

  def eat(t: Token): Unit =
    if (tok == t) advance() else throw new UnboundValidException(
      message = s"eating error expected $t, but actual $tok"
    )


  def F(): Exp = // 拡張が必要
    tok match {
      case NIL =>
        advance(); NilExp
      case INT(i) =>
        advance(); IntExp(i)
      case ID(s) =>
        advance(); FPrime(VarExp(s))
      case LPAREN =>
        eat(LPAREN)
        val e = E()
        eat(RPAREN)
        e
      case _ =>
        error(
          message = s"expected NIL, num, (E), id, but actual $tok"
        )
    }

  def FPrime(e: Exp): Exp =
    tok match {
      case DOT =>
        eat(DOT); FDoublePrime(e)
      case LPAREN =>
        val name = e match {
          case VarExp(s) => s
          case _ => throw new UnboundValidException(s"expected VarExp, but actual is $e")
        }
        eat(LPAREN)
        val listExp = FPrimeAux(e)
        eat(RPAREN)
        AppExp(name, listExp)
      case PLUS | MINUS | TIMES | DIV | RPAREN | ELSE | EQEQ | EOF | COLONCOLON | LESS | DEF => e
      case _ => throw new UnboundValidException(s"expected . ( + - * / ) else end-of-file, but $tok")
    }

  def FPrimeAux(e: Exp): List[Exp] = {
    tok match {
      case RPAREN => Nil
      case CAMMA =>
        eat(CAMMA)
        val ee = E()
        ee :: FPrimeAux(e)
      case INT(i) => advance(); IntExp(i) :: FPrimeAux(e)
      case ID(s) => advance(); VarExp(s) :: FPrimeAux(e)
      case _ => throw new UnboundValidException(s"expected ), , but $tok")
    }
  }

  def FDoublePrime(e: Exp): Exp =
    tok match {
      case ID(s) if s == "isEmpty" => advance(); UOpExp(IsEmptyOp, e)
      case ID(s) if s == "head" => advance(); UOpExp(HeadOp, e)
      case ID(s) if s == "tail" => advance(); UOpExp(TailOp, e)
      case _ => throw new UnboundValidException("expected isEmpty, head, tail")
    }

  def T(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL | EOF =>
        TPrime(F())
      case _ =>
        throw new UnboundValidException(
          message = s"expected id, int, (, Nil, but actual $tok"
        )
    }

  def TPrime(e: Exp): Exp =
    tok match {
      case TIMES =>
        eat(TIMES); TPrime(BOpExp(TimesOp, e, F()))
      case DIV =>
        eat(DIV); TPrime(BOpExp(DivideOp, e, F()))
      case PLUS | MINUS | RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON | CAMMA | DEF =>
        e
      case _ =>
        throw new UnboundValidException(
          message = s"expected +, -, (, EOF, else, ==, <, ::, but actual $tok"
        )
    }


  def E(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL | EOF =>
        EPrime(T())
      case IF =>
        I()
      case _@token =>
        throw new UnboundValidException(
          message = s"param: E\nexpected: id, num, (, Nil\nactual: $token"
        )
    }

  def EPrime(e: Exp): Exp =
    tok match {
      case PLUS =>
        eat(PLUS); EPrime(BOpExp(PlusOp, e, T()))
      case MINUS =>
        eat(MINUS); EPrime(BOpExp(MinusOp, e, T()))
      case RPAREN | EOF | ELSE | EQEQ | LESS | COLONCOLON | CAMMA | DEF =>
        e
      case _@token =>
        throw new UnboundValidException(
          message = s"param: E'\nexpected: +, -, ), EOF, else, ==, <, ::\nactual: $token"
        )
    }


  def C(): Exp =
    tok match {
      case ID(_) | INT(_) | NIL | LPAREN =>
        CPrime(E())
      case _@token =>
        throw new UnboundValidException(
          message = s"param: C\nexpected: id, num, Nil, (\nactual: $token"
        )
    }

  def CPrime(e: Exp): Exp =
    tok match {
      case COLONCOLON =>
        eat(COLONCOLON); BOpExp(ConsOp, e, C())
      case _ =>
        e
    }

  def I(): Exp = tok match {
    case ID(_) | INT(_) | LPAREN | NIL =>
      C()
    case IF =>
      eat(IF)
      eat(LPAREN)
      val b = B()
      eat(RPAREN)
      val i1 = I()
      eat(ELSE)
      val i2 = I()
      IfExp(b, i1, i2)
    case _@token =>
      throw new UnboundValidException(
        message = s"param: I\nexpected: if, id, num\nactual: $token"
      )
  }

  def B(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL => BPrime(E())
      case _@token =>
        throw new UnboundValidException(
          message = s"param: B\nexpected: id, num, (, Nil\nactual: $token"
        )
    }

  def BPrime(e: Exp): Exp =
    tok match {
      case EQEQ =>
        eat(EQEQ); BOpExp(EqOp, e, E())
      case LESS =>
        eat(LESS); BOpExp(LtOp, e, E())
      case RPAREN | EOF =>
        e
      case _@token =>
        throw new UnboundValidException(
          message = s"param: B'\nexpected: ==, <, ), EOF\nactual: $token"
        )
    }

  def D(): Def =
    tok match {
      case DEF =>
        eat(DEF)
        val functionName = tok match {
          case ID(s) => s
          case _ => throw new UnboundValidException(s"expected ID, but actual $tok")
        }
        advance(); eat(LPAREN)
        val arguments = defArgs()
        eat(RPAREN); eat(COLON)
        val rtype = U()
        advance()
        val body = E()
        Def(functionName, arguments, rtype, body)
      case _ => throw new UnboundValidException(s"expected DEF, but actual $tok")
    }

  def defArgs(): List[(Var, Ty)] =
    tok match {
      case ID(s) =>
        advance(); eat(COLON)
        val rtype = U()
        if (tok != RPAREN) eat(CAMMA)
        (s, rtype) :: defArgs()
      case RPAREN => Nil
      case _ => throw new UnboundValidException(s"expected ID, ), but actual $tok")
    }

  def U(): Ty =
    tok match {
      case ID("Int") =>
        advance(); IntTy
      case ID("Boolean") =>
        advance(); BoolTy
      case ID("List") =>
        eat(ID("List"))
        eat(LBRACKET)
        eat(ID("Int"))
        eat(RBRACKET)
        IntListTy
      case _ =>
        throw new UnboundValidException(message = s"expected Int, Boolean, List[Int], but actual$tok")
    }

  def Ds(): List[Def] = {
    tok match {
      case DEF =>
        val d = D()
        val ds = Ds()
        d :: ds
      case EOF =>
        Nil
    }
  }
}

