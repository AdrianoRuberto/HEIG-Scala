package calculator.lexer
import calculator.lexer.Lexer.LexerOffset

trait Token

object Token {
	case class Unknown(value: Char) extends Token
	case class Number(value: Double) extends Token
	case class Identifier(value: String) extends Token
	case class Operator(value: String) extends Token
	case object LParen extends Token
	case object RParen extends Token
	case object Comma extends Token
	case object End extends Token

	case class SourceToken(token: Token, offset: Int)
	object SourceToken {
		implicit def fromToken(token: Token)
		                      (implicit offset: LexerOffset): SourceToken = SourceToken(token, offset.value)
	}

	def unapply(st: SourceToken): Option[Token] = Some(st.token)
}
