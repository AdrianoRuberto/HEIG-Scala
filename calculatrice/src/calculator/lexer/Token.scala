package calculator.lexer

import calculator.lexer.Lexer.LexerOffset
import scala.language.implicitConversions

/** An input token */
trait Token

object Token {
	/**
	  * Unknown character
	  * The lexer will always succeed, unknown characters a wrapped as Unknown tokens.
	  */
	case class Unknown(value: Char) extends Token

	/** A number literal */
	case class Number(value: Double) extends Token

	/** An identifier, used as variable or function name */
	case class Identifier(value: String) extends Token

	/** An operator */
	case class Operator(value: String) extends Token

	/** A left parenthesis */
	case object LParen extends Token

	/** A right parenthesis */
	case object RParen extends Token

	/** A comma */
	case object Comma extends Token

	/** A colon */
	case object Colon extends Token

	/** The end of input token */
	case object End extends Token

	/**
	  * A SourceToken is a language token associated with its position in the source expression.
	  *
	  * @param token  the token
	  * @param offset its offset in the input string
	  */
	case class SourceToken(token: Token, offset: Int)

	object SourceToken {
		/** Provides implicit conversion from Token if an implicit LexerOffset is available */
		implicit def fromToken(token: Token)
		                      (implicit offset: LexerOffset): SourceToken = SourceToken(token, offset.value)
	}
}
