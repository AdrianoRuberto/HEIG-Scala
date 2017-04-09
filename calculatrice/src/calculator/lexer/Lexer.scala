package calculator.lexer

import calculator.lexer.Token.SourceToken

object Lexer {
	case class LexerOffset(value: Int) extends AnyVal {
		def + (skip: Int): LexerOffset = LexerOffset(value + skip)
	}

	def tokenize(input: Stream[Char]): Stream[SourceToken] = readNext(input, LexerOffset(0))

	private def readNext(input: Stream[Char], offset: LexerOffset): Stream[SourceToken] = {
		implicit val currentOffset = offset
		input match {
			case c #:: cs if Character.isWhitespace(c) =>
				readNext(cs, offset + 1)
			case (op @ ('+' | '-' | '*' | '/' | '%' | '^' | '!' | '=')) #:: cs =>
				Token.Operator(op.toString) #:: readNext(cs, offset + 1)
			case (p @ ('(' | ')')) #:: cs =>
				(if (p == '(') Token.LParen else Token.RParen) #:: readNext(cs, offset + 1)
			case ',' #:: cs =>
				Token.Comma #:: readNext(cs, offset + 1)
			case ':' #:: cs =>
				Token.Colon #:: readNext(cs, offset + 1)
			case c #:: cs if Character.isDigit(c) =>
				readNumber(input)
			case c #:: cs if Character.isJavaIdentifierStart(c) =>
				val (rest, next) = cs.span(Character.isJavaIdentifierPart)
				val identifier = (c #:: rest).mkString
				Token.Identifier(identifier) #:: readNext(next, offset + identifier.length)
			case c #:: cs =>
				Token.Unknown(c) #:: readNext(cs, offset + 1)
			case Stream.Empty =>
				Token.End #:: Stream.empty[SourceToken]
		}
	}

	private val numberFormat = """-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+\-]?[0-9]+)?""".r

	private def isValidNumberCharacter(c: Char): Boolean = {
		c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' || Character.isDigit(c)
	}

	private def readNumber(in: Stream[Char])(implicit offset: LexerOffset): Stream[SourceToken] = {
		val candidate = in.takeWhile(isValidNumberCharacter)
		numberFormat.findPrefixOf(candidate.mkString) match {
			case Some(number) => Token.Number(number.toDouble) #:: readNext(in.drop(number.length), offset + number.length)
			case None => Token.Unknown(in.head) #:: readNext(in.tail, offset + 1)
		}
	}
}
