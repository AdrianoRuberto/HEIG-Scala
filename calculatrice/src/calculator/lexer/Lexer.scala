package calculator.lexer

import calculator.lexer.Token.SourceToken

/**
  * The input lexer
  *
  * The current implementation is based on Streams and provide a lazy behavior
  * for the lexer. Input is only consumed and tokenized as the output tokens
  * are being consumed by the parser.
  */
object Lexer {
	/** The current offset in the input string */
	case class LexerOffset(value: Int) extends AnyVal {
		/** Move the offset by the given amount of characters */
		def +(skip: Int): LexerOffset = LexerOffset(value + skip)
	}

	/**
	  * Tokenizes a stream of char to a stream of tokens.
	  * This process is obviously lazy, tokens will be read only when requested
	  * by the returned stream consumer.
	  *
	  * @param input the input stream of chars
	  * @return a stream of tokens from the input
	  */
	def tokenize(input: Stream[Char]): Stream[SourceToken] = readNext(input, LexerOffset(0))

	/**
	  * Reads the next tokens from the input stream.
	  *
	  * @param input  the input stream of chars
	  * @param offset the current input offset
	  * @return a stream of tokens from the input
	  */
	private def readNext(input: Stream[Char], offset: LexerOffset): Stream[SourceToken] = {
		// Make the offset implicitly available in this method
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

	/** The Number format from the JSON spec */
	private val numberFormat = """-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+\-]?[0-9]+)?""".r

	/** Checks that the given character could be part of a number literal */
	private def isValidNumberCharacter(c: Char): Boolean = {
		c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' || Character.isDigit(c)
	}

	/**
	  * Reads a number literal.
	  *
	  * For simplicity, reasons this function is a bit too eager. Input is consumed as long
	  * as the characters being read could be a part of a valid number. The actual sequence
	  * of these characters is not validated at this point.
	  *
	  * Once the first illegal character is encountered, the format regex is used to find
	  * a valid prefix in the consumed character sequence. The input offset is then
	  * incremented by the exact length of this matching prefix and characters remaining
	  * that are not part of the number literal are put back for the next token to use.
	  */
	private def readNumber(in: Stream[Char])(implicit offset: LexerOffset): Stream[SourceToken] = {
		val candidate = in.takeWhile(isValidNumberCharacter)
		numberFormat.findPrefixOf(candidate.mkString) match {
			case Some(number) => Token.Number(number.toDouble) #:: readNext(in.drop(number.length), offset + number.length)
			case None => Token.Unknown(in.head) #:: readNext(in.tail, offset + 1)
		}
	}
}
