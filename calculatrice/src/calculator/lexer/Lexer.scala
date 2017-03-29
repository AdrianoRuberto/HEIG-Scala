package calculator.lexer

import calculator.lexer.Token.SourceToken

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Lexer {
	case class LexerOffset(value: Int) extends AnyVal {
		def +(skip: Int): LexerOffset = LexerOffset(value + skip)
	}

	def tokenize(input: String): List[SourceToken] = readNext(input.toList, ListBuffer.empty, LexerOffset(0))

	@tailrec
	def readNext(input: List[Char], acc: ListBuffer[SourceToken], offset: LexerOffset): List[SourceToken] = {
		implicit val currentOffset = offset
		input match {
			case c :: cs if Character.isWhitespace(c) =>
				readNext(cs, acc, offset + 1)
			case (op @ ('+' | '-' | '*' | '/' | '%' | '^' | '!' | '=')) :: cs =>
				readNext(cs, acc += Token.Operator(op.toString), offset + 1)
			case (p @ ('(' | ')')) :: cs =>
				readNext(cs, acc += (if (p == '(') Token.LParen else Token.RParen), offset + 1)
			case ',' :: cs =>
				readNext(cs, acc += Token.Comma, offset + 1)
			case c :: cs if Character.isDigit(c) =>
				val (consumed, token, next) = readNumber(input)
				readNext(next, acc += token, offset + consumed)
			case c :: cs if Character.isJavaIdentifierStart(c) =>
				val (rest, next) = cs.span(Character.isJavaIdentifierPart)
				val identifier = (c :: rest).mkString
				readNext(next, acc += Token.Identifier(identifier), offset + identifier.length)
			case c :: cs =>
				readNext(cs, acc += Token.Unknown(c), offset + 1)
			case Nil =>
				(acc += Token.End).toList
		}
	}

	private val numberFormat = """-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+\-]?[0-9]+)?""".r

	private def isValidNumberCharacter(c: Char): Boolean = {
		c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' || Character.isDigit(c)
	}

	private def readNumber(in: List[Char]): (Int, Token, List[Char]) = {
		val candidate = in.takeWhile(isValidNumberCharacter)
		numberFormat.findPrefixOf(candidate.mkString) match {
			case Some(number) => (number.length, Token.Number(number.toDouble), in.drop(number.length))
			case None => (1, Token.Unknown(in.head), in.tail)
		}
	}
}
