package calculator.parser

import calculator.lexer.Token._
import calculator.lexer.{Lexer, Token}
import calculator.parser.Expr._

import scala.language.implicitConversions

object Parser {
	/** Implicitly constructs a step parsing a single token when a token is used in place of a parse step */
	private implicit def tokenStep[T <: Token](tok: T): Step[T] = Step.single { case t if t == tok => tok }

	/** Parses text input and returns an expression tree */
	def parse(input: String): Either[ParseError, Expr] = {
		val tokens = Lexer.tokenize(input)
		parseExpression(tokens) match {
			case Success(expr, Nil) =>
				Right(expr)
			case Failure(unexpected) =>
				val wave = mkWave(input, unexpected)
				Left(ParseError(s"Unexpected token: ${unexpected.token} at offset ${unexpected.offset}\n\n$wave\n"))
			case _ =>
				Left(ParseError(s"Unknown parse error"))
		}
	}

	/** Parses a number literal */
	private val parseNumber: Step[Double] = Step.single { case Number(value) => value }

	/** Parses a variable identifier, or a function name */
	private val parseIdentifier: Step[String] = Step.single { case Identifier(name) => name }

	/** Parses an atom, that can be either a number literal or an identifier */
	private val parseAtom: Step[Expr] = (parseNumber map Literal.apply) | (parseIdentifier map Reference.apply)

	/** Parses function arguments list */
	private lazy val parseArguments: Step[List[Expr]] = {
		parseAdditive ~ (Comma ~ parseAdditive map { case comma ~ arg => arg }).* map {
			case first ~ nexts => first :: nexts
		}
	}

	/** Parses a function call */
	private val parseCall: Step[Expr] = {
		parseIdentifier ~ LParen ~ parseArguments ~ RParen map {
			case name ~ lp ~ args ~ rp => Call(name, args)
		}
	}

	/** Parses a parenthesised expression */
	private lazy val parseParentheses: Step[Expr] = {
		LParen ~ parseAdditive ~ RParen map {
			case lp ~ expr ~ rp => expr
		}
	}

	/** Parses a primary expression, that can be parentheses, function call or an atom */
	private val parsePrimary: Step[Expr] = parseParentheses | parseCall | parseAtom

	/** Parses the factorial operator */
	private val parseFactorial: Step[Expr] = {
		parsePrimary ~ Operator("!").* map {
			case first ~ facs => facs.foldLeft(first) { case (operand, fac) => Unary(fac.value, operand) }
		}
	}

	/** Parses the power operator */
	private val parsePower: Step[Expr] = {
		binaryStep(next = parseFactorial) { case op @ Operator("^") => op.value }
	}

	/** Parses the unary minus operator */
	private val parseUnaryMinus: Step[Expr] = {
		Operator("-") ~ parseUnaryMinus map {
			case minus ~ operand => Unary(minus.value, operand)
		}
	}

	/** Parses the modulo operator */
	private val parseModulo: Step[Expr] = {
		binaryStep(next = parseUnaryMinus | parsePower) { case op @ Operator("%") => op.value }
	}

	/** Parses multiplicative operators */
	private val parseMultiplicative: Step[Expr] = {
		binaryStep(next = parseModulo) { case op @ Operator("*" | "/") => op.value }
	}

	/** Parses additive operators */
	private val parseAdditive: Step[Expr] = {
		binaryStep(next = parseMultiplicative) { case op @ Operator("+" | "-") => op.value }
	}

	/** Parses variable assignment */
	private val parseAssign: Step[Expr] = {
		parseIdentifier ~ Operator("=") ~ parseAdditive map {
			case id ~ eq ~ value => Assign(id, value)
		}
	}

	/** Parses a top level expression */
	private val parseExpression: Step[Expr] = {
		(parseAssign | parseAdditive) ~ End map { case e ~ end => e }
	}

	/** Helper for binary operator parsing (with precedence) */
	private def binaryStep(next: Step[Expr])(opMatcher: PartialFunction[Token, String]): Step[Expr] = {
		next ~ (Step.single(opMatcher) ~ next).* map {
			case first ~ ops => ops.foldLeft(first) { case (lhs, (op ~ rhs)) => Binary(op, lhs, rhs) }
		}
	}

	/** Constructs the syntax error message wave */
	private def mkWave(input: String, unexpected: SourceToken): String = {
		val offset = unexpected.offset
		val dropping = (offset - 30) max 0
		val cutoff = (offset + 30) min input.length
		val truncatesLeft = dropping > 0
		val truncatesRight = cutoff < input.length
		val view = input.substring(dropping, cutoff)
		val viewIdent = " " * (if (truncatesLeft) 2 else 4)
		val leftEllipse = if (truncatesLeft) ".." else ""
		val rightEllipse = if (truncatesRight) ".." else ""
		val wave = "~" * (offset - dropping)
		val waveIndent = " " * 4
		s"$viewIdent$leftEllipse$view$rightEllipse\n$waveIndent$wave^"
	}
}

