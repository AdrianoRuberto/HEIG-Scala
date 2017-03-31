package calculator.parser

import calculator.lexer.Token._
import calculator.lexer.{Lexer, Token}
import calculator.parser.Expr._
import scala.language.implicitConversions

/**
  * Input parser.
  */
object Parser {
	/** Implicitly constructs a step parsing a single token when a token is used in place of a parse step */
	private implicit def tokenStep[T <: Token](tok: T): Step[T] = Step.single { case t if t == tok => tok }

	/** Parses text input and returns an expression tree */
	def parse(input: String): Either[ParseError, Expr] = {
		val tokens = Lexer.tokenize(input)
		parseExpression(tokens) match {
			case Success(expr, Nil) =>
				Right(expr)
			case Failure(unexpected, _) =>
				val wave = mkWave(input, unexpected)
				Left(ParseError(s"Unexpected token: ${unexpected.token} at offset ${unexpected.offset}\n\n$wave\n"))
			case _ =>
				Left(ParseError(s"Unknown parse error"))
		}
	}

	/** Parses a number literal */
	private lazy val parseNumber: Step[Double] = Step.single { case Number(value) => value }

	/** Parses a variable identifier, or a function name */
	private lazy val parseIdentifier: Step[String] = Step.single { case Identifier(name) => name }

	/** Parses an atom, that can be either a number literal or an identifier */
	private lazy val parseAtom: Step[Expr] = (parseNumber map Literal.apply) | (parseIdentifier map Reference.apply)

	/** Parses function arguments list */
	private lazy val parseArguments: Step[List[Expr]] = {
		parseAdditive ~ (Comma ~ parseAdditive.! map { case comma ~ arg => arg }).* map {
			case first ~ nexts => first :: nexts
		}
	}

	/** Parses a function call */
	private lazy val parseCall: Step[Expr] = {
		parseIdentifier ~ LParen ~ parseArguments.? ~ RParen.! map {
			case name ~ lp ~ args ~ rp => Call(name, args.getOrElse(Nil))
		}
	}

	/** Parses a parenthesised expression */
	private lazy val parseParentheses: Step[Expr] = {
		LParen ~ parseAdditive.! ~ RParen.! map {
			case lp ~ expr ~ rp => expr
		}
	}

	/** Parses a primary expression, that can be parentheses, function call or an atom */
	private lazy val parsePrimary: Step[Expr] = parseParentheses | parseCall | parseAtom

	/** Parses the factorial operator */
	private lazy val parseFactorial: Step[Expr] = {
		parsePrimary ~ Operator("!").* map {
			case first ~ facs => facs.foldLeft(first) { case (operand, fac) => Unary(fac.value, operand) }
		}
	}

	/** Parses the power operator */
	private lazy val parsePower: Step[Expr] = binaryStep(next = parseFactorial)(Operator("^"))

	/** Parses the unary minus operator */
	private lazy val parseUnaryMinus: Step[Expr] = {
		Operator("-") ~ parseUnaryMinusOrPower.! map {
			case minus ~ operand => Unary(minus.value, operand)
		}
	}

	/** Parses an unary minus operation or a power operation */
	private lazy val parseUnaryMinusOrPower: Step[Expr] = parseUnaryMinus | parsePower

	/** Parses the modulo operator */
	private lazy val parseModulo: Step[Expr] = binaryStep(next = parseUnaryMinusOrPower)(Operator("%"))

	/** Parses multiplicative operators */
	private lazy val parseMultiplicative: Step[Expr] = binaryStep(next = parseModulo)(Operator("*"), Operator("/"))

	/** Parses additive operators */
	private lazy val parseAdditive: Step[Expr] = binaryStep(next = parseMultiplicative)(Operator("+"), Operator("-"))

	/** Parses variable assignment */
	private lazy val parseAssign: Step[Expr] = {
		parseIdentifier ~ Operator("=") ~ parseAdditive.! map {
			case id ~ eq ~ value => Assign(id, value)
		}
	}

	/** Parses a top level expression */
	private lazy val parseExpression: Step[Expr] = {
		(parseAssign | parseAdditive) ~ End map { case e ~ end => e }
	}

	/** Helper for binary operator parsing (with precedence) */
	private def binaryStep(next: Step[Expr])(operator: Operator, operators: Operator*): Step[Expr] = {
		val operatorMatcher = operators.foldLeft(tokenStep[Operator](operator))(_ | _)
		next ~ (operatorMatcher ~ next.!).* map {
			case first ~ operations => operations.foldLeft(first) { case (lhs, (op ~ rhs)) => Binary(op.value, lhs, rhs) }
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

