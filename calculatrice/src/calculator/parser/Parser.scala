package calculator.parser

import calculator.lexer.Token._
import calculator.lexer.{Lexer, Token}
import calculator.parser.Expr._

import scala.language.implicitConversions

object Parser {
	private implicit def tokenStep[T <: Token](tok: T): Step[T] = Step.single { case t if t == tok => tok }

	def parse(input: String): Either[ParseError, Expr] = {
		val tokens = Lexer.tokenize(input)
		parseExpression(tokens) match {
			case Success(expr, Nil) =>
				Right(expr)
			case Failure(unexpected) =>
				Left(ParseError(s"Unexpected token: ${unexpected.token} at offset ${unexpected.offset}\n\n${mkWave(input, unexpected)}\n"))
			case _ =>
				Left(ParseError(s"Unknown parse error"))
		}
	}

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

	private val parseNumber: Step[Double] = Step.single { case Number(value) => value }

	private val parseIdentifier: Step[String] = Step.single { case Identifier(name) => name }

	private val parseAtom: Step[Expr] = {
		(parseNumber map Literal.apply) | (parseIdentifier map Reference.apply)
	}

	private lazy val parseArguments: Step[List[Expr]] = {
		(parseAdditive ~ (Comma ~ parseAdditive map { case comma ~ arg => arg}).*).? map {
			case None => Nil
			case Some(first ~ nexts) => first :: nexts
		}
	}

	private val parseCall: Step[Expr] = {
		Step.single { case Identifier(name) => name } ~ LParen ~ parseArguments ~ RParen map {
			case name ~ lp ~ args ~ rp => Call(name, args)
		}
	}

	private lazy val parseParenthesis: Step[Expr] = {
		LParen ~ parseAdditive ~ RParen map { case lp ~ expr ~ rp => expr }
	}

	private val parsePrimary: Step[Expr] = parseParenthesis | parseCall | parseAtom

	private val parseFactorial: Step[Expr] = {
		parsePrimary ~ Operator("!").* map {
			case first ~ facs => facs.foldLeft(first) { case (operand, fac) => Unary(fac.value, operand) }
		}
	}

	private val parsePower: Step[Expr] = binaryStep(next = parseFactorial) {
		case op @ Operator("^") => op.value
	}

	private val parseUnaryMinus: Step[Expr] = {
		(Operator("-") ~ parseUnaryMinus map { case minus ~ operand => Unary(minus.value, operand) }) | parsePower
	}

	private val parseModulo: Step[Expr] = binaryStep(next = parseUnaryMinus) {
		case op @ Operator("%") => op.value
	}

	private val parseMultiplicative: Step[Expr] = binaryStep(next = parseModulo) {
		case op @ Operator("*" | "/") => op.value
	}

	private val parseAdditive: Step[Expr] = binaryStep(next = parseMultiplicative) {
		case op @ Operator("+" | "-") => op.value
	}

	private def binaryStep(next: Step[Expr])(opMatcher: PartialFunction[Token, String]): Step[Expr] = {
		next ~ (Step.single(opMatcher) ~ next).* map {
			case first ~ ops => ops.foldLeft(first) { case (lhs, (op ~ rhs)) => Binary(op, lhs, rhs) }
		}
	}

	private val parseAssign: Step[Expr] = {
		parseIdentifier ~ Operator("=") ~ parseAdditive map { case id ~ eq ~ value => Assign(id, value) }
	}

	private val parseExpression: Step[Expr] = {
		(parseAssign | parseAdditive) ~ End map { case e ~ end => e }
	}
}

