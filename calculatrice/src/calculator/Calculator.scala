package calculator

import calculator.parser.Expr._
import calculator.parser.{Expr, Parser}

import scala.util.Try

class Calculator {
	private var memory: Map[String, Double] = Map.empty
	private var ans: Double = 0.0

	def execute(input: String): CalculatorResult = Parser.parse(input.trim).map(execute) match {
		case Left(parseError) => CalculatorMessage(s"[Parse error] ${parseError.msg}")
		case Right(res @ CalculatorValue(value)) => ans = value; res
		case Right(res) => res
	}

	def execute(tree: Expr): CalculatorResult = Try {
		tree match {
			case Call("clear", _) =>
				memory = memory.empty
				ans = 0.0
				CalculatorMessage("Memory cleared")

			case Call("show", args) =>
				CalculatorMessage(args.map(_.toString).mkString("\n"))

			case Assign(name, expr) =>
				val value = eval(expr)
				memory += (name -> value)
				CalculatorMessage(s"Defined $name = $value")

			case _ =>
				CalculatorValue(eval(tree))
		}
	}.toEither.fold(
		{
			case e: CalculatorError => CalculatorMessage(e.fullMessage)
			case e: Throwable => CalculatorMessage(s"[Engine Error] Uncaught exception: ${e.getMessage}")
		},
		identity
	)

	def eval(expr: Expr): Double = expr match {
		case Literal(value) => value

		case Reference("ans") => ans
		case Reference(name) => memory.getOrElse(name, throw CalculatorError(s"Variable $name is undefined", "Memory"))

		case Unary(operator, operand) => operation(operator, eval(operand))
		case Binary(op, lhs, rhs) => operation(op, eval(lhs), eval(rhs))

		case Call("gcd", List(a, b)) => gcd(eval(a).toLong, eval(b).toLong)
		case Call("modinv", List(a, b)) => modInvert(eval(a).toLong, eval(b).toLong)
		case Call("sqrt", List(a)) => sqrt(eval(a).toLong)

		case Call(name, args) => throw CalculatorError(s"Function $name@${args.length} is undefined", "Function")
		case _ => throw CalculatorError(s"Unable to evaluate expression: $expr", "Engine")
	}

	def functionCall(f0: () => Double): Double = ???
	def functionCall(f1: (Double) => Double): Double = ???
	def functionCall(f2: (Double, Double) => Double): Double = ???

	/** Binary operations */
	def operation(op: String, lhs: Double, rhs: Double): Double = op match {
		case "+" => lhs + rhs
		case "-" => lhs - rhs
		case "/" => lhs / rhs
		case "*" => lhs * rhs
		case "%" => lhs % rhs
		case "^" => Math.pow(lhs, rhs)
	}

	/** Unary operations */
	def operation(operator: String, operand: Double): Double = operator match {
		case "!" => factorial(operand.toLong)
		case "-" => -operand
	}

	/** Computes factorial of n */
	def factorial(n: Long): Long = {
		require(n >= 0)
		def loop(n: Long, acc: Long): Long = {
			if (n <= 1) acc
			else loop(n - 1, acc * n)
		}
		loop(n, 1)
	}

	/** Greatest Common Divisor */
	def gcd(a: Long, b: Long): Long = {
		if (b == 0) a else gcd(b, a % b)
	}

	/** Square root */
	def sqrt(n: Double): Double = {
		require(n >= 0)
		val epsilon = 0.0001
		def loop(x: Double): Double = {
			if (Math.abs(x * x - n) / n < epsilon) x
			else loop((x + n / x) / 2)
		}
		loop(1)
	}

	/** Extended Euclidean algorithm implementation */
	def egcd(u: Long, v: Long): (Long, Long, Long) = {
		def loop(a: Long, b: Long,
		         x1: Long = 0, x2: Long = 1,
		         y1: Long = 1, y2: Long = 0): (Long, Long, Long) = {
			if (b == 0) (x2, y2, a)
			else {
				loop(
					a = b,
					b = a % b,
					x1 = x2 - (a / b) * x1,
					x2 = x1,
					y1 = y2 - (a / b) * y1,
					y2 = y1
				)
			}
		}
		loop(u, v)
	}

	/** Modular multiplicative inverse */
	def modInvert(u: Long, v: Long): Long = {
		val (x, _, z) = egcd(u, v)
		assert(z == 1)
		x
	}
}
