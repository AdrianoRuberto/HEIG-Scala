package calculator

import calculator.parser.Expr._
import calculator.parser.{Expr, Parser}
import scala.annotation.tailrec
import scala.util.Try

/**
  * Calculator engine
  */
class Calculator {
	/**
	  * The calculator memory
	  * Each map entry correspond to a defined variable
	  */
	private var memory: Map[String, Double] = Map.empty

	/**
	  * The special `ans` variable
	  * This variable always holds the result of the last operation
	  */
	private var ans: Double = 0.0

	/**
	  * Executes the given input by parsing it and evaluating it.
	  *
	  * @param input the input expression
	  * @return the execution result
	  */
	def execute(input: String): CalculatorResult = Parser.parse(input.trim).map(execute) match {
		case Left(parseError) => CalculatorMessage(s"[Parse error] ${parseError.msg}")
		case Right(res @ CalculatorValue(value)) => ans = value; res
		case Right(res) => res
	}

	private def execute(tree: Expr): CalculatorResult = Try {
		tree match {
			case Command("clear", args) =>
				args match {
					case Reference(name) :: Nil =>
						memory -= name
						CalculatorMessage(s"Variable $name is now undefined")
					case Nil =>
						memory = memory.empty
						ans = 0.0
						CalculatorMessage("Memory cleared")
					case _ =>
						throw CalculatorError(s"Usage ':clear [variable?]'", "Command")
				}
			case Command("show", args) => CalculatorMessage(args.map(_.toString).mkString("\n"))
			case Command(name, args) => throw CalculatorError(s"Command $name/${args.length} is undefined", "Command")

			case Assign("ans", _) => throw CalculatorError("Defining `ans` is forbidden", "Memory")
			case Assign(name, expr) =>
				ans = eval(expr)
				memory += (name -> ans)
				CalculatorMessage(s"Defined $name = $ans")

			case _ => CalculatorValue(eval(tree))
		}
	}.toEither.fold(
		{
			case e: CalculatorError => CalculatorMessage(e.fullMessage)
			case e: IllegalArgumentException => CalculatorMessage(s"[Engine Error] ${e.getMessage}")
			case e: Throwable => CalculatorMessage(s"[Engine Error] Uncaught exception: ${e.getMessage}")
		},
		identity
	)

	private def eval(expr: Expr): Double = expr match {
		case Literal(value) => value

		case Reference("ans") => ans
		case Reference(name) => memory.getOrElse(name, throw CalculatorError(s"Variable $name is undefined", "Memory"))

		case Unary(operator, operand) => operation(operator, eval(operand))
		case Binary(op, lhs, rhs) => operation(op, eval(lhs), eval(rhs))

		case Call("gcd", List(a, b)) => gcd(eval(a), eval(b))
		case Call("modinv", List(a, b)) => modInvert(eval(a), eval(b))
		case Call("sqrt", List(a)) => sqrt(eval(a))

		case Call(name, args) => throw CalculatorError(s"Function $name/${args.length} is undefined", "Function")
		case _ => throw CalculatorError(s"Unable to evaluate expression: $expr", "Engine")
	}

	/** Binary operations */
	private def operation(operator: String, lhs: Double, rhs: Double): Double = operator match {
		case "+" => lhs + rhs
		case "-" => lhs - rhs
		case "/" => lhs / rhs
		case "*" => lhs * rhs
		case "%" => lhs % rhs
		case "^" => Math.pow(lhs, rhs)
	}

	/** Unary operations */
	private def operation(operator: String, operand: Double): Double = operator match {
		case "!" => factorial(operand)
		case "-" => -operand
	}

	/** Computes factorial of n */
	private def factorial(n: Double): Double = {
		require(n >= 0, "factorial of negative is undefined")
		require(n.isWhole, "factorial of non-whole number is undefined")
		@tailrec
		def loop(n: Double, acc: Double): Double = {
			if (n <= 1) acc
			else loop(n - 1, acc * n)
		}
		loop(n, 1)
	}

	/** Greatest Common Divisor */
	private def gcd(a: Double, b: Double): Double = {
		require(a.isWhole && b.isWhole, "gcd of non-whole numbers is undefined")
		@tailrec
		def loop(x: Double, y: Double): Double = {
			if (y == 0) x else loop(y, x % y)
		}
		loop(a, b)
	}

	/** Square root */
	private def sqrt(n: Double): Double = {
		require(n >= 0, "square root of negative number is not implemented")
		val epsilon = 0.0001
		@tailrec
		def loop(x: Double): Double = {
			if (Math.abs(x * x - n) / n < epsilon) x
			else loop((x + n / x) / 2)
		}
		loop(1)
	}

	/** Extended Euclidean algorithm implementation */
	private def egcd(u: Double, v: Double): (Double, Double, Double) = {
		@tailrec
		def loop(a: Double, b: Double,
		         x1: Double = 0, x2: Double = 1,
		         y1: Double = 1, y2: Double = 0): (Double, Double, Double) = {
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
	private def modInvert(u: Double, v: Double): Double = {
		require(u.isWhole && v.isWhole, "modInvert of non-whole numbers is undefined")
		val (x, _, z) = egcd(u, v)
		assert(z == 1)
		x
	}
}
