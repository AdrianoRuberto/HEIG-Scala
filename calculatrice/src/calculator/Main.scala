package calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {
	val engine = new Calculator

	def main(args: Array[String]): Unit = {
		println("Welcome to the Scala calculator !")
		console()
	}

	@tailrec
	def console(): Unit = {
		print("> ")
		readLine match {
			case "quit" => println("Bye !"); return
			case "usage" => println(usage)
			case line if line.trim.nonEmpty =>
				engine.execute(line) match {
					case CalculatorValue(value) => println(s"ans = $value")
					case CalculatorMessage(msg) => println(msg)
				}
			case blank => // ignore
		}
		console()
	}

	val usage: String =
		"""- Arithmetic expression to evaluate are entered directly (eg: `2 * 4 + 5`)
		  |  - Supported operators: +, -, *, /, %, ^, !
		  |    - All operators are left-associative, usual precedence applies
		  |  - Numbers are read as double, `2.3` and `5e-4` are both valid syntax,
		  |    the syntax must match the syntax of Number from JSON
		  |  - The unary minus operator is available
		  |    - Unary minus has lower precedence than power, `-2^2` is `(-2)^2`
		  |- You can use functions (eg: `gcd(65, 22)`)
		  |  - Available functions:
		  |    - gcd(a, b): greater common divisor of a and b
		  |    - sqrt(a): square root of a (also available as `a ^ 0.5`)
		  |    - modinv(a, b): inverse of a, mod b
		  |- Variable can be defined and used in expressions
		  |  - Variable are defined by `variable = <expr>`
		  |  - You can only define variable from a top-level expression (not nested)
		  |  - Any legal identifier for a Java variable is allowed
		  |  - A special variable `ans` is always the result of the previous evaluation
		  |    - `ans` is read-only, attempting to define it is forbidden
		  |- Top level commands are prefixed by `:` (eg: `:show 2 + 2`)
		  |  - Available commands:
		  |    - :show <expr>: display parse tree for expression <expr>
		  |    - :clear: (without argument) clear the calculator memory
		  |    - :clear <var>: removes the given variable from memory
		""".stripMargin
}
