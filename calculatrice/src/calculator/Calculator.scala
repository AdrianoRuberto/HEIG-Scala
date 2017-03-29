package calculator

import calculator.parser.{Expr, Parser}

class Calculator {
	private var memory: Map[String, Double] = Map()
	private var ans: Double = 0.0

	def evaluate(input: String): CalculatorResult = Parser.parse(input).map(evaluate) match {
		case Left(parseError) => CalculatorMessage(s"[Parse error] ${parseError.msg}")
		case Right(res) => res
	}

	def evaluate(expr: Expr): CalculatorResult = expr match {
		case _ => CalculatorMessage(s"[Error] Unable to handle expression: $expr")
	}
}
