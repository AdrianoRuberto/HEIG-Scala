package calculator

sealed trait CalculatorResult

case class CalculatorValue(value: Double) extends CalculatorResult
case class CalculatorMessage(text: String) extends CalculatorResult
