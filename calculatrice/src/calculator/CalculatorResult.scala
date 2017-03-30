package calculator

/** The result of a calculator expression execution */
sealed trait CalculatorResult

/** A valued result */
case class CalculatorValue(value: Double) extends CalculatorResult

/** A message string result */
case class CalculatorMessage(text: String) extends CalculatorResult
