package calculator

/**
  * An error that occurred during execution of user input by the Calculator.
  *
  * @param msg       the error message
  * @param component the component that generated the error
  */
case class CalculatorError(msg: String, component: String = null) extends Throwable {
	/** The full error message, including the component as prefix (if given) */
	def fullMessage: String = if (component != null) s"[$component Error] $msg" else msg
}
