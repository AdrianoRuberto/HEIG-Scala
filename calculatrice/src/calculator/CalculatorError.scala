package calculator

case class CalculatorError(msg: String, domain: String = null) extends Throwable {
	def fullMessage: String = if (domain != null) s"[$domain Error] $msg" else msg
}
