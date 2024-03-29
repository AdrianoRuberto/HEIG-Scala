package calculator.parser

/** Base trait for expression AST */
sealed trait Expr

object Expr {
	/** A number literal */
	case class Literal(value: Double) extends Expr

	/** A variable reference */
	case class Reference(name: String) extends Expr

	/** An assign operation */
	case class Assign(name: String, value: Expr) extends Expr

	/** A binary operation */
	case class Binary(operator: String, lhs: Expr, rhs: Expr) extends Expr /*{
		// Fake official names
		private def operatorName: String = operator match {
			case "+" => "Plus"
			case "-" => "Minus"
			case "/" => "Divide"
			case "*" => "Times"
			case "%" => "Mod"
			case "^" => "Pow"
			case o => o
		}
		override def toString: String = s"$operatorName($lhs,$rhs)"
	} */

	/** An unary operation */
	case class Unary(operator: String, operand: Expr) extends Expr

	/** A function call */
	case class Call(fun: String, args: List[Expr]) extends Expr

	/** A top level command */
	case class Command(cmd: String, args: List[Expr]) extends Expr
}

